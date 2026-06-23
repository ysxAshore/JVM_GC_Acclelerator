#include <linux/module.h>
#include <linux/pci.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/device.h>
#include <linux/uaccess.h>
#include <linux/interrupt.h>
#include <linux/wait.h>
#include <linux/spinlock.h>
#include <linux/io.h>
#include <linux/mutex.h>
#include <linux/bitops.h>
#include <linux/version.h>

#define DRV_NAME "hwgc"
#define HWGC_VENDOR_ID 0x1234
#define HWGC_DEVICE_ID 0x0308

#define REG_STATUS 0x0
#define REG_IRQ_STATUS 0x4
#define REG_IRQ_CLEAR 0x8
#define REG_CMD 0x0c

#define REG_PAR0 0x10  // AgeThreshold ++ ChunkSize
#define REG_PAR1 0x18  // HeapRegionShiftBy ++ Bias
#define REG_PAR2 0x20  // LogOfHRGrainBytes ++ RegionAttrShiftBy
#define REG_PAR3 0x28  // CompressedFlag ++ TaskQueueBottom
#define REG_PAR4 0x30  // StepperOffset
#define REG_PAR5 0x38  // YoungWordsBase
#define REG_PAR6 0x40  // RegionAttrBase
#define REG_PAR7 0x48  // PlabAllocatorPTr
#define REG_PAR8 0x50  // RegionAttrBiasedBase
#define REG_PAR9 0x58  // HeapRegionBiasedBase
#define REG_PAR10 0x60 // ParScanThreadStatePtr
#define REG_PAR11 0x68 // TaskQueueElemsBase
#define REG_PAR12 0x70 // HumoungousReclaimBoolBase
#define REG_PAR13 0x78 // CardTablePtr
#define REG_PAR14 0x80 // G1h
#define REG_PAR15 0x88 // IntArrayKlassObj
#define REG_PAR16 0x90 // ObjectKlass
#define REG_PAR17 0x98 // LockPtr
#define REG_PAR18 0xa0 // Thread
#define REG_PAR19 0xa8 // DummyRegion
#define REG_PAR20 0xb0 // CompressedOopBase
#define REG_PAR21 0xb8 // CompressedKlassPointerBase

/*
 * IRQ define
 * BufferNode::allocate                    -> param: allocator_ptr(uintptr_t); return_value: node(uintptr_t)
 * expand_single_region                    -> param: node_index(uint)        ; return_value: success(bool)
 * ((GrowableArray<HeapRegion *> *))->grow -> param: grow_array_ptr(uintptr_t), len(int); return_value: no
 * ((Mutex *)) ->unlock                    -> param: lock_ptr(uintptr_t); return_value: no
 * TLB_MISS                                -> param: MISS_VA(uintptr_t), MISS_ACCESS(uint); return_value: TLB_FILL_VA(uintptr_t), TLB_FILL_PA(uintptr_t)
 * DONE                                    -> param: no; return_value: no
 * ERROR                                   -> param: no; return_value: no
 */
#define REG_IRQ_PAR0 0xc0
#define REG_IRQ_PAR1 0xc8
#define REG_IRQ_RES0 0xd0
#define REG_IRQ_RES1 0xd8

/* Command bits */
#define CMD_START 0x1
#define CMD_CONTINUE 0x2
#define CMD_RESET 0x4

/* Status bits */
#define ST_BUSY 0x1
#define ST_DONE 0x2
#define ST_WAIT_TLB 0x4
#define ST_WAIT_GROW 0x8
#define ST_WAIT_EXPAND 0x10
#define ST_WAIT_ALLOCATE 0x20
#define ST_WAIT_WAKE 0x40
#define ST_ERROR 0x80
#define ST_IRQ_EN 0x100

/* IRQ bits*/
#define IRQ_TLB_MISS 0x1
#define IRQ_DONE 0x2
#define IRQ_GORW 0x4
#define IRQ_EXPAND 0x8
#define IRQ_ALLOCATE 0x10
#define IRQ_WAKE 0x20
#define IRQ_ERROR 0x40

/* Device access type reported to driver */
#define ACCESS_READ 1
#define ACCESS_WRITE 2
#define MAX_PINNED 512 * 1024

#define FIELD64(lo, hi) (((u64)(lo)) | ((u64)(hi) << 32))

struct HWGCParameters
{
    __u32 chunkSize;
    __u32 ageThreshold;
    __u32 heapRegionBias;
    __u32 heapRegionShiftBy;
    __u32 regionAttrShiftBy;
    __u32 logOfHRGrainBytes;

    __u64 stepperOffset;
    __u64 youngWordsBase;
    __u64 regionAttrBase;
    __u64 plabAllocatorPtr;
    __u64 regionAttrBiasedBase;
    __u64 heapRegionBiasedBase;
    __u64 pss;

    __u32 localBot;

    __u64 taskQueueElemsBase;
    __u64 humogousReclaimCandidateBoolBase;
    __u64 cardTablePtr;
    __u64 g1h;
    __u64 intArrayKlassObj;
    __u64 objectKlass;
    __u64 lockPtr;
    __u64 thread;
    __u64 dummyRegion;
    __u64 compressedOopBase;
    __u64 compressedKlassPointerBase;

    __u8 compressedOopShift;
    __u8 compressedKlassPointerShift;
    __u8 useCompressedOops;
    __u8 useCompressedKlassPointers;
};

#define HWGC_EVENT_NONE 0
#define HWGC_EVENT_ALLOCATE 1
#define HWGC_EVENT_WAKE 2

struct HWGCEvent
{
    __u32 type;
    __u32 reserved;

    __u64 seq;

    __u64 par0;
    __u64 par1;

    __u64 res0;
    __u64 res1;
};

#define XOR_TLB_IOC_MAGIC 'x'
#define HWGC_IOC_RUN _IOWR(HWGC_IOC_MAGIC, 1, struct HWGCParameters)
#define HWGC_IOC_WAIT_EVENT _IOR(HWGC_IOC_MAGIC, 2, struct HWGCEvent)
#define HWGC_IOC_REPLY_EVENT _IOW(HWGC_IOC_MAGIC, 3, struct HWGCEvent)

struct pinned_page
{
    struct page *page; // get_user_pages_remote() 的return value
    bool write;        // 是否被修改过, 若有则在释放前需要set_page_dirty_lock()
};

struct hwgc
{
    struct pci_dev *pdev;
    void __iomem *bar0;
    int irq;
    struct miscdevice miscdev; // 字符设备
    struct mutex op_lock;      // 保证同一时刻只有一个ioctl操作在使用设备
    struct completion done;    // ioctl线程sleep在completion上 等待IRQ DONE/ERROR 唤醒

    struct task_struct *active_task;
    struct mm_struct *active_mm;
    bool op_active; // 有设备操作正在进行
    int op_status;
    spinlock_t irq_lock;
    u32 pending_irq;

    // pin 住的 page
    struct pinned_page pinned[MAX_PINNED];
    int nr_pinned;

    /*
     * 软件事件通道：
     * IRQ_ALLOCATE / IRQ_WAKE 到来后，IRQ thread 把事件放到这里，
     * 用户态通过 HWGC_IOC_WAIT_EVENT 取走，
     * 处理完后通过 HWGC_IOC_REPLY_EVENT 写回结果。
     */
    spinlock_t event_lock;
    wait_queue_head_t event_wq;

    struct HWGCEvent current_event;
    bool event_pending;
    bool event_in_service;
    u64 event_seq;

    bool removing;
};

static inline void reg_write32(struct hwgc *d, u32 val, u32 off)
{
    iowrite32(val, d->bar0 + off);
}

static inline u32 reg_read32(struct hwgc *d, u32 off)
{
    return ioread32(d->bar0 + off);
}

static inline void reg_write64(struct hwgc *d, u64 val, u32 off)
{
#if BITS_PER_LONG == 64
    writeq(val, d->bar0 + off);
#else
    iowrite32(lower_32_bits(val), d->bar0 + off);
    iowrite32(upper_32_bits(val), d->bar0 + off + 4);
#endif
}

static inline u64 reg_read64(struct hwgc *d, u32 off)
{
#if BITS_PER_LONG == 64
    return readq(d->bar0 + off);
#else
    u32 lo;
    u32 hi;

    lo = ioread32(d->bar0 + off);
    hi = ioread32(d->bar0 + off + 4);

    return ((u64)hi << 32) | lo;
#endif
}