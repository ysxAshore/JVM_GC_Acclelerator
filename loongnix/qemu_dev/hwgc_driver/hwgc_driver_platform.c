#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/platform_device.h>
#include <linux/io.h>
#include <linux/interrupt.h>
#include <linux/miscdevice.h>
#include <linux/fs.h>
#include <linux/uaccess.h>
#include <linux/slab.h>
#include <linux/idr.h>
#include <linux/spinlock.h>
#include <linux/mutex.h>
#include <linux/wait.h>
#include <linux/mm.h>
#include <linux/sched.h>
#include <linux/sched/mm.h>
#include <linux/sched/task.h>
#include <linux/highmem.h>
#include <linux/acpi.h>
#include <linux/irq.h>

#ifndef DRV_NAME
#define DRV_NAME "hwgc-platform"
#endif

#ifndef MAX_PINNED
#define MAX_PINNED 512 * 1024
#endif

#define HWGC_DEV_NAME_LEN 32

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
#define IRQ_ALL (IRQ_ERROR | IRQ_WAKE | IRQ_ALLOCATE | IRQ_EXPAND | IRQ_GORW | IRQ_DONE | IRQ_TLB_MISS)

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
#define HWGC_EVENT_DONE 1
#define HWGC_EVENT_GROW 2
#define HWGC_EVENT_EXPAND 3
#define HWGC_EVENT_ALLOCATE 4
#define HWGC_EVENT_WAKE 5
#define HWGC_EVENT_ERROR 6

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

#define HWGC_IOC_MAGIC 'x'
#define HWGC_IOC_START _IOWR(HWGC_IOC_MAGIC, 1, struct HWGCParameters)
#define HWGC_IOC_WAIT_EVENT _IOR(HWGC_IOC_MAGIC, 2, struct HWGCEvent)
#define HWGC_IOC_REPLY_EVENT _IOW(HWGC_IOC_MAGIC, 3, struct HWGCEvent)

struct hwgc_pinned_page
{
    struct page *page;
    bool write;
};

struct hwgc
{
    struct device *dev;

    void __iomem *bar0;
    int irq;

    spinlock_t irq_lock;
    u32 pending_irq;

    spinlock_t event_lock;
    wait_queue_head_t event_wq;

    struct HWGCEvent current_event;
    bool event_pending;
    bool event_in_service;
    u64 event_seq;

    struct mutex op_lock;
    bool op_active;
    int op_status;

    bool removing;

    struct task_struct *active_task;
    struct mm_struct *active_mm;

    struct hwgc_pinned_page pinned[MAX_PINNED];
    int nr_pinned;

    struct miscdevice miscdev;
    int dev_id;
    char dev_name[32];
};

static DEFINE_IDA(hwgc_ida);

#define HWGC_MAX_DEVS 2

static unsigned long mmio_base[HWGC_MAX_DEVS];
static int num_devs;
static unsigned int mmio_size = 0x1000;
static int irq = -1;

module_param_array(mmio_base, ulong, &num_devs, 0444);
module_param(mmio_size, uint, 0444);
module_param(irq, int, 0444);

MODULE_PARM_DESC(mmio_base, "HWGC MMIO base array");
MODULE_PARM_DESC(mmio_size, "HWGC MMIO size");
MODULE_PARM_DESC(irq, "Shared HWGC IRQ number");

static struct platform_device *hwgc_pdevs[HWGC_MAX_DEVS];

static inline u32 reg_read32(struct hwgc *d, u32 off)
{
    return readl((u8 __iomem *)d->bar0 + off);
}

static inline void reg_write32(struct hwgc *d, u32 val, u32 off)
{
    writel(val, (u8 __iomem *)d->bar0 + off);
}

static inline u64 reg_read64(struct hwgc *d, u32 off)
{
    return readq((u8 __iomem *)d->bar0 + off);
}

static inline void reg_write64(struct hwgc *d, u64 val, u32 off)
{
    writeq(val, (u8 __iomem *)d->bar0 + off);
}

static void hwgc_program_parameters(struct hwgc *d, const struct HWGCParameters *p)
{
    reg_write64(d, FIELD64(p->chunkSize, p->ageThreshold), REG_PAR0);
    reg_write64(d, FIELD64(p->heapRegionBias, p->heapRegionShiftBy), REG_PAR1);
    reg_write64(d, FIELD64(p->regionAttrShiftBy, p->logOfHRGrainBytes), REG_PAR2);
    reg_write64(d, FIELD64(p->localBot, (((u32)(p->compressedKlassPointerShift) << 24) | ((u32)(p->useCompressedKlassPointers) << 16) | ((u32)(p->compressedOopShift) << 8) | ((u32)(p->useCompressedOops)))), REG_PAR3);
    reg_write64(d, p->stepperOffset, REG_PAR4);
    reg_write64(d, p->youngWordsBase, REG_PAR5);
    reg_write64(d, p->regionAttrBase, REG_PAR6);
    reg_write64(d, p->plabAllocatorPtr, REG_PAR7);
    reg_write64(d, p->regionAttrBiasedBase, REG_PAR8);
    reg_write64(d, p->heapRegionBiasedBase, REG_PAR9);
    reg_write64(d, p->pss, REG_PAR10);
    reg_write64(d, p->taskQueueElemsBase, REG_PAR11);
    reg_write64(d, p->humogousReclaimCandidateBoolBase, REG_PAR12);
    reg_write64(d, p->cardTablePtr, REG_PAR13);
    reg_write64(d, p->g1h, REG_PAR14);
    reg_write64(d, p->intArrayKlassObj, REG_PAR15);
    reg_write64(d, p->objectKlass, REG_PAR16);
    reg_write64(d, p->lockPtr, REG_PAR17);
    reg_write64(d, p->thread, REG_PAR18);
    reg_write64(d, p->dummyRegion, REG_PAR19);
    reg_write64(d, p->compressedOopBase, REG_PAR20);
    reg_write64(d, p->compressedKlassPointerBase, REG_PAR21);
}

static bool hwgc_event_needs_reply(u32 type)
{
    switch (type)
    {
    case HWGC_EVENT_GROW:
    case HWGC_EVENT_EXPAND:
    case HWGC_EVENT_ALLOCATE:
    case HWGC_EVENT_WAKE:
        return true;

    default:
        return false;
    }
}

static bool hwgc_event_is_terminal(u32 type)
{
    switch (type)
    {
    case HWGC_EVENT_DONE:
    case HWGC_EVENT_ERROR:
        return true;

    default:
        return false;
    }
}

static void hwgc_clear_event_state_locked(struct hwgc *d)
{
    memset(&d->current_event, 0, sizeof(d->current_event));
    d->event_pending = false;
    d->event_in_service = false;
}

static void hwgc_unpin_all(struct hwgc *d)
{
    struct page *page;
    int i;

    for (i = 0; i < d->nr_pinned; i++)
    {
        page = d->pinned[i].page;
        if (!page)
            continue;

        if (d->pinned[i].write)
            set_page_dirty_lock(page);

        put_page(page);

        d->pinned[i].page = NULL;
        d->pinned[i].write = false;
    }

    d->nr_pinned = 0;
}

static int hwgc_remember_pinned(struct hwgc *d, struct page *page, bool write)
{
    int i;

    for (i = 0; i < d->nr_pinned; i++)
    {
        if (d->pinned[i].page == page)
        {
            d->pinned[i].write |= write;
            put_page(page);
            return 0;
        }
    }

    if (d->nr_pinned >= MAX_PINNED)
        return -ENOSPC;

    d->pinned[d->nr_pinned].page = page;
    d->pinned[d->nr_pinned].write = write;
    d->nr_pinned++;

    return 0;
}

static void hwgc_release_process_context(struct hwgc *d)
{
    if (d->active_mm)
    {
        mmput(d->active_mm);
        d->active_mm = NULL;
    }

    if (d->active_task)
    {
        put_task_struct(d->active_task);
        d->active_task = NULL;
    }
}

static int hwgc_fill_tlb_for_miss(struct hwgc *d)
{
    struct task_struct *task;
    struct mm_struct *mm;
    struct page *page;
    unsigned long miss_va;
    unsigned long page_va;
    unsigned int gup_flags;
    phys_addr_t pa_page;
    u32 access;
    bool write;
    long ret;
    int locked;

    task = d->active_task;
    mm = d->active_mm;

    if (!task || !mm)
        return -EINVAL;

    miss_va = (unsigned long)reg_read64(d, REG_IRQ_PAR0);
    access = reg_read32(d, REG_IRQ_PAR1);

    if (access != ACCESS_READ && access != ACCESS_WRITE)
        return -EINVAL;

    write = access == ACCESS_WRITE;
    page_va = miss_va & PAGE_MASK;

    gup_flags = 0;
    if (write)
        gup_flags |= FOLL_WRITE;

    page = NULL;
    locked = 1;

    down_read(&mm->mmap_sem);

    ret = get_user_pages_remote(task, mm, page_va, 1,
                                gup_flags, &page, NULL, &locked);

    if (locked)
        up_read(&mm->mmap_sem);

    if (ret != 1)
    {
        if (ret < 0)
            return (int)ret;

        return -EFAULT;
    }

    if (!page)
        return -EFAULT;

    ret = hwgc_remember_pinned(d, page, write);
    if (ret)
    {
        put_page(page);
        return (int)ret;
    }

    pa_page = page_to_phys(page) & PAGE_MASK;

    reg_write64(d, (u64)page_va, REG_IRQ_RES0);
    reg_write64(d, (u64)pa_page, REG_IRQ_RES1);

    reg_write32(d, CMD_CONTINUE, REG_CMD);

    return 0;
}

static int hwgc_publish_sw_event(struct hwgc *d, u32 type, u64 par0, u64 par1)
{
    unsigned long flags;
    struct HWGCEvent ev;

    memset(&ev, 0, sizeof(ev));
    ev.type = type;
    ev.par0 = par0;
    ev.par1 = par1;

    spin_lock_irqsave(&d->event_lock, flags);

    if (d->removing)
    {
        spin_unlock_irqrestore(&d->event_lock, flags);
        return -ENODEV;
    }

    if (d->event_pending || d->event_in_service)
    {
        spin_unlock_irqrestore(&d->event_lock, flags);
        return -EBUSY;
    }

    ev.seq = ++d->event_seq;
    d->current_event = ev;
    d->event_pending = true;

    spin_unlock_irqrestore(&d->event_lock, flags);

    wake_up_interruptible(&d->event_wq);

    return 0;
}

static void hwgc_finish_operation(struct hwgc *d)
{
    unsigned long flags;

    mutex_lock(&d->op_lock);

    if (!READ_ONCE(d->op_active))
    {
        mutex_unlock(&d->op_lock);
        return;
    }

    synchronize_irq(d->irq);

    hwgc_unpin_all(d);
    hwgc_release_process_context(d);

    spin_lock_irqsave(&d->event_lock, flags);
    hwgc_clear_event_state_locked(d);
    spin_unlock_irqrestore(&d->event_lock, flags);

    WRITE_ONCE(d->op_active, false);

    mutex_unlock(&d->op_lock);

    wake_up_interruptible_all(&d->event_wq);
}

static void hwgc_abort_operation_from_irq(struct hwgc *d, int status)
{
    int ret;

    d->op_status = status;
    reg_write32(d, CMD_RESET, REG_CMD);

    ret = hwgc_publish_sw_event(d, HWGC_EVENT_ERROR, (u64)(s64)status, 0);
    if (ret)
        dev_err(d->dev, "failed to publish ERROR event: %d\n", ret);
}

static irqreturn_t hwgc_irq_top(int irq_num, void *data)
{
    struct hwgc *d;
    unsigned long flags;
    u32 st;

    (void)irq_num;

    d = data;
    st = reg_read32(d, REG_IRQ_STATUS);

    if (!st)
        return IRQ_NONE;

    reg_write32(d, st, REG_IRQ_CLEAR);

    spin_lock_irqsave(&d->irq_lock, flags);
    d->pending_irq |= st;
    spin_unlock_irqrestore(&d->irq_lock, flags);

    return IRQ_WAKE_THREAD;
}

static irqreturn_t hwgc_irq_thread(int irq_num, void *data)
{
    struct hwgc *d;
    unsigned long flags;
    u32 pending;
    int ret;

    (void)irq_num;

    d = data;

    spin_lock_irqsave(&d->irq_lock, flags);
    pending = d->pending_irq;
    d->pending_irq = 0;
    spin_unlock_irqrestore(&d->irq_lock, flags);

    if (pending & IRQ_TLB_MISS)
    {
        dev_info(d->dev, "IRQ_TLB_MISS\n");

        ret = hwgc_fill_tlb_for_miss(d);
        if (ret)
        {
            dev_err(d->dev, "TLB miss handling failed: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_DONE)
    {
        dev_info(d->dev, "IRQ_DONE\n");

        d->op_status = 0;
        ret = hwgc_publish_sw_event(d, HWGC_EVENT_DONE, 0, 0);
        if (ret)
        {
            dev_err(d->dev, "failed to publish DONE: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_GORW)
    {
        dev_info(d->dev, "IRQ_GROW\n");

        ret = hwgc_publish_sw_event(d,
                                    HWGC_EVENT_GROW,
                                    reg_read64(d, REG_IRQ_PAR0),
                                    reg_read64(d, REG_IRQ_PAR1));
        if (ret)
        {
            dev_err(d->dev, "failed to publish GROW: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_EXPAND)
    {
        dev_info(d->dev, "IRQ_EXPAND\n");

        ret = hwgc_publish_sw_event(d,
                                    HWGC_EVENT_EXPAND,
                                    reg_read64(d, REG_IRQ_PAR0),
                                    reg_read64(d, REG_IRQ_PAR1));
        if (ret)
        {
            dev_err(d->dev, "failed to publish EXPAND: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_ALLOCATE)
    {
        dev_info(d->dev, "IRQ_ALLOCATE\n");

        ret = hwgc_publish_sw_event(d,
                                    HWGC_EVENT_ALLOCATE,
                                    reg_read64(d, REG_IRQ_PAR0),
                                    reg_read64(d, REG_IRQ_PAR1));
        if (ret)
        {
            dev_err(d->dev, "failed to publish ALLOCATE: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_WAKE)
    {
        dev_info(d->dev, "IRQ_WAKE\n");

        ret = hwgc_publish_sw_event(d,
                                    HWGC_EVENT_WAKE,
                                    reg_read64(d, REG_IRQ_PAR0),
                                    reg_read64(d, REG_IRQ_PAR1));
        if (ret)
        {
            dev_err(d->dev, "failed to publish WAKE: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_ERROR)
    {
        dev_info(d->dev, "IRQ_ERROR\n");
        hwgc_abort_operation_from_irq(d, -EIO);
        return IRQ_HANDLED;
    }

    return IRQ_HANDLED;
}

static long hwgc_start_ioctl(struct hwgc *d, unsigned long arg)
{
    struct HWGCParameters params;
    unsigned long flags;
    int ret;

    if (copy_from_user(&params, (void __user *)arg, sizeof(params)))
        return -EFAULT;

    mutex_lock(&d->op_lock);

    ret = 0;

    if (d->removing)
    {
        ret = -ENODEV;
        goto out_unlock;
    }

    if (READ_ONCE(d->op_active))
    {
        ret = -EBUSY;
        goto out_unlock;
    }

    d->active_task = current;
    get_task_struct(d->active_task);

    d->active_mm = get_task_mm(current);
    if (!d->active_mm)
    {
        put_task_struct(d->active_task);
        d->active_task = NULL;
        ret = -EINVAL;
        goto out_unlock;
    }

    d->op_status = 0;
    d->nr_pinned = 0;

    spin_lock_irqsave(&d->irq_lock, flags);
    d->pending_irq = 0;
    spin_unlock_irqrestore(&d->irq_lock, flags);

    spin_lock_irqsave(&d->event_lock, flags);
    hwgc_clear_event_state_locked(d);
    spin_unlock_irqrestore(&d->event_lock, flags);

    WRITE_ONCE(d->op_active, true);

    reg_write32(d, CMD_RESET, REG_CMD);
    reg_write32(d, IRQ_ALL, REG_IRQ_CLEAR);
    reg_write32(d, ST_IRQ_EN, REG_STATUS);

    hwgc_program_parameters(d, &params);

    reg_write32(d, CMD_START, REG_CMD);

out_unlock:
    mutex_unlock(&d->op_lock);
    return ret;
}

static long hwgc_wait_event_ioctl(struct hwgc *d, unsigned long arg)
{
    struct HWGCEvent ev;
    unsigned long flags;
    bool terminal;
    int ret;

    ret = wait_event_interruptible(d->event_wq,
                                   d->event_pending ||
                                       d->removing ||
                                       !READ_ONCE(d->op_active));
    if (ret)
        return ret;

    spin_lock_irqsave(&d->event_lock, flags);

    if (d->removing)
    {
        spin_unlock_irqrestore(&d->event_lock, flags);
        return -ENODEV;
    }

    if (!d->event_pending)
    {
        spin_unlock_irqrestore(&d->event_lock, flags);
        return -EAGAIN;
    }

    ev = d->current_event;
    terminal = hwgc_event_is_terminal(ev.type);

    d->event_pending = false;
    d->event_in_service = hwgc_event_needs_reply(ev.type);

    spin_unlock_irqrestore(&d->event_lock, flags);

    if (copy_to_user((void __user *)arg, &ev, sizeof(ev)))
    {
        spin_lock_irqsave(&d->event_lock, flags);

        if (!d->removing)
        {
            d->current_event = ev;
            d->event_pending = true;
            d->event_in_service = false;
        }

        spin_unlock_irqrestore(&d->event_lock, flags);

        wake_up_interruptible(&d->event_wq);
        return -EFAULT;
    }

    if (terminal)
        hwgc_finish_operation(d);

    return 0;
}

static long hwgc_reply_event_ioctl(struct hwgc *d, unsigned long arg)
{
    struct HWGCEvent ev;
    unsigned long flags;
    int ret = 0;

    if (copy_from_user(&ev, (void __user *)arg, sizeof(ev)))
        return -EFAULT;

    if (!hwgc_event_needs_reply(ev.type))
        return -EINVAL;

    spin_lock_irqsave(&d->event_lock, flags);

    if (d->removing)
    {
        ret = -ENODEV;
        goto out_unlock;
    }

    if (!d->event_in_service)
    {
        ret = -EINVAL;
        goto out_unlock;
    }

    if (ev.seq != d->current_event.seq || ev.type != d->current_event.type)
    {
        ret = -EINVAL;
        goto out_unlock;
    }

    d->event_in_service = false;
    memset(&d->current_event, 0, sizeof(d->current_event));

out_unlock:
    spin_unlock_irqrestore(&d->event_lock, flags);

    if (ret)
        return ret;

    reg_write64(d, ev.res0, REG_IRQ_RES0);
    reg_write64(d, ev.res1, REG_IRQ_RES1);

    reg_write32(d, CMD_CONTINUE, REG_CMD);

    return 0;
}

static long hwgc_ioctl(struct file *filp, unsigned int cmd, unsigned long arg)
{
    struct hwgc *d = filp->private_data;

    if (!d)
        return -ENODEV;

    switch (cmd)
    {
    case HWGC_IOC_START:
        return hwgc_start_ioctl(d, arg);

    case HWGC_IOC_WAIT_EVENT:
        return hwgc_wait_event_ioctl(d, arg);

    case HWGC_IOC_REPLY_EVENT:
        return hwgc_reply_event_ioctl(d, arg);

    default:
        return -ENOTTY;
    }
}

static int hwgc_open(struct inode *inode, struct file *filp)
{
    struct miscdevice *mdev = filp->private_data;
    struct hwgc *d;

    d = container_of(mdev, struct hwgc, miscdev);
    filp->private_data = d;

    return 0;
}

static const struct file_operations hwgc_fops = {
    .owner = THIS_MODULE,
    .open = hwgc_open,
    .unlocked_ioctl = hwgc_ioctl,
#ifdef CONFIG_COMPAT
    .compat_ioctl = hwgc_ioctl,
#endif
    .llseek = no_llseek,
};

static int hwgc_platform_probe(struct platform_device *pdev)
{
    struct hwgc *d;
    struct resource *res;
    int ret;

    d = kzalloc(sizeof(*d), GFP_KERNEL);
    if (!d)
        return -ENOMEM;

    d->dev = &pdev->dev;
    platform_set_drvdata(pdev, d);

    res = platform_get_resource(pdev, IORESOURCE_MEM, 0);
    if (!res)
    {
        dev_err(&pdev->dev, "no MMIO resource\n");
        ret = -ENODEV;
        goto err_free;
    }

    d->bar0 = devm_ioremap_resource(&pdev->dev, res);
    if (IS_ERR(d->bar0))
    {
        ret = PTR_ERR(d->bar0);
        d->bar0 = NULL;
        goto err_free;
    }

    d->irq = acpi_register_gsi(NULL,
                               irq,
                               ACPI_LEVEL_SENSITIVE,
                               ACPI_ACTIVE_HIGH);
    if (d->irq < 0)
    {
        ret = d->irq;
        dev_err(&pdev->dev, "no IRQ resource: %d\n", ret);
        goto err_free;
    }

    spin_lock_init(&d->irq_lock);
    spin_lock_init(&d->event_lock);
    mutex_init(&d->op_lock);
    init_waitqueue_head(&d->event_wq);

    d->pending_irq = 0;
    d->event_pending = false;
    d->event_in_service = false;
    d->event_seq = 0;
    d->removing = false;
    d->op_status = 0;
    d->nr_pinned = 0;
    d->active_task = NULL;
    d->active_mm = NULL;
    WRITE_ONCE(d->op_active, false);

    ret = request_threaded_irq(d->irq,
                               hwgc_irq_top,
                               hwgc_irq_thread,
                               IRQF_ONESHOT | IRQF_SHARED,
                               dev_name(&pdev->dev),
                               d);
    if (ret)
    {
        dev_err(&pdev->dev,
                "request_threaded_irq irq=%d failed: %d\n",
                d->irq, ret);
        goto err_free;
    }

    ret = ida_simple_get(&hwgc_ida, 0, 0, GFP_KERNEL);
    if (ret < 0)
        goto err_free_irq;

    d->dev_id = ret;
    snprintf(d->dev_name, sizeof(d->dev_name), "hwgc%d", d->dev_id);

    d->miscdev.minor = MISC_DYNAMIC_MINOR;
    d->miscdev.name = d->dev_name;
    d->miscdev.fops = &hwgc_fops;
    d->miscdev.parent = &pdev->dev;

    ret = misc_register(&d->miscdev);
    if (ret)
        goto err_free_id;

    dev_info(&pdev->dev,
             "hwgc platform driver loaded: /dev/%s resource=%pR irq=%d\n",
             d->miscdev.name, res, d->irq);

    dev_info(&pdev->dev, "initial irq_status=0x%x\n",
             reg_read32(d, REG_IRQ_STATUS));

    return 0;

err_free_id:
    ida_simple_remove(&hwgc_ida, d->dev_id);
    d->dev_id = -1;

err_free_irq:
    free_irq(d->irq, d);

err_free:
    platform_set_drvdata(pdev, NULL);
    kfree(d);
    return ret;
}

static int hwgc_platform_remove(struct platform_device *pdev)
{
    struct hwgc *d;
    unsigned long flags;

    d = platform_get_drvdata(pdev);
    if (!d)
        return 0;

    spin_lock_irqsave(&d->event_lock, flags);
    d->removing = true;
    hwgc_clear_event_state_locked(d);
    spin_unlock_irqrestore(&d->event_lock, flags);

    wake_up_interruptible_all(&d->event_wq);

    misc_deregister(&d->miscdev);

    if (d->dev_id >= 0)
    {
        ida_simple_remove(&hwgc_ida, d->dev_id);
        d->dev_id = -1;
    }

    mutex_lock(&d->op_lock);

    if (d->bar0)
        reg_write32(d, CMD_RESET, REG_CMD);

    synchronize_irq(d->irq);

    hwgc_unpin_all(d);
    hwgc_release_process_context(d);
    WRITE_ONCE(d->op_active, false);

    mutex_unlock(&d->op_lock);

    free_irq(d->irq, d);

    platform_set_drvdata(pdev, NULL);
    kfree(d);

    dev_info(&pdev->dev, "hwgc platform driver removed\n");

    return 0;
}

static struct platform_driver hwgc_platform_driver = {
    .probe = hwgc_platform_probe,
    .remove = hwgc_platform_remove,
    .driver = {
        .name = DRV_NAME,
    },
};

static int __init hwgc_module_init(void)
{
    struct resource res[2];
    int i;
    int ret;

    if (num_devs <= 0 || num_devs > HWGC_MAX_DEVS)
    {
        pr_err("hwgc: invalid num_devs=%d\n", num_devs);
        return -EINVAL;
    }

    if (irq < 0)
    {
        pr_err("hwgc: irq is required\n");
        return -EINVAL;
    }

    ret = platform_driver_register(&hwgc_platform_driver);
    if (ret)
    {
        pr_err("hwgc: platform_driver_register failed: %d\n", ret);
        return ret;
    }

    for (i = 0; i < num_devs; i++)
    {
        memset(res, 0, sizeof(res));

        res[0].start = mmio_base[i];
        res[0].end = mmio_base[i] + mmio_size - 1;
        res[0].flags = IORESOURCE_MEM;
        res[0].name = "hwgc-mmio";

        res[1].start = irq;
        res[1].end = irq;
        res[1].flags = IORESOURCE_IRQ;
        res[1].name = "hwgc-irq";

        pr_info("hwgc: register pdev%d mmio=0x%lx size=0x%x irq=%d\n",
                i, mmio_base[i], mmio_size, irq);

        hwgc_pdevs[i] = platform_device_register_simple(DRV_NAME,
                                                        i,
                                                        res,
                                                        ARRAY_SIZE(res));
        if (IS_ERR(hwgc_pdevs[i]))
        {
            ret = PTR_ERR(hwgc_pdevs[i]);
            hwgc_pdevs[i] = NULL;
            goto err_unregister_devices;
        }
    }

    return 0;

err_unregister_devices:
    while (--i >= 0)
    {
        if (hwgc_pdevs[i])
        {
            platform_device_unregister(hwgc_pdevs[i]);
            hwgc_pdevs[i] = NULL;
        }
    }

    platform_driver_unregister(&hwgc_platform_driver);
    return ret;
}

static void __exit hwgc_module_exit(void)
{
    int i;

    for (i = 0; i < HWGC_MAX_DEVS; i++)
    {
        if (hwgc_pdevs[i])
        {
            platform_device_unregister(hwgc_pdevs[i]);
            hwgc_pdevs[i] = NULL;
        }
    }

    platform_driver_unregister(&hwgc_platform_driver);
    ida_destroy(&hwgc_ida);

    pr_info("hwgc: module unloaded\n");
}

module_init(hwgc_module_init);
module_exit(hwgc_module_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("sxyang");
MODULE_DESCRIPTION("QEMU HWGC platform driver with Linux 4.19 GUP compatibility");