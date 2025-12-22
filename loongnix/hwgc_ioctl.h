#include <linux/uaccess.h>
struct HWGCParameter
{
    u32 chunkSize;
    u32 ageThreshold;
    u32 heapRegionBias;
    u32 regionAttrShiftBy;
    u32 heapRegionShiftBy;
    u32 logOfHRGrainBytes;
    u64 stepperOffset;
    u64 youngWordsBase;
    u64 regionAttrBase;
    u64 plabAllocatorPtr;
    u64 regionAttrBiasedBase;
    u64 heapRegionBiasedBase;
    u64 parScanThreadStatePtr;
    u64 taskQueueBottomAddr;
    u64 taskQueueAgeTopAddr;
    u64 taskQueueElemsBase;
    u64 humogousReclaimCandidateBoolBase;
};

enum hwgc_state
{
    HWGC_IDLE,
    HWGC_RUNNING,
    HWGC_WAIT_MALLOC,
    HWGC_WAIT_ENQUEUED,
    HWGC_DONE
};

// 定义 ioctl 命令
#define HWGC_IOC_MAGIC 'H'
#define HWGC_IOC_START _IOW(HWGC_IOC_MAGIC, 0, struct HWGCParameter)
#define HWGC_IOC_WAIT_EVENT _IOR(HWGC_IOC_MAGIC, 1, int)
#define HWGC_IOC_SOFT_PROVIDE _IOW(HWGC_IOC_MAGIC, 2, uint64_t)

// reg define
#define REG_DEVICE_ID 0x0
#define REG_STATUS 0x4
#define REG_INT_STATUS 0x8
#define REG_CLEAR_IRQ 0xc
#define REG_PAR 0x10
#define REG_START_WORK 0x80
#define REG_CONTINUE_WORK 0x84
#define REG_SOFT_RES 0x88
#define REG_SOFT_PAR0 0x90
#define REG_SOFT_PAR1 0x98
#define REG_SOFT_PAR2 0xa0
#define REG_SOFT_PAR3 0xa8

// get array size
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

#define FIELD64(lo, hi) (((u64)(lo)) | ((u64)(hi) << 32))