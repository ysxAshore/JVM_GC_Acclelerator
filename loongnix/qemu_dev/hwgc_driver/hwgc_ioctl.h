enum hwgc_state
{
    HWGC_IDLE,
    HWGC_RUNNING,
    HWGC_WAIT_MALLOC,
    HWGC_WAIT_ENQUEUED,
    HWGC_WAIT_PAGEFAULT,
    HWGC_DEBUG,
    HWGC_DONE
};

// 定义 ioctl 命令
#define HWGC_IOC_MAGIC 'H'
#define HWGC_IOC_START _IOW(HWGC_IOC_MAGIC, 0, struct HWGCParameter)
#define HWGC_IOC_WAIT_EVENT _IOR(HWGC_IOC_MAGIC, 1, int)
#define HWGC_IOC_SOFT_PROVIDE _IOW(HWGC_IOC_MAGIC, 2, uint64_t)
#define HWGC_IOC_DEBUG_WRITE _IOW(HWGC_IOC_MAGIC, 3, uint64_t)

// reg define
#define REG_DEVICE_ID 0x0
#define REG_STATUS 0x4
#define REG_INT_STATUS 0x8
#define REG_CLEAR_IRQ 0xc
#define REG_PAR 0x10
#define REG_START_WORK 0xd0
#define REG_CONTINUE_WORK 0xd4
#define REG_SOFT_RES 0xd8
#define REG_SOFT_PAR0 0xe0
#define REG_SOFT_PAR1 0xe8
#define REG_SOFT_PAR2 0xf0
#define REG_SOFT_PAR3 0xf8

// get array size

#define FIELD64(lo, hi) (((u64)(lo)) | ((u64)(hi) << 32))