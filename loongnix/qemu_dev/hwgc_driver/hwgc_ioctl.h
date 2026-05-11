#include <linux/module.h>
#include <linux/pci.h>
#include <linux/io.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/interrupt.h>
#include <linux/wait.h>
#include <linux/sched.h>
#include <linux/uaccess.h>

enum hwgc_state
{
    HWGC_IDLE,
    HWGC_RUNNING,
    HWGC_WAIT_ATOMIC,
    HWGC_WAIT_PAGEFAULT,
    HWGC_DONE
};
struct HWGC_PARALLOCATE_PARS
{
    uint64_t alloc_region;
    uint64_t min_word_size;
    uint64_t desired_word_size;
};

// 定义 ioctl 命令
#define HWGC_IOC_MAGIC 'H'
#define HWGC_IOC_START _IOW(HWGC_IOC_MAGIC, 0, struct HWGC_PARALLOCATE_PARS)
#define HWGC_IOC_WAIT_EVENT _IOR(HWGC_IOC_MAGIC, 1, int)
#define HWGC_IOC_SOFT_PROVIDE _IOW(HWGC_IOC_MAGIC, 2, uint64_t)

#define ATOMIC_IRQ 0x0001
#define PAGE_FAULT_IRQ 0x0010
#define COMPLETE_IRQ 0x0100

#define HWGC_STATUS_COMPUTING 0x01
#define HWGC_STATUS_WAKE 0x02
#define HWGC_STATUS_IRQ 0x04
// reg define
#define REG_STATUS 0x0
#define REG_IRQ_STATUS 0x4
#define REG_START_WORK 0x8
#define REG_CONTINUE_WORK 0xc
#define REG_CLEAR_IRQ 0x10
#define REG_PAR 0x18
#define REG_IRQ_PAR0 0x30
#define REG_IRQ_PAR1 0x38
#define REG_IRQ_PAR2 0x40
#define REG_IRQ_PAR3 0x48
#define REG_IRQ_RES1 0x50
#define REG_IRQ_RES2 0x58