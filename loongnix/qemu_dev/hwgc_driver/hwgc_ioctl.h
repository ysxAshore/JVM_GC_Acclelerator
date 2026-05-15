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

enum hwgc_state
{
    HWGC_IDLE,
    HWGC_RUNNING,
    HWGC_WAIT_ALLOCATE,
    HWGC_WAIT_ATTEMPT,
    HWGC_WAIT_LOCK_WAKE,
    HWGC_WAIT_PAGEFAULT,
    HWGC_WAIT_ATOMIC,
    HWGC_DONE,
};
struct HWGC_PARALLOCATE_PARS
{
    uint8_t dest_attr_type;
    uint64_t allocator_ptr;
    uint64_t alloc_region;
    uint64_t min_word_size;
    uint64_t desired_word_size;
    uint64_t freelist_lock_ptr;
    uint64_t thread;
};

// 定义 ioctl 命令
#define HWGC_IOC_MAGIC 'H'
#define HWGC_IOC_START _IOW(HWGC_IOC_MAGIC, 0, struct HWGC_PARALLOCATE_PARS)
#define HWGC_IOC_WAIT_EVENT _IOR(HWGC_IOC_MAGIC, 1, int)
#define HWGC_IOC_SOFT_PROVIDE _IOW(HWGC_IOC_MAGIC, 2, uint64_t)

#define ALLOCATE_IRQ 0x01
#define ATTEMPT_IRQ 0x02
#define LOCK_WAKE_IRQ 0x04
#define PAGE_FAULT_IRQ 0x08
#define COMPLETE_IRQ 0x10
#define ATOMIC_IRQ 0x20

#define HWGC_STATUS_COMPUTING 0x01
#define HWGC_STATUS_WAKE 0x02
#define HWGC_STATUS_IRQ 0x04
// reg define
#define REG_STATUS 0x0
#define REG_IRQ_STATUS 0x4
#define REG_START_WORK 0x8
#define REG_CONTINUE_WORK 0xc
#define REG_CLEAR_IRQ 0x10
#define REG_PAR0 0x18 // dest_attr_type
#define REG_PAR1 0x20 // allocator_ptr
#define REG_PAR2 0x28 // alloc_region
#define REG_PAR3 0x30 // min_word_size
#define REG_PAR4 0x38 // desired_word_size
#define REG_PAR5 0x40 // freelist_lock_ptr
#define REG_PAR6 0x48 // thread
#define REG_IRQ_PAR0 0x50
#define REG_IRQ_PAR1 0x58
#define REG_IRQ_PAR2 0x60
#define REG_IRQ_PAR3 0x68
#define REG_IRQ_RES0 0x70 // obj_ptr
#define REG_IRQ_RES1 0x78 // actual_plab_size