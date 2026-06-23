/*
 * Linux 4.19 PCI driver for QEMU xor-tlbdev.
 *
 * Userspace passes virtual addresses of: u64 a, u64 b, u64 out
 *
 * The QEMU device performs address translation using its own TLB.
 * On a TLB miss, it raises an interrupt. The threaded IRQ handler pins/translates the user page and fills the device TLB.
 *
 * This implementation targets the Linux 4.19 GUP API:
 *   get_user_pages_remote(), put_page(), mm->mmap_sem
 *
 * Build: make
 *
 * Device node: /dev/xor_tlbdev0
 */

#include <linux/module.h>
#include <linux/pci.h>
#include <linux/interrupt.h>
#include <linux/miscdevice.h>
#include <linux/fs.h>
#include <linux/uaccess.h>
#include <linux/completion.h>
#include <linux/mutex.h>
#include <linux/mm.h>
#include <linux/sched.h>
#include <linux/sched/mm.h>
#include <linux/sched/task.h>
#include <linux/highmem.h>
#include <linux/io.h>
#include <linux/slab.h>

/* defined in hw/misc/test_device.c */
#define DRV_NAME      "xor_tlbdev"
#define XOR_VENDOR_ID 0x1234
#define XOR_DEVICE_ID 0x0420

/* MMIO register map; must match the QEMU device. */
#define REG_STATUS       0x00
#define REG_IRQ_STATUS   0x04
#define REG_IRQ_CLEAR    0x08
#define REG_CMD          0x0c

#define REG_A_VA         0x10
#define REG_B_VA         0x18
#define REG_OUT_VA       0x20

#define REG_MISS_VA      0x28
#define REG_MISS_ACCESS  0x30
#define REG_TLB_FILL_VA  0x38
#define REG_TLB_FILL_PA  0x40

#define REG_PERIOD_NS    0x48
#define REG_STAGE        0x50

#define CMD_START        0x01
#define CMD_CONTINUE     0x02
#define CMD_RESET        0x04

#define ST_BUSY          0x00000001
#define ST_DONE          0x00000002
#define ST_WAIT_TLB      0x00000004
#define ST_ERROR         0x00000008
#define ST_IRQ_EN        0x00000010

#define IRQ_TLB_MISS     0x00000001
#define IRQ_DONE         0x00000002
#define IRQ_ERROR        0x00000004
#define IRQ_ALL          (IRQ_TLB_MISS | IRQ_DONE | IRQ_ERROR)

#define ACCESS_READ      1
#define ACCESS_WRITE     2

#define MAX_PINNED       16

struct xor_tlb_op {
	// a, b, out都是guest vaddr
	__u64 a;
	__u64 b;
	__u64 out;
	// driver会从out中读取数据填到result
	__u64 result;
};

#define XOR_TLB_IOC_MAGIC 'x'
#define XOR_TLB_IOC_RUN _IOWR(XOR_TLB_IOC_MAGIC, 1, struct xor_tlb_op)

struct pinned_page {
	struct page *page; // get_user_pages_remote() 的return value
	bool write; // 是否被修改过, 若有则在释放前需要set_page_dirty_lock()
};

struct xor_tlbdev {
	struct pci_dev *pdev;
	void __iomem *bar0;
	int irq;

	struct miscdevice miscdev; // 字符设备
	struct mutex op_lock; // 保证同一时刻只有一个ioctl操作在使用设备
	struct completion done; // ioctl线程sleep在completion上 等待IRQ DONE/ERROR 唤醒

	//  References are held while an operation is active.  
	//  Linux 4.19 get_user_pages_remote() requires both task and mm. 
	//  表示正在运行的ioctl进程上下文, 因为中断发生在中断线程所以需要提前保存 才能在中断线程中对用户地址做GUP(get_user_page)
	struct task_struct *active_task;
	struct mm_struct *active_mm;

	bool op_active; // 有设备操作正在进行
	int op_status;

	spinlock_t irq_lock;
	u32 pending_irq;

	// pin 住的 page
	struct pinned_page pinned[MAX_PINNED];
	int nr_pinned;
};

/*
 * Single-device demonstration.
 * For multiple devices, private_data should be associated with each miscdevice/open file rather than using a global pointer.
 */
static struct xor_tlbdev *global_dev;

static inline void reg_write32(struct xor_tlbdev *d, u32 val, u32 off)
{
	iowrite32(val, d->bar0 + off);
}

static inline u32 reg_read32(struct xor_tlbdev *d, u32 off)
{
	return ioread32(d->bar0 + off);
}

static inline void reg_write64(struct xor_tlbdev *d, u64 val, u32 off)
{
#if BITS_PER_LONG == 64
	writeq(val, d->bar0 + off);
#else
	iowrite32(lower_32_bits(val), d->bar0 + off);
	iowrite32(upper_32_bits(val), d->bar0 + off + 4);
#endif
}

static inline u64 reg_read64(struct xor_tlbdev *d, u32 off)
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

/*
 * Drop every GUP reference held by the current operation.
 *
 * Linux 4.19 does not have unpin_user_page().
 * get_user_pages_remote() references are released with put_page().
 */
static void xor_unpin_all(struct xor_tlbdev *d)
{
	struct page *page;
	int i;

	for (i = 0; i < d->nr_pinned; i++) {
		page = d->pinned[i].page;
		if (!page)
			continue;

		/*
		 * Conservatively mark pages writable by the device dirty
		 * before dropping the GUP reference.
		 */
		if (d->pinned[i].write)
			set_page_dirty_lock(page);

		put_page(page);

		d->pinned[i].page = NULL;
		d->pinned[i].write = false;
	}

	d->nr_pinned = 0;
}

/*
 * Record a page returned by get_user_pages_remote().
 *
 * A repeated GUP call on the same page obtains another page reference.
 * If the page is already in the array, drop that extra reference while
 * retaining the original one.
 */
static int xor_remember_pinned(struct xor_tlbdev *d, struct page *page, bool write)
{
	int i;

	// 同一次操作 可能会对同一页多次TLB miss 如果发现这页已经被记录过 则只保留原来的引用
	// 通过GUP多出来的重复引用需要PUT掉以减少该page的引用次数
	for (i = 0; i < d->nr_pinned; i++) {
		if (d->pinned[i].page == page) {
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

// 释放进程数据
static void xor_release_process_context(struct xor_tlbdev *d)
{
	if (d->active_mm) {
		mmput(d->active_mm);
		d->active_mm = NULL;
	}

	if (d->active_task) {
		put_task_struct(d->active_task);
		d->active_task = NULL;
	}
}

/*
 * Translate a user VA belonging to the active process:
 *
 *   user VA
 *       -> get_user_pages_remote()
 *       -> struct page
 *       -> guest physical page address
 *
 * This runs in threaded IRQ context, so it is allowed to sleep.
 */
static int xor_fill_tlb_for_miss(struct xor_tlbdev *d)
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

	miss_va = (unsigned long)reg_read64(d, REG_MISS_VA);
	access = reg_read32(d, REG_MISS_ACCESS);

	if (access != ACCESS_READ && access != ACCESS_WRITE)
		return -EINVAL;

	write = access == ACCESS_WRITE;
	page_va = miss_va & PAGE_MASK; // 14 bit page

	gup_flags = 0;
	if (write)
		gup_flags |= FOLL_WRITE;

	page = NULL;
	locked = 1;

	/*
	 * Linux 4.19 uses mm->mmap_sem rather than mmap_read_lock().
	 *
	 * get_user_pages_remote() may release mmap_sem internally and set
	 * locked to zero. Therefore, unlock only when locked remains one.
	 */
	down_read(&mm->mmap_sem);

	ret = get_user_pages_remote(task, mm, page_va, 1, gup_flags, &page, NULL, &locked);

	if (locked)
		up_read(&mm->mmap_sem);

	if (ret != 1) {
		if (ret < 0)
			return (int)ret;

		return -EFAULT;
	}

	if (!page)
		return -EFAULT;

	ret = xor_remember_pinned(d, page, write);
	if (ret) {
		/*
		 * The device has not been told to continue yet, so it has
		 * not written through this translation.
		 */
		put_page(page);
		return (int)ret;
	}

    // For this QEMU device, page_to_phys() produces the guest physical address expected by the emulated device.
	pa_page = page_to_phys(page) & PAGE_MASK;

	reg_write64(d, (u64)page_va, REG_TLB_FILL_VA);
	reg_write64(d, (u64)pa_page, REG_TLB_FILL_PA);

	// Tell the device to install the new translation and continue.
	reg_write32(d, CMD_CONTINUE, REG_CMD);

	return 0;
}

/*
 * Hard IRQ handler:
 *
 * Only acknowledge and collect status here. All device event handling,
 * including DONE and ERROR, is performed by the threaded handler.
 */
static irqreturn_t xor_irq_top(int irq, void *data)
{
	struct xor_tlbdev *d;
	unsigned long flags;
	u32 st;

	(void)irq;

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

static irqreturn_t xor_irq_thread(int irq, void *data)
{
	struct xor_tlbdev *d;
	unsigned long flags;
	u32 pending;
	int ret;

	(void)irq;

	d = data;

	spin_lock_irqsave(&d->irq_lock, flags);
	pending = d->pending_irq;
	d->pending_irq = 0;
	spin_unlock_irqrestore(&d->irq_lock, flags);

	/*
	 * Treat a device error as authoritative. Do not try to service a
	 * simultaneous TLB miss after an error has already been reported.
	 */
	if (pending & IRQ_ERROR) {
		d->op_status = -EIO;
		printk("irq error\n");
		reg_write32(d, CMD_RESET, REG_CMD);
		complete(&d->done);
		return IRQ_HANDLED;
	}

	if (pending & IRQ_TLB_MISS) {
		printk("irq tlb miss\n");
		ret = xor_fill_tlb_for_miss(d);
		if (ret) {
			d->op_status = ret;
			reg_write32(d, CMD_RESET, REG_CMD);
			complete(&d->done);
			return IRQ_HANDLED;
		}
	}

	if (pending & IRQ_DONE) {
		d->op_status = 0;
		complete(&d->done);
	}

	return IRQ_HANDLED;
}

static bool xor_u64_user_addr_ok(u64 addr)
{
	if (!addr)
		return false;

	if (addr & (sizeof(u64) - 1))
		return false;

	/*
	 * Reject a u64 object crossing a page boundary. The device expects
	 * each operand to be covered by one TLB entry.
	 */
	if ((addr & (PAGE_SIZE - 1)) > PAGE_SIZE - sizeof(u64))
		return false;

	return true;
}

static long xor_ioctl(struct file *filp, unsigned int cmd, unsigned long arg)
{
	struct xor_tlbdev *d;
	struct xor_tlb_op op;
	unsigned long flags;
	long timeout;
	int ret;

	(void)filp;

	d = global_dev;
	if (!d)
		return -ENODEV;

	if (cmd != XOR_TLB_IOC_RUN)
		return -ENOTTY;

	if (copy_from_user(&op, (void __user *)arg, sizeof(op)))
		return -EFAULT;

	if (!xor_u64_user_addr_ok(op.a) ||
	    !xor_u64_user_addr_ok(op.b) ||
	    !xor_u64_user_addr_ok(op.out))
		return -EINVAL;

	mutex_lock(&d->op_lock);

	ret = 0;

	if (d->op_active) {
		ret = -EBUSY;
		goto out_unlock;
	}

	// Hold both task and mm references for the threaded IRQ handler.
	d->active_task = current;
	get_task_struct(d->active_task);

	d->active_mm = get_task_mm(current);
	if (!d->active_mm) {
		put_task_struct(d->active_task);
		d->active_task = NULL;
		ret = -EINVAL;
		goto out_unlock;
	}

	d->op_active = true;
	d->op_status = 0;
	d->nr_pinned = 0;

	reinit_completion(&d->done);

	spin_lock_irqsave(&d->irq_lock, flags);
	d->pending_irq = 0;
	spin_unlock_irqrestore(&d->irq_lock, flags);

	// Stop any previous operation and clear stale interrupt status.
	reg_write32(d, CMD_RESET, REG_CMD);
	reg_write32(d, IRQ_ALL, REG_IRQ_CLEAR);

	// Enable device interrupts.
	reg_write32(d, ST_IRQ_EN, REG_STATUS);

	reg_write64(d, op.a, REG_A_VA);
	reg_write64(d, op.b, REG_B_VA);
	reg_write64(d, op.out, REG_OUT_VA);

	reg_write32(d, CMD_START, REG_CMD);

	// 等待设备完成
	timeout = wait_for_completion_interruptible_timeout(&d->done, msecs_to_jiffies(11111111));
	pr_info("xor: wait returned %ld, op_status=%d\n", timeout, d->op_status);
	if (timeout == 0) {
		ret = -ETIMEDOUT;
		reg_write32(d, CMD_RESET, REG_CMD);
	} else if (timeout < 0) {
		ret = (int)timeout;
		reg_write32(d, CMD_RESET, REG_CMD);
	} else {
		ret = d->op_status;
	}

	/*
	 * Ensure the threaded IRQ handler no longer uses active_task,
	 * active_mm or any pinned page before releasing those references.
	 */
	synchronize_irq(d->irq);

	if (!ret) {
		if (copy_from_user(
			    &op.result,
			    (void __user *)(unsigned long)op.out,
			    sizeof(op.result))) {
			ret = -EFAULT;
		} else if (copy_to_user(
				   (void __user *)arg,
				   &op,
				   sizeof(op))) {
			ret = -EFAULT;
		}
	}

	// 操作结束
	xor_unpin_all(d);
	xor_release_process_context(d);

	d->op_active = false;

out_unlock:
	mutex_unlock(&d->op_lock);
	return ret;
}

static const struct file_operations xor_fops = {
	.owner          = THIS_MODULE,
	.unlocked_ioctl = xor_ioctl,
#ifdef CONFIG_COMPAT
	.compat_ioctl   = xor_ioctl,
#endif
	.llseek         = no_llseek,
};

static int xor_probe(struct pci_dev *pdev, const struct pci_device_id *id)
{

	/*
	 * probe 阶段负责把 PCI 设备初始化成一个可用的字符设备：
	 *
	 *   1. 启用 PCI device
	 *   2. 申请 BAR region
	 *   3. 映射 BAR0 MMIO
	 *   4. 分配 IRQ vector
	 *   5. 注册 threaded IRQ
	 *   6. 注册 miscdevice，生成 /dev/xor_tlbdev0
	 */
	struct xor_tlbdev *d;
	int ret;

	(void)id;

	d = kzalloc(sizeof(*d), GFP_KERNEL);
	if (!d)
		return -ENOMEM;

	d->pdev = pdev;
	pci_set_drvdata(pdev, d);

	ret = pci_enable_device(pdev);
	if (ret)
		goto err_free;

	ret = pci_request_regions(pdev, DRV_NAME);
	if (ret)
		goto err_disable;

	pci_set_master(pdev);

	d->bar0 = pci_iomap(pdev, 0, 0);
	if (!d->bar0) {
		ret = -ENOMEM;
		goto err_regions;
	}

	ret = pci_alloc_irq_vectors(pdev, 1, 1, PCI_IRQ_MSI | PCI_IRQ_LEGACY);
	if (ret < 0)
		goto err_iounmap;

	d->irq = pci_irq_vector(pdev, 0);
	if (d->irq < 0) {
		ret = d->irq;
		goto err_irq_vectors;
	}

	spin_lock_init(&d->irq_lock);
	mutex_init(&d->op_lock);
	init_completion(&d->done);

	ret = request_threaded_irq(d->irq, xor_irq_top, xor_irq_thread, IRQF_ONESHOT, DRV_NAME, d);
	if (ret)
		goto err_irq_vectors;

	d->miscdev.minor = MISC_DYNAMIC_MINOR;
	d->miscdev.name = "xor_tlbdev0";
	d->miscdev.fops = &xor_fops;
	d->miscdev.parent = &pdev->dev;

	ret = misc_register(&d->miscdev);
	if (ret)
		goto err_free_irq;

	global_dev = d;

	dev_info(&pdev->dev, "xor-tlbdev Linux 4.19 driver loaded: /dev/%s\n", d->miscdev.name);

	return 0;

err_free_irq:
	free_irq(d->irq, d);

err_irq_vectors:
	pci_free_irq_vectors(pdev);

err_iounmap:
	pci_iounmap(pdev, d->bar0);

err_regions:
	pci_clear_master(pdev);
	pci_release_regions(pdev);

err_disable:
	pci_disable_device(pdev);

err_free:
	pci_set_drvdata(pdev, NULL);
	kfree(d);
	return ret;
}

static void xor_remove(struct pci_dev *pdev)
{
	struct xor_tlbdev *d;

	d = pci_get_drvdata(pdev);
	if (!d)
		return;

	// Prevent new ;ioctl calls through the global demonstration pointer.
	global_dev = NULL;
	misc_deregister(&d->miscdev);

	/*
	 * Wait for an in-progress ioctl operation. The ioctl holds op_lock
	 * for the complete lifetime of the device operation.
	 */
	mutex_lock(&d->op_lock);

	reg_write32(d, CMD_RESET, REG_CMD);
	synchronize_irq(d->irq);

	xor_unpin_all(d);
	xor_release_process_context(d);
	d->op_active = false;

	mutex_unlock(&d->op_lock);

	free_irq(d->irq, d);
	pci_free_irq_vectors(pdev);

	pci_iounmap(pdev, d->bar0);

	pci_clear_master(pdev);
	pci_release_regions(pdev);
	pci_disable_device(pdev);

	pci_set_drvdata(pdev, NULL);
	kfree(d);
}

static const struct pci_device_id xor_ids[] = {
	{ PCI_DEVICE(XOR_VENDOR_ID, XOR_DEVICE_ID) },
	{ 0, }
};
MODULE_DEVICE_TABLE(pci, xor_ids);

static struct pci_driver xor_pci_driver = {
	.name     = DRV_NAME,
	.id_table = xor_ids,
	.probe    = xor_probe,
	.remove   = xor_remove,
};

module_pci_driver(xor_pci_driver);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("demo");
MODULE_DESCRIPTION("QEMU xor-tlbdev PCI driver with Linux 4.19 GUP compatibility");
