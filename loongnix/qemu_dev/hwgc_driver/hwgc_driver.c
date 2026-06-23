#include "hwgc_ioctl.h"

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
    page_va = miss_va & PAGE_MASK; // 14 bit page

    gup_flags = 0;
    if (write)
        gup_flags |= FOLL_WRITE;

    page = NULL;
    locked = 1;

    down_read(&mm->mmap_sem);

    ret = get_user_pages_remote(task, mm, page_va, 1, gup_flags, &page, NULL, &locked);

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

    /*
     * 当前实现只允许一个 outstanding software event。
     *
     * 因为你的设备寄存器也只有一组 IRQ_PAR / IRQ_RES。
     * 如果以后设备允许多个 software event 并发，就需要 event_id/FIFO。
     */
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

    // Make sure no IRQ thread is still using active_task, active_mm, or pinned pages before releasing them.
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
        pr_err("hwgc: failed to publish ERROR event: %d\n", ret);
}

static irqreturn_t hwgc_irq_top(int irq, void *data)
{
    struct hwgc *d;
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

static irqreturn_t hwgc_irq_thread(int irq, void *data)
{
    struct hwgc *d;
    unsigned long flags;
    u32 pending;
    int ret;

    (void)irq;

    d = data;

    spin_lock_irqsave(&d->irq_lock, flags);
    pending = d->pending_irq;
    d->pending_irq = 0;
    spin_unlock_irqrestore(&d->irq_lock, flags);

    if (pending & IRQ_TLB_MISS)
    {
        pr_info("hwgc: IRQ_TLB_MISS\n");
        ret = hwgc_fill_tlb_for_miss(d);
        if (ret)
        {
            pr_err("hwgc:  LB miss hadling failed: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_DONE)
    {
        pr_info("hwgc: IRQ_DONE\n");
        d->op_status = 0;
        ret = hwgc_publish_sw_event(d, HWGC_EVENT_DONE, 0, 0);
        if (ret)
        {
            pr_err("hwgc: failed to publish DONE: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_GORW)
    {
        pr_info("hwgc: irq grow\n");

        ret = hwgc_publish_sw_event(d, HWGC_EVENT_GROW, reg_read64(d, REG_IRQ_PAR0), reg_read64(d, REG_IRQ_PAR1));
        if (ret)
        {
            pr_err("hwgc: failed to publish GROW: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_EXPAND)
    {
        pr_info("hwgc: irq expand\n");

        ret = hwgc_publish_sw_event(d, HWGC_EVENT_EXPAND, reg_read64(d, REG_IRQ_PAR0), reg_read64(d, REG_IRQ_PAR1));
        if (ret)
        {
            pr_err("hwgc: failed to publish EXPAND: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_ALLOCATE)
    {
        pr_info("hwgc: irq allocate\n");

        ret = hwgc_publish_sw_event(d, HWGC_EVENT_ALLOCATE, reg_read64(d, REG_IRQ_PAR0), reg_read64(d, REG_IRQ_PAR1));
        if (ret)
        {
            pr_err("hwgc: failed to publish ALLOCATE: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_WAKE)
    {
        pr_info("hwgc: irq wake\n");

        ret = hwgc_publish_sw_event(d, HWGC_EVENT_WAKE, reg_read64(d, REG_IRQ_PAR0), reg_read64(d, REG_IRQ_PAR1));
        if (ret)
        {
            pr_err("hwgc: failed to publish WAKE: %d\n", ret);
            hwgc_abort_operation_from_irq(d, ret);
            return IRQ_HANDLED;
        }
    }

    if (pending & IRQ_ERROR)
    {
        pr_info("hwgc: IRQ_ERROR\n");
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

    ret = wait_event_interruptible(d->event_wq, d->event_pending || d->removing || !READ_ONCE(d->op_active));
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
        // copy_to_user 失败时，把 event 回滚成 pending，避免事件卡死在 in_service 状态。
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

static int hwgc_probe(struct pci_dev *pdev, const struct pci_device_id *id)
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
    struct hwgc *d;
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
    if (!d->bar0)
    {
        ret = -ENOMEM;
        goto err_regions;
    }

    ret = pci_alloc_irq_vectors(pdev, 1, 1, PCI_IRQ_MSI | PCI_IRQ_LEGACY);
    if (ret < 0)
        goto err_iounmap;

    d->irq = pci_irq_vector(pdev, 0);
    if (d->irq < 0)
    {
        ret = d->irq;
        goto err_irq_vectors;
    }

    spin_lock_init(&d->irq_lock);
    spin_lock_init(&d->event_lock);
    mutex_init(&d->op_lock);
    init_waitqueue_head(&d->event_wq);

    d->event_pending = false;
    d->event_in_service = false;
    d->event_seq = 0;
    d->removing = false;
    WRITE_ONCE(d->op_active, false);

    ret = request_threaded_irq(d->irq, hwgc_irq_top, hwgc_irq_thread, IRQF_ONESHOT, DRV_NAME, d);
    if (ret)
        goto err_irq_vectors;

    d->miscdev.minor = MISC_DYNAMIC_MINOR;
    d->miscdev.name = "hwgc0";
    d->miscdev.fops = &hwgc_fops;
    d->miscdev.parent = &pdev->dev;

    ret = misc_register(&d->miscdev);
    if (ret)
        goto err_free_irq;

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

static void hwgc_remove(struct pci_dev *pdev)
{
    struct hwgc *d;
    unsigned long flags;

    d = pci_get_drvdata(pdev);
    if (!d)
        return;

    spin_lock_irqsave(&d->event_lock, flags);
    d->removing = true;
    hwgc_clear_event_state_locked(d);
    spin_unlock_irqrestore(&d->event_lock, flags);

    wake_up_interruptible_all(&d->event_wq);

    misc_deregister(&d->miscdev);

    mutex_lock(&d->op_lock);

    reg_write32(d, CMD_RESET, REG_CMD);
    synchronize_irq(d->irq);

    hwgc_unpin_all(d);
    hwgc_release_process_context(d);
    WRITE_ONCE(d->op_active, false);

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

static const struct pci_device_id hwgc_ids[] = {
    {PCI_DEVICE(HWGC_VENDOR_ID, HWGC_DEVICE_ID)},
    {
        0,
    }};
MODULE_DEVICE_TABLE(pci, hwgc_ids);

static struct pci_driver hwgc_pci_driver = {
    .name = DRV_NAME,
    .id_table = hwgc_ids,
    .probe = hwgc_probe,
    .remove = hwgc_remove,
};

module_pci_driver(hwgc_pci_driver);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("sxyang");
MODULE_DESCRIPTION("QEMU HWGC PCI driver with Linux 4.19 GUP compatibility");