#include "hwgc_ioctl.h"

#define HWGC_DEV_NAME "hwgc"
#define HWGC_MAX_DEVS 256

struct hwgc_dev
{
    dev_t devno;
    int index;

    struct cdev cdev;
    struct device *dev;

    struct pci_dev *pdev;
    void __iomem *mmio;

    spinlock_t lock;
    enum hwgc_state state;
    wait_queue_head_t waitq;
};

static dev_t hwgc_base_devno;
static struct class *hwgc_class;

static DEFINE_MUTEX(hwgc_minor_lock);
static unsigned long hwgc_minor_bitmap[BITS_TO_LONGS(HWGC_MAX_DEVS)];

static int hwgc_alloc_minor(void)
{
    int index;

    mutex_lock(&hwgc_minor_lock);

    index = find_first_zero_bit(hwgc_minor_bitmap, HWGC_MAX_DEVS);
    if (index >= HWGC_MAX_DEVS)
    {
        mutex_unlock(&hwgc_minor_lock);
        return -ENOSPC;
    }

    set_bit(index, hwgc_minor_bitmap);

    mutex_unlock(&hwgc_minor_lock);
    return index;
}

static void hwgc_free_minor(int index)
{
    if (index < 0 || index >= HWGC_MAX_DEVS)
        return;

    mutex_lock(&hwgc_minor_lock);
    clear_bit(index, hwgc_minor_bitmap);
    mutex_unlock(&hwgc_minor_lock);
}

/* 中断处理 */
static irqreturn_t hwgc_irq_handler(int irq, void *dev_id)
{
    struct hwgc_dev *hwgc = dev_id;
    u32 irq_status;
    unsigned long flags;
    int i;

    struct
    {
        u32 mask;
        enum hwgc_state new_state;
    } irq_map[] = {
        {ALLOCATE_IRQ, HWGC_WAIT_ALLOCATE},
        {ATTEMPT_IRQ, HWGC_WAIT_ATTEMPT},
        {LOCK_WAKE_IRQ, HWGC_WAIT_LOCK_WAKE},
        {PAGE_FAULT_IRQ, HWGC_WAIT_PAGEFAULT},
        {COMPLETE_IRQ, HWGC_DONE},
        {ATOMIC_IRQ, HWGC_WAIT_ATOMIC},
    };

    irq_status = ioread32(hwgc->mmio + REG_IRQ_STATUS);
    if (!irq_status)
        return IRQ_NONE;

    spin_lock_irqsave(&hwgc->lock, flags);

    /*
     * 一次只处理一个中断。
     * 如果 QEMU 可能一次置多个 bit，这里只清一个。
     */
    for (i = 0; i < ARRAY_SIZE(irq_map); ++i)
        if (irq_status & irq_map[i].mask)
        {
            iowrite32(irq_map[i].mask, hwgc->mmio + REG_CLEAR_IRQ);
            hwgc->state = irq_map[i].new_state;
            wake_up_interruptible(&hwgc->waitq);
            break;
        }

    spin_unlock_irqrestore(&hwgc->lock, flags);

    return IRQ_HANDLED;
}

static ssize_t hwgc_read(struct file *file,
                         char __user *buf,
                         size_t count,
                         loff_t *ppos)
{
    struct hwgc_dev *hwgc = file->private_data;
    loff_t offset = *ppos;
    u64 val = ~0ULL;

    if (count > sizeof(val))
        return -EINVAL;

    switch (offset)
    {
    case REG_STATUS:
        val = hwgc->state;
        break;

    case REG_IRQ_PAR0:
    case REG_IRQ_PAR1:
    case REG_IRQ_PAR2:
    case REG_IRQ_PAR3:
    case REG_IRQ_RES0:
    case REG_IRQ_RES1:
        val = readq(hwgc->mmio + offset);
        break;

    default:
        return -EINVAL;
    }

    if (copy_to_user(buf, &val, count))
        return -EFAULT;

    *ppos += count;
    return count;
}

static void hwgc_write_params(struct hwgc_dev *hwgc,
                              const struct HWGC_PARALLOCATE_PARS *par)
{
    void __iomem *base = hwgc->mmio + REG_PAR0;

    writeq(par->dest_attr_type, base + 0x00);
    writeq(par->allocator_ptr, base + 0x08);
    writeq(par->alloc_region, base + 0x10);
    writeq(par->min_word_size, base + 0x18);
    writeq(par->desired_word_size, base + 0x20);
    writeq(par->freelist_lock_ptr, base + 0x28);
    writeq(par->thread, base + 0x30);
}

static long hwgc_ioctl(struct file *file,
                       unsigned int cmd,
                       unsigned long arg)
{
    struct hwgc_dev *hwgc = file->private_data;
    struct HWGC_PARALLOCATE_PARS hwgc_par;

    unsigned long flags;
    enum hwgc_state cur_state;
    int state;
    int ret;

    struct
    {
        u64 res;
        u64 word_size;
    } temp = {0, 0};

    switch (cmd)
    {
    case HWGC_IOC_START:
        if (copy_from_user(&hwgc_par, (void __user *)arg, sizeof(hwgc_par)))
            return -EFAULT;

        spin_lock_irqsave(&hwgc->lock, flags);

        if (hwgc->state != HWGC_IDLE)
        {
            spin_unlock_irqrestore(&hwgc->lock, flags);
            return -EBUSY;
        }

        hwgc->state = HWGC_RUNNING;

        spin_unlock_irqrestore(&hwgc->lock, flags);

        hwgc_write_params(hwgc, &hwgc_par);

        writel(HWGC_STATUS_IRQ, hwgc->mmio + REG_STATUS);
        writel(1, hwgc->mmio + REG_START_WORK);

        break;

    case HWGC_IOC_WAIT_EVENT:
        ret = wait_event_interruptible(hwgc->waitq, hwgc->state != HWGC_IDLE && hwgc->state != HWGC_RUNNING);

        if (ret)
            return ret;

        spin_lock_irqsave(&hwgc->lock, flags);

        state = hwgc->state;

        if (state == HWGC_DONE)
            hwgc->state = HWGC_IDLE;

        spin_unlock_irqrestore(&hwgc->lock, flags);

        if (copy_to_user((void __user *)arg, &state, sizeof(state)))
            return -EFAULT;

        break;

    case HWGC_IOC_SOFT_PROVIDE:
        spin_lock_irqsave(&hwgc->lock, flags);
        cur_state = hwgc->state;
        spin_unlock_irqrestore(&hwgc->lock, flags);

        switch (cur_state)
        {
        case HWGC_WAIT_PAGEFAULT:
        case HWGC_WAIT_ATOMIC:
        case HWGC_WAIT_ALLOCATE:
        case HWGC_WAIT_LOCK_WAKE:
            if (copy_from_user(&temp.res, (void __user *)arg, sizeof(temp.res)))
                return -EFAULT;
            break;

        case HWGC_WAIT_ATTEMPT:
            if (copy_from_user(&temp, (void __user *)arg, sizeof(temp)))
                return -EFAULT;
            break;

        default:
            return -EINVAL;
        }

        spin_lock_irqsave(&hwgc->lock, flags);

        if (hwgc->state != cur_state)
        {
            spin_unlock_irqrestore(&hwgc->lock, flags);
            return -EAGAIN;
        }

        switch (cur_state)
        {
        case HWGC_WAIT_PAGEFAULT:
        case HWGC_WAIT_ATOMIC:
        case HWGC_WAIT_ALLOCATE:
        case HWGC_WAIT_LOCK_WAKE:
            writeq(temp.res, hwgc->mmio + REG_IRQ_RES0);
            break;

        case HWGC_WAIT_ATTEMPT:
            writeq(temp.res, hwgc->mmio + REG_IRQ_RES0);
            writeq(temp.word_size, hwgc->mmio + REG_IRQ_RES1);
            break;

        default:
            spin_unlock_irqrestore(&hwgc->lock, flags);
            return -EINVAL;
        }

        hwgc->state = HWGC_RUNNING;

        spin_unlock_irqrestore(&hwgc->lock, flags);

        writel(1, hwgc->mmio + REG_CONTINUE_WORK);

        break;

    default:
        return -ENOTTY;
    }

    return 0;
}

static int hwgc_open(struct inode *inode, struct file *file)
{
    struct hwgc_dev *hwgc;

    hwgc = container_of(inode->i_cdev, struct hwgc_dev, cdev);
    file->private_data = hwgc;
    return 0;
}

static const struct file_operations hwgc_fops = {
    .owner = THIS_MODULE,
    .open = hwgc_open,
    .read = hwgc_read,
    .unlocked_ioctl = hwgc_ioctl,
    .llseek = default_llseek,
};

/* PCI probe */
static int hwgc_probe(struct pci_dev *pdev,
                      const struct pci_device_id *id)
{
    struct hwgc_dev *hwgc;
    int ret;

    hwgc = devm_kzalloc(&pdev->dev, sizeof(*hwgc), GFP_KERNEL);
    if (!hwgc)
        return -ENOMEM;

    hwgc->pdev = pdev;
    hwgc->state = HWGC_IDLE;

    spin_lock_init(&hwgc->lock);
    init_waitqueue_head(&hwgc->waitq);

    pci_set_drvdata(pdev, hwgc);

    ret = pcim_enable_device(pdev);
    if (ret)
    {
        dev_err(&pdev->dev, "pcim_enable_device failed: %d\n", ret);
        return ret;
    }

    pci_set_master(pdev);

    ret = pcim_iomap_regions(pdev, BIT(0), HWGC_DEV_NAME);
    if (ret)
    {
        dev_err(&pdev->dev, "pcim_iomap_regions failed: %d\n", ret);
        return ret;
    }

    hwgc->mmio = pcim_iomap_table(pdev)[0];
    if (!hwgc->mmio)
    {
        dev_err(&pdev->dev, "BAR0 iomap failed\n");
        return -ENOMEM;
    }

    // 多个 PCI 设备可能共享 INTx，所以这里用 IRQF_SHARED。irq_handler 里会检查 REG_IRQ_STATUS，不属于自己的中断返回 IRQ_NONE。
    ret = devm_request_irq(&pdev->dev, pdev->irq, hwgc_irq_handler, IRQF_SHARED, HWGC_DEV_NAME, hwgc);
    if (ret)
    {
        dev_err(&pdev->dev, "request_irq failed: %d\n", ret);
        return ret;
    }

    hwgc->index = hwgc_alloc_minor();
    if (hwgc->index < 0)
    {
        dev_err(&pdev->dev, "no free minor\n");
        return hwgc->index;
    }

    hwgc->devno = MKDEV(MAJOR(hwgc_base_devno), hwgc->index);

    cdev_init(&hwgc->cdev, &hwgc_fops);
    hwgc->cdev.owner = THIS_MODULE;

    ret = cdev_add(&hwgc->cdev, hwgc->devno, 1);
    if (ret)
    {
        dev_err(&pdev->dev, "cdev_add failed: %d\n", ret);
        goto err_free_minor;
    }

    hwgc->dev = device_create(hwgc_class, &pdev->dev, hwgc->devno, hwgc, HWGC_DEV_NAME "%d", hwgc->index);
    if (IS_ERR(hwgc->dev))
    {
        ret = PTR_ERR(hwgc->dev);
        dev_err(&pdev->dev, "device_create failed: %d\n", ret);
        goto err_cdev_del;
    }

    dev_info(&pdev->dev, "hwgc: device probed as /dev/%s%d\n", HWGC_DEV_NAME, hwgc->index);

    return 0;

err_cdev_del:
    cdev_del(&hwgc->cdev);

err_free_minor:
    hwgc_free_minor(hwgc->index);

    return ret;
}

static void hwgc_remove(struct pci_dev *pdev)
{
    struct hwgc_dev *hwgc = pci_get_drvdata(pdev);

    if (!hwgc)
        return;

    device_destroy(hwgc_class, hwgc->devno);
    cdev_del(&hwgc->cdev);
    hwgc_free_minor(hwgc->index);

    dev_info(&pdev->dev, "hwgc: device /dev/%s%d removed\n", HWGC_DEV_NAME, hwgc->index);
}

static const struct pci_device_id hwgc_pci_ids[] = {
    {PCI_DEVICE(0x1234, 0x0308)},
    {},
};
MODULE_DEVICE_TABLE(pci, hwgc_pci_ids);

static struct pci_driver hwgc_pci_driver = {
    .name = HWGC_DEV_NAME,
    .id_table = hwgc_pci_ids,
    .probe = hwgc_probe,
    .remove = hwgc_remove,
};

static int __init hwgc_init(void)
{
    int ret;

    ret = alloc_chrdev_region(&hwgc_base_devno, 0, HWGC_MAX_DEVS, HWGC_DEV_NAME);
    if (ret)
    {
        pr_err("hwgc: alloc_chrdev_region failed: %d\n", ret);
        return ret;
    }

#if LINUX_VERSION_CODE >= KERNEL_VERSION(6, 4, 0)
    hwgc_class = class_create(HWGC_DEV_NAME);
#else
    hwgc_class = class_create(THIS_MODULE, HWGC_DEV_NAME);
#endif

    if (IS_ERR(hwgc_class))
    {
        ret = PTR_ERR(hwgc_class);
        pr_err("hwgc: class_create failed: %d\n", ret);
        goto err_unregister_chrdev;
    }

    ret = pci_register_driver(&hwgc_pci_driver);
    if (ret)
    {
        pr_err("hwgc: pci_register_driver failed: %d\n", ret);
        goto err_class_destroy;
    }

    pr_info("hwgc: module loaded, major=%d\n", MAJOR(hwgc_base_devno));
    return 0;

err_class_destroy:
    class_destroy(hwgc_class);

err_unregister_chrdev:
    unregister_chrdev_region(hwgc_base_devno, HWGC_MAX_DEVS);

    return ret;
}

static void __exit hwgc_exit(void)
{
    pci_unregister_driver(&hwgc_pci_driver);

    class_destroy(hwgc_class);
    unregister_chrdev_region(hwgc_base_devno, HWGC_MAX_DEVS);

    pr_info("hwgc: module unloaded\n");
}

module_init(hwgc_init);
module_exit(hwgc_exit);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("QEMU hwgc PCI driver with per-device /dev/hwgcN nodes");