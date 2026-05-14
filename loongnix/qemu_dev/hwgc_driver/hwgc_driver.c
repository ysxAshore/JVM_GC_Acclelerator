#include "hwgc_ioctl.h"

#define HWGC_DEV_NAME "hwgc"
struct hwgc_dev
{
    dev_t devno;
    struct cdev cdev;
    struct class *cls;
    struct pci_dev *pdev;

    spinlock_t lock;
    void __iomem *mmio;
    enum hwgc_state state;
    wait_queue_head_t waitq;
};

/* 中断处理 */
static irqreturn_t hwgc_irq_handler(int irq, void *dev_id)
{
    struct hwgc_dev *hwgc = dev_id;
    u32 irq_status = ioread32(hwgc->mmio + REG_IRQ_STATUS);
    unsigned long flags;

    if (!irq_status)
        return IRQ_NONE;

    spin_lock_irqsave(&hwgc->lock, flags);

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
        {ATOMIC_IRQ, HWGC_WAIT_ATOMIC}};

    // 一次只处理一个中断
    int i = 0;
    for (; i < ARRAY_SIZE(irq_map); ++i)
    {
        if (irq_status & irq_map[i].mask)
        {
            iowrite32(irq_map[i].mask, hwgc->mmio + REG_CLEAR_IRQ);
            hwgc->state = irq_map[i].new_state;
            wake_up_interruptible(&hwgc->waitq);
            break;
        }
    }

    spin_unlock_irqrestore(&hwgc->lock, flags);
    return IRQ_HANDLED;
}

static ssize_t hwgc_read(struct file *file, char __user *buf, size_t count, loff_t *ppos)
{
    struct hwgc_dev *hwgc = file->private_data;
    loff_t offset = *ppos;
    u64 val = ~0ULL;
    switch (offset)
    {
    case REG_STATUS:
        val = hwgc->state;
        break;
    case REG_IRQ_PAR0:
        val = readq(hwgc->mmio + offset);
        break;
    case REG_IRQ_PAR1:
        val = readq(hwgc->mmio + offset);
        break;
    case REG_IRQ_PAR2:
        val = readq(hwgc->mmio + offset);
        break;
    case REG_IRQ_PAR3:
        val = readq(hwgc->mmio + offset);
        break;
    case REG_IRQ_RES0:
        val = readq(hwgc->mmio + offset);
        break;
    case REG_IRQ_RES1:
        val = readq(hwgc->mmio + offset);
        break;
    default:
        break;
    }
    if (copy_to_user(buf, &val, count))
        return -EFAULT;

    *ppos += count;
    return count;
}

static void hwgc_write_params(struct hwgc_dev *hwgc, const struct HWGC_PARALLOCATE_PARS *par)
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

static long hwgc_ioctl(struct file *file, unsigned int cmd, unsigned long arg)
{
    struct hwgc_dev *hwgc = file->private_data;
    struct HWGC_PARALLOCATE_PARS hwgc_par;
    int state;
    uint64_t res;
    struct
    {
        uint64_t res;
        uint64_t word_size;
    } temp;
    unsigned long flags;

    switch (cmd)
    {
    case HWGC_IOC_START:
        if (copy_from_user(&hwgc_par, (void __user *)arg, sizeof(hwgc_par)))
            return -EFAULT;

        spin_lock_irqsave(&hwgc->lock, flags);
        hwgc->state = HWGC_RUNNING;
        spin_unlock_irqrestore(&hwgc->lock, flags);

        hwgc_write_params(hwgc, &hwgc_par); // write parameters

        writel(HWGC_STATUS_IRQ, hwgc->mmio + REG_STATUS); // enable interrupts
        writel(1, hwgc->mmio + REG_START_WORK);           // start work

        break;

    case HWGC_IOC_WAIT_EVENT:
        wait_event_interruptible(hwgc->waitq,
                                 hwgc->state != HWGC_IDLE && hwgc->state != HWGC_RUNNING);
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
        if (hwgc->state == HWGC_WAIT_PAGEFAULT || hwgc->state == HWGC_WAIT_ATOMIC)
        {
            if (copy_from_user(&res, (void __user *)arg, sizeof(res)))
                return -EFAULT;
            writeq(res, hwgc->mmio + REG_IRQ_RES0);
        }
        if (hwgc->state == HWGC_WAIT_ATTEMPT)
        {
            if (copy_from_user(&temp, (void __user *)arg, sizeof(temp)))
                return -EFAULT;
            writeq(temp.res, hwgc->mmio + REG_IRQ_RES0);
            writeq(temp.word_size, hwgc->mmio + REG_IRQ_RES1);
        }
        hwgc->state = HWGC_RUNNING;
        spin_unlock_irqrestore(&hwgc->lock, flags);
        writel(1, hwgc->mmio + REG_CONTINUE_WORK);
        break;

    default:
        break;
    }
    return 0;
}

static int hwgc_open(struct inode *inode, struct file *file)
{
    struct hwgc_dev *hwgc = container_of(inode->i_cdev, struct hwgc_dev, cdev);
    file->private_data = hwgc;
    return 0;
}

static const struct file_operations hwgc_fops = {
    .owner = THIS_MODULE,
    .open = hwgc_open,
    .read = hwgc_read,
    .unlocked_ioctl = hwgc_ioctl,
    .llseek = default_llseek, // support lseek function
};

/* PCI probe */
static int hwgc_probe(struct pci_dev *pdev, const struct pci_device_id *id)
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

    // enable pci/pcie device
    ret = pcim_enable_device(pdev);
    if (ret)
        return ret;

    // register and map pci/pcie bar0 memory region
    ret = pcim_iomap_regions(pdev, BIT(0), HWGC_DEV_NAME);
    if (ret)
        return ret;
    hwgc->mmio = pcim_iomap_table(pdev)[0]; // get the bar0 vitual addr

    // ret = pci_enable_msi(pdev);
    // if (ret) {
    //     dev_err(&pdev->dev, "Failed to enable MSI (ret=%d), trying INTx\n", ret);
    // } else {
    //     dev_info(&pdev->dev, "MSI enabled successfully\n");
    // }
    ret = devm_request_irq(&pdev->dev, pdev->irq, hwgc_irq_handler, 0, HWGC_DEV_NAME, hwgc);
    if (ret)
    {
        dev_err(&pdev->dev, "request_irq failed: %d\n", ret);
        return ret;
    }
    else
        dev_info(&pdev->dev, "request irq successfully\n");

    // 注册字符设备
    ret = alloc_chrdev_region(&hwgc->devno, 0, 1, HWGC_DEV_NAME);
    if (ret)
    {
        dev_err(&pdev->dev, "Failed to allocate chrdev\n");
        return ret;
    }

    cdev_init(&hwgc->cdev, &hwgc_fops);
    hwgc->cdev.owner = THIS_MODULE;
    ret = cdev_add(&hwgc->cdev, hwgc->devno, 1);
    if (ret)
    {
        dev_err(&pdev->dev, "Failed to add cdev\n");
        goto err_unregister;
    }

    hwgc->cls = class_create(THIS_MODULE, HWGC_DEV_NAME);
    if (IS_ERR(hwgc->cls))
    {
        ret = PTR_ERR(hwgc->cls);
        goto err_cdev_del;
    }

    if (IS_ERR(device_create(hwgc->cls, &pdev->dev, hwgc->devno, NULL, HWGC_DEV_NAME)))
    {
        dev_err(&pdev->dev, "Failed to create /dev node\n");
        ret = -ENOMEM;
        goto err_class_destroy;
    }

    pci_set_drvdata(pdev, hwgc);
    dev_info(&pdev->dev, "hwgc: device probed at /dev/%s\n", HWGC_DEV_NAME);
    return 0;

err_class_destroy:
    class_destroy(hwgc->cls);
err_cdev_del:
    cdev_del(&hwgc->cdev);
err_unregister:
    unregister_chrdev_region(hwgc->devno, 1);
    return ret;
}

static void hwgc_remove(struct pci_dev *pdev)
{
    struct hwgc_dev *hwgc = pci_get_drvdata(pdev);
    device_destroy(hwgc->cls, hwgc->devno);
    class_destroy(hwgc->cls);
    cdev_del(&hwgc->cdev);
    unregister_chrdev_region(hwgc->devno, 1);
    dev_info(&pdev->dev, "hwgc: device removed\n");
}

static const struct pci_device_id hwgc_pci_ids[] = {
    {PCI_DEVICE(0x1234, 0x0308)},
    {}, // 在 Linux 内核 PCI 驱动中，struct pci_device_id 表必须以一个全零（NULL）条目结尾，用于告诉内核“设备 ID 列表到此结束”
};
MODULE_DEVICE_TABLE(pci, hwgc_pci_ids);

static struct pci_driver hwgc_pci_driver = {
    .name = HWGC_DEV_NAME,
    .id_table = hwgc_pci_ids,
    .probe = hwgc_probe,
    .remove = hwgc_remove,
};

module_pci_driver(hwgc_pci_driver);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("QEMU hwgc driver with user-space test support");
