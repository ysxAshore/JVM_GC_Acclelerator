// hwgc_driver.c
#include <linux/module.h>
#include <linux/pci.h>
#include <linux/io.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/uaccess.h>
#include <linux/interrupt.h>
#include <linux/wait.h>
#include <linux/sched.h>

#define REG_DEVICE_ID 0x0
#define REG_STATUS 0x4
#define REG_INT_STATUS 0x8
#define REG_CLEAR_IRQ 0xc
#define REG_PAR 0x10
#define REG_START_WORK 0x80

#define IRQ_ALLOC_SLOW 0x00000001
#define IRQ_ENQUEUE_FAILED 0x00000100
#define IRQ_COMPLETE 0x00010000

#define HWGC_STATUS_IRQ 0x80
#define HWGC_STATUS_COMPUTING 0x01

#define HWGC_DEV_NAME "hwgc"

struct HWGCParameter
{
    uint32_t chunkSize;
    uint32_t ageThreshold;
    uint32_t heapRegionBias;
    uint32_t regionAttrShiftBy;
    uint32_t heapRegionShiftBy;
    uint32_t logOfHRGrainBytes;
    uint64_t stepperOffset;
    uint64_t youngWordsBase;
    uint64_t regionAttrBase;
    uint64_t plabAllocatorPtr;
    uint64_t regionAttrBiasedBase;
    uint64_t heapRegionBiasedBase;
    uint64_t parScanThreadStatePtr;
    uint64_t taskQueueBottomAddr;
    uint64_t taskQueueAgeTopAddr;
    uint64_t taskQueueElemsBase;
    uint64_t humogousReclaimCandidateBoolBase;
};

struct hwgc_dev
{
    void __iomem *mmio;
    struct pci_dev *pdev;
    dev_t devno;
    struct cdev cdev;
    struct class *cls;
    struct completion done;
};

/* 中断处理 */
static irqreturn_t hwgc_irq_handler(int irq, void *dev_id)
{
    struct hwgc_dev *hwgc = dev_id;
    u32 irq_status = ioread32(hwgc->mmio + REG_INT_STATUS);
    if (irq_status & IRQ_ALLOC_SLOW)
    {
        iowrite32(IRQ_ALLOC_SLOW, hwgc->mmio + REG_CLEAR_IRQ);
        // handler alloc_slow to user program
    }
    else if (irq_status & IRQ_ENQUEUE_FAILED)
    {
        iowrite32(IRQ_ENQUEUE_FAILED, hwgc->mmio + REG_CLEAR_IRQ);
        // handler enqueue failed to user program
    }
    else if (irq_status & IRQ_COMPLETE)
    {
        iowrite32(IRQ_COMPLETE, hwgc->mmio + REG_CLEAR_IRQ);
        complete(&hwgc->done);
    }
    return IRQ_HANDLED;
}

static ssize_t hwgc_write(struct file *file, const char __user *buf, size_t count, loff_t *ppos)
{
    struct hwgc_dev *hwgc = file->private_data;
    u32 offset = *ppos;
    printk("offset: %x\n", offset);

    void __iomem *addr = hwgc->mmio + offset;
    struct HWGCParameter hwgc_par;
    switch (offset)
    {
    case REG_PAR:
        if (count != sizeof(hwgc_par))
            return -EINVAL;
        if (copy_from_user(&hwgc_par, buf, count))
            return -EFAULT;
        writeq((u64)hwgc_par.chunkSize | (u64)hwgc_par.ageThreshold << 32, addr);
        writeq((u64)hwgc_par.heapRegionBias | (u64)hwgc_par.regionAttrShiftBy << 32, addr + 0x8);
        writeq((u64)hwgc_par.heapRegionShiftBy | (u64)hwgc_par.logOfHRGrainBytes << 32, addr + 0x10);
        writeq(hwgc_par.stepperOffset, addr + 0x18);
        writeq(hwgc_par.youngWordsBase, addr + 0x20);
        writeq(hwgc_par.regionAttrBase, addr + 0x28);
        writeq(hwgc_par.plabAllocatorPtr, addr + 0x30);
        writeq(hwgc_par.regionAttrBiasedBase, addr + 0x38);
        writeq(hwgc_par.heapRegionBiasedBase, addr + 0x40);
        writeq(hwgc_par.parScanThreadStatePtr, addr + 0x48);
        writeq(hwgc_par.taskQueueBottomAddr, addr + 0x50);
        writeq(hwgc_par.taskQueueAgeTopAddr, addr + 0x58);
        writeq(hwgc_par.taskQueueElemsBase, addr + 0x60);
        writeq(hwgc_par.humogousReclaimCandidateBoolBase, addr + 0x68);
        printk("1111\n");
        break;
    case REG_START_WORK:
        writel(HWGC_STATUS_IRQ, hwgc->mmio + REG_STATUS);
        writeq(1, addr);
        break;
    default:
        break;
    }
    *ppos += count;
    return count;
}

static ssize_t hwgc_read(struct file *file, char __user *buf, size_t count, loff_t *ppos)
{
    struct hwgc_dev *hwgc = file->private_data;
    u32 offset = *ppos;
    u64 val = ~0ULL;
    void __iomem *addr = hwgc->mmio + offset;
    switch (offset)
    {
    case REG_DEVICE_ID:
        val = readl(addr);
        break;

    default:
        break;
    }
    if (copy_to_user(buf, &val, count))
        return -EFAULT;

    *ppos += count;
    return count;
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
    .write = hwgc_write,
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

    // initial completion to identify interrupt done
    init_completion(&hwgc->done);
    hwgc->pdev = pdev;

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

static int __init hwgc_init(void)
{
    return pci_register_driver(&hwgc_pci_driver);
}

static void __exit hwgc_exit(void)
{
    pci_unregister_driver(&hwgc_pci_driver);
}

module_init(hwgc_init);
module_exit(hwgc_exit);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("QEMU hwgc driver with user-space test support");