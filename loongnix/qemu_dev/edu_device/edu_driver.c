// edu_driver.c
#include <linux/module.h>
#include <linux/pci.h>
#include <linux/io.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/uaccess.h>
#include <linux/interrupt.h>
#include <linux/wait.h>
#include <linux/sched.h>

#define REG_FACT 0x08
#define REG_STATUS 0x20
#define REG_INT_STATUS 0x24
#define REG_CLEAR_IRQ 0x64

#define IRQ_FACT 0x1

#define STATUS_COMPUTING BIT(0)
#define STATUS_IRQFACT BIT(7)

#define EDU_DEV_NAME "edu"

struct edu_dev
{
    void __iomem *mmio;
    struct pci_dev *pdev;
    dev_t devno;
    struct cdev cdev;
    struct class *cls;
    struct completion fact_done;
    u32 fact_result;
};

/* 中断处理 */
static irqreturn_t edu_irq_handler(int irq, void *dev_id)
{
    struct edu_dev *edu = dev_id;
    iowrite32(IRQ_FACT, edu->mmio + REG_CLEAR_IRQ);
    edu->fact_result = ioread32(edu->mmio + REG_FACT);
    complete(&edu->fact_done);
    return IRQ_HANDLED;
}

static long edu_ioctl(struct file *file, unsigned int cmd, unsigned long arg)
{
    struct edu_dev *edu = file->private_data;
    u32 val;

    switch (cmd)
    {
    case 0:
        if (copy_from_user(&val, (void __user *)arg, sizeof(val)))
            return -EFAULT;

        reinit_completion(&edu->fact_done);

        iowrite32(STATUS_IRQFACT, edu->mmio + REG_STATUS);
        iowrite32(val, edu->mmio + REG_FACT);

        // wait fact_down complete
        if (!wait_for_completion_timeout(&edu->fact_done, HZ))
        {
            pr_err("edu: factorial timeout!\n");
            return -ETIMEDOUT;
        }

        if (copy_to_user((void __user *)arg, &edu->fact_result, sizeof(edu->fact_result)))
            return -EFAULT;
        break;

    default:
        return -ENOTTY;
    }
    return 0;
}

static int edu_open(struct inode *inode, struct file *file)
{
    struct edu_dev *edu = container_of(inode->i_cdev, struct edu_dev, cdev);
    file->private_data = edu;
    return 0;
}

static const struct file_operations edu_fops = {
    .owner = THIS_MODULE,
    .open = edu_open,
    .unlocked_ioctl = edu_ioctl,
};

/* PCI probe */
static int edu_probe(struct pci_dev *pdev, const struct pci_device_id *id)
{
    struct edu_dev *edu;
    int ret;

    edu = devm_kzalloc(&pdev->dev, sizeof(*edu), GFP_KERNEL);
    if (!edu)
        return -ENOMEM;

    // initial completion to identify interrupt done
    init_completion(&edu->fact_done);
    edu->pdev = pdev;

    // enable pci/pcie device
    ret = pcim_enable_device(pdev);
    if (ret)
        return ret;

    // register and map pci/pcie bar0 memory region
    ret = pcim_iomap_regions(pdev, BIT(0), EDU_DEV_NAME);
    if (ret)
        return ret;
    edu->mmio = pcim_iomap_table(pdev)[0]; // get the bar0 vitual addr

    // ret = pci_enable_msi(pdev);
    // if (ret) {
    //     dev_err(&pdev->dev, "Failed to enable MSI (ret=%d), trying INTx\n", ret);
    // } else {
    //     dev_info(&pdev->dev, "MSI enabled successfully\n");
    // }
    ret = devm_request_irq(&pdev->dev, pdev->irq, edu_irq_handler, 0, EDU_DEV_NAME, edu);
    if (ret)
    {
        dev_err(&pdev->dev, "request_irq failed: %d\n", ret);
        return ret;
    }
    else
        dev_info(&pdev->dev, "request irq successfully\n");

    // 注册字符设备
    ret = alloc_chrdev_region(&edu->devno, 0, 1, EDU_DEV_NAME);
    if (ret)
    {
        dev_err(&pdev->dev, "Failed to allocate chrdev\n");
        return ret;
    }

    cdev_init(&edu->cdev, &edu_fops);
    edu->cdev.owner = THIS_MODULE;
    ret = cdev_add(&edu->cdev, edu->devno, 1);
    if (ret)
    {
        dev_err(&pdev->dev, "Failed to add cdev\n");
        goto err_unregister;
    }

    edu->cls = class_create(THIS_MODULE, EDU_DEV_NAME);
    if (IS_ERR(edu->cls))
    {
        ret = PTR_ERR(edu->cls);
        goto err_cdev_del;
    }

    if (IS_ERR(device_create(edu->cls, &pdev->dev, edu->devno, NULL, EDU_DEV_NAME)))
    {
        dev_err(&pdev->dev, "Failed to create /dev node\n");
        ret = -ENOMEM;
        goto err_class_destroy;
    }

    pci_set_drvdata(pdev, edu);
    dev_info(&pdev->dev, "edu: device probed at /dev/%s\n", EDU_DEV_NAME);
    return 0;

err_class_destroy:
    class_destroy(edu->cls);
err_cdev_del:
    cdev_del(&edu->cdev);
err_unregister:
    unregister_chrdev_region(edu->devno, 1);
    return ret;
}

static void edu_remove(struct pci_dev *pdev)
{
    struct edu_dev *edu = pci_get_drvdata(pdev);
    device_destroy(edu->cls, edu->devno);
    class_destroy(edu->cls);
    cdev_del(&edu->cdev);
    unregister_chrdev_region(edu->devno, 1);
    dev_info(&pdev->dev, "edu: device removed\n");
}

static const struct pci_device_id edu_pci_ids[] = {
    {PCI_DEVICE(0x1234, 0x11e8)},
    {},
};
MODULE_DEVICE_TABLE(pci, edu_pci_ids);

static struct pci_driver edu_pci_driver = {
    .name = EDU_DEV_NAME,
    .id_table = edu_pci_ids,
    .probe = edu_probe,
    .remove = edu_remove,
};

static int __init edu_init(void)
{
    return pci_register_driver(&edu_pci_driver);
}

static void __exit edu_exit(void)
{
    pci_unregister_driver(&edu_pci_driver);
}

module_init(edu_init);
module_exit(edu_exit);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("QEMU edu driver with user-space test support");
