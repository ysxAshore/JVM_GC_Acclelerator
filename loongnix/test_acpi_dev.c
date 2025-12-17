#include <linux/module.h>
#include <linux/platform_device.h>
#include <linux/acpi.h>
#include <linux/io.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/device.h>
#include <linux/uaccess.h>

#define TEST_DEV_NAME "test"
#define DEVICE_NAME "TESTDEV0"

struct test_acpi_dev
{
    void __iomem *base; // MMIO 映射基地址
    struct device *dev;
    dev_t devt;
    struct cdev cdev;
    struct class *cls;
};

/* 寄存器字节读 */
static u8 test_reg_read(struct test_acpi_dev *test_dev, u32 offset)
{
    return readb(test_dev->base + offset);
}

/* 寄存器字节写 */
static void test_reg_write(struct test_acpi_dev *test_dev, u32 offset, u8 value)
{
    writeb(value, test_dev->base + offset);
}

/* 字符设备 read：读 1 字节寄存器 */
static ssize_t test_dev_read(struct file *file, char __user *buf,
                             size_t count, loff_t *ppos)
{
    struct test_acpi_dev *test_dev = file->private_data;
    u32 reg_offset = *ppos;
    u8 val;

    if (count != 1)
        return -EINVAL;

    val = test_reg_read(test_dev, reg_offset);

    if (copy_to_user(buf, &val, 1))
        return -EFAULT;

    *ppos += 1;
    return 1;
}

static ssize_t test_dev_write(struct file *file, const char __user *buf,
                              size_t count, loff_t *ppos)
{
    struct test_acpi_dev *test_dev = file->private_data;
    u32 reg_offset = *ppos;
    u8 val;

    if (count != 1)
        return -EINVAL;

    if (copy_from_user(&val, buf, 1))
        return -EFAULT;

    test_reg_write(test_dev, reg_offset, val);
    *ppos += 1;
    return 1;
}

static int test_dev_open(struct inode *inode, struct file *file)
{
    struct test_acpi_dev *test_dev = container_of(inode->i_cdev,
                                                  struct test_acpi_dev, cdev);
    file->private_data = test_dev;
    return 0;
}

static const struct file_operations test_fops = {
    .owner = THIS_MODULE,
    .open = test_dev_open,
    .read = test_dev_read,
    .write = test_dev_write,
    .llseek = default_llseek,
};

/* ACPI 匹配表 */
static const struct acpi_device_id test_acpi_match[] = {
    {"TESTDEV0"},
    {}};
MODULE_DEVICE_TABLE(acpi, test_acpi_match);

/* Probe：设备发现时调用 */
static int test_acpi_probe(struct platform_device *pdev)
{
    struct test_acpi_dev *test_dev;
    struct resource *res;
    int ret;

    dev_info(&pdev->dev, "Probing ACPI device: %s\n", DEVICE_NAME);

    test_dev = devm_kzalloc(&pdev->dev, sizeof(*test_dev), GFP_KERNEL);
    if (!test_dev)
        return -ENOMEM;

    test_dev->dev = &pdev->dev;

    /* 从 ACPI _CRS 获取 MMIO 资源 */
    res = platform_get_resource(pdev, IORESOURCE_MEM, 0);
    if (!res)
    {
        dev_err(&pdev->dev, "No MMIO resource in _CRS\n");
        return -EINVAL;
    }

    dev_info(&pdev->dev, "MMIO: %pR\n", res);

    /* 映射非缓存 MMIO 区域 */
    test_dev->base = devm_ioremap_resource(&pdev->dev, res);
    if (IS_ERR(test_dev->base))
    {
        return PTR_ERR(test_dev->base);
    }

    /* 注册字符设备 */
    ret = alloc_chrdev_region(&test_dev->devt, 0, 1, TEST_DEV_NAME);
    if (ret)
    {
        dev_err(&pdev->dev, "Failed to allocate chrdev\n");
        return ret;
    }

    cdev_init(&test_dev->cdev, &test_fops);
    test_dev->cdev.owner = THIS_MODULE;
    ret = cdev_add(&test_dev->cdev, test_dev->devt, 1);
    if (ret)
    {
        dev_err(&pdev->dev, "Failed to add cdev\n");
        goto err_unregister;
    }

    test_dev->cls = class_create(THIS_MODULE, TEST_DEV_NAME);
    if (IS_ERR(test_dev->cls))
    {
        ret = PTR_ERR(test_dev->cls);
        goto err_cdev_del;
    }

    if (IS_ERR(device_create(test_dev->cls, &pdev->dev, test_dev->devt, NULL, TEST_DEV_NAME)))
    {
        dev_err(&pdev->dev, "Failed to create /dev node\n");
        ret = -ENOMEM;
        goto err_class_destroy;
    }

    platform_set_drvdata(pdev, test_dev);
    dev_info(&pdev->dev, "Driver ready: /dev/%s\n", TEST_DEV_NAME);
    return 0;

err_class_destroy:
    class_destroy(test_dev->cls);
err_cdev_del:
    cdev_del(&test_dev->cdev);
err_unregister:
    unregister_chrdev_region(test_dev->devt, 1);
    return ret;
}

/* Remove：设备移除时调用 */
static int test_acpi_remove(struct platform_device *pdev)
{
    struct test_acpi_dev *test_dev = platform_get_drvdata(pdev);

    device_destroy(test_dev->cls, test_dev->devt);
    class_destroy(test_dev->cls);
    cdev_del(&test_dev->cdev);
    unregister_chrdev_region(test_dev->devt, 1);

    dev_info(&pdev->dev, "Driver removed\n");
    return 0;
}

/* Platform driver 注册 */
static struct platform_driver test_acpi_driver = {
    .probe = test_acpi_probe,
    .remove = test_acpi_remove,
    .driver = {
        .name = TEST_DEV_NAME,
        .acpi_match_table = ACPI_PTR(test_acpi_match),
    },
};

module_platform_driver(test_acpi_driver);

MODULE_AUTHOR("ysxAshore");
MODULE_DESCRIPTION("ACPI TESTDEV0 Driver");
MODULE_LICENSE("GPL");