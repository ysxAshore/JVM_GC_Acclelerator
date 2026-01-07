#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>

#define DEVICE_PATH "/dev/test"

int main(void)
{
    int fd;
    ssize_t ret;

    /* 打开设备 */
    fd = open(DEVICE_PATH, O_RDWR);
    if (fd < 0)
    {
        perror("Failed to open " DEVICE_PATH);
        return EXIT_FAILURE;
    }

    printf("Opened %s\n", DEVICE_PATH);

    /* check access va */
    printf("Check access Va\n");
    off_t offset = 0x8; // 寄存器偏移地址
    uint64_t write_val = 0x13579bdf;
    uintptr_t val_va = (uintptr_t)&write_val;
    if (lseek(fd, offset, SEEK_SET) == (off_t)-1)
    {
        perror("lseek failed");
        close(fd);
        return EXIT_FAILURE;
    }
    ret = write(fd, &val_va, 8);
    if (ret != 8)
    {
        perror("write failed");
        close(fd);
        return EXIT_FAILURE;
    }
    printf("Wrote 0x%02lX to register offset 0x%lX\n", val_va, offset);
    if (lseek(fd, offset, SEEK_SET) == (off_t)-1)
    {
        perror("lseek failed");
        close(fd);
        return EXIT_FAILURE;
    }

    uint64_t read_val;
    ret = read(fd, &read_val, 8);
    if (ret != 8)
    {
        perror("read failed");
        close(fd);
        return EXIT_FAILURE;
    }
    printf("Read 0x%02lX from register offset 0x%lX\n", read_val, offset);
    if (read_val == write_val)
    {
        printf("✅ Access VA Passed\n");
    }
    else
    {
        printf("❌ Access VA FAILED: Expected 0x%02X, got 0x%02X\n", write_val, read_val);
    }

    /* 写：先 lseek 到偏移，再写 1 字节 */
    offset = 0x10;
    if (lseek(fd, offset, SEEK_SET) == (off_t)-1)
    {
        perror("lseek failed");
        close(fd);
        return EXIT_FAILURE;
    }

    ret = write(fd, &write_val, 1);
    if (ret != 1)
    {
        perror("write failed");
        close(fd);
        return EXIT_FAILURE;
    }
    printf("Wrote 0x%02X to register offset 0x%lX\n", write_val, offset);

    /* 读：lseek 到同一偏移，读 1 字节 */
    if (lseek(fd, offset, SEEK_SET) == (off_t)-1)
    {
        perror("lseek failed");
        close(fd);
        return EXIT_FAILURE;
    }

    ret = read(fd, &read_val, 1);
    if (ret != 1)
    {
        perror("read failed");
        close(fd);
        return EXIT_FAILURE;
    }

    printf("Read 0x%02X from register offset 0x%lX\n", (uint8_t)read_val, offset);

    /* 验证 */
    if ((uint8_t)read_val == (uint8_t)write_val)
    {
        printf("✅ Test PASSED: Read value matches written value.\n");
    }
    else
    {
        printf("❌ Test FAILED: Expected 0x%02X, got 0x%02X\n", (uint8_t)write_val, (uint8_t)read_val);
    }

    close(fd);
    return EXIT_SUCCESS;
}
