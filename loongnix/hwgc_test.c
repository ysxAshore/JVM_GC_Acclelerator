// hwgc_test.c
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

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

#define REG_DEVICE_ID 0x0
#define REG_PAR 0x10
#define REG_START_WORK 0x80

// 辅助函数：安全地执行 lseek 并验证
off_t safe_lseek(int fd, off_t offset, const char *reg_name)
{
    off_t pos = lseek(fd, offset, SEEK_SET);
    if (pos == (off_t)-1)
    {
        perror("lseek failed");
        return -1;
    }
    if (pos != offset)
    {
        fprintf(stderr, "lseek to %s (0x%lx) returned unexpected position: 0x%lx\n",
                reg_name, (unsigned long)offset, (unsigned long)pos);
        return -1;
    }
    printf("✅ Successfully seeked to %s (offset=0x%lx)\n", reg_name, (unsigned long)pos);
    return pos;
}

int main(void)
{
    int fd = open("/dev/hwgc", O_RDWR);
    if (fd < 0)
    {
        perror("Failed to open /dev/hwgc");
        return EXIT_FAILURE;
    }

    // 1. 读取设备 ID
    printf("Reading device ID...\n");
    if (safe_lseek(fd, REG_DEVICE_ID, "REG_DEVICE_ID") == (off_t)-1)
    {
        close(fd);
        return EXIT_FAILURE;
    }

    uint32_t dev_id = 0;
    ssize_t ret = read(fd, &dev_id, sizeof(dev_id));
    if (ret != sizeof(dev_id))
    {
        perror("Read device ID failed");
        close(fd);
        return EXIT_FAILURE;
    }
    printf("Device ID: 0x%08x\n", dev_id);

    // 2. 构造测试参数
    struct HWGCParameter par = {0};
    par.chunkSize = 1024;
    par.ageThreshold = 15;
    par.heapRegionBias = 0x100000;
    par.regionAttrShiftBy = 20;
    par.heapRegionShiftBy = 21;
    par.logOfHRGrainBytes = 20;

    // 3. 写入参数到 REG_PAR
    printf("\nWriting HWGC parameters...\n");
    if (safe_lseek(fd, REG_PAR, "REG_PAR") == (off_t)-1)
    {
        close(fd);
        return EXIT_FAILURE;
    }

    ret = write(fd, &par, sizeof(par));
    if (ret != sizeof(par))
    {
        perror("Write parameters failed");
        close(fd);
        return EXIT_FAILURE;
    }
    printf("✅ Parameters written successfully.\n");

    // 4. 触发硬件开始工作
    printf("\nStarting HWGC work...\n");
    if (safe_lseek(fd, REG_START_WORK, "REG_START_WORK") == (off_t)-1)
    {
        close(fd);
        return EXIT_FAILURE;
    }

    uint64_t start_val = 1;
    ret = write(fd, &start_val, sizeof(start_val));
    if (ret != sizeof(start_val))
    {
        perror("Start work failed");
        close(fd);
        return EXIT_FAILURE;
    }
    printf("✅ Work started.\n");

    close(fd);
    printf("\nTest completed.\n");
    return EXIT_SUCCESS;
}