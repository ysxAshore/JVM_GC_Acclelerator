// hwgc_test.c
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/ioctl.h>
#include "hwgc_ioctl.h"
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
    uint64_t taskQueueElemsBase;
    uint64_t humogousReclaimCandidateBoolBase;
    uint64_t cardTablePtr;
};
int main(void)
{
    int fd = open("/dev/hwgc", O_RDWR);
    if (fd < 0)
    {
        perror("Failed to open /dev/hwgc");
        return EXIT_FAILURE;
    }

    if (lseek(fd, REG_DEVICE_ID, SEEK_SET) == (off_t)-1)
        goto lseek_failed;

    uint32_t dev_id = 0;
    ssize_t ret = read(fd, &dev_id, sizeof(dev_id));
    if (ret != sizeof(dev_id))
        goto read_failed;
    printf("Device ID: 0x%08x\n", dev_id);

    struct HWGCParameter par = {0};
    uint localBot = 0;
    par.taskQueueBottomAddr = (uintptr_t)&localBot;

    int state;
    do
    {
        ssize_t ret = read(fd, &state, sizeof(state));
        if (ret != sizeof(state))
            goto read_failed;
    } while (state != HWGC_IDLE);

    printf("device driver is free, can start new work\n");
    ioctl(fd, HWGC_IOC_START, &par);

    while (1)
    {
        ioctl(fd, HWGC_IOC_WAIT_EVENT, &state);
        if (state == HWGC_DONE)
        {
            printf("the work is done\n");
            break;
        }
        if (state == HWGC_WAIT_MALLOC)
        {
            printf("do malloc\n");
            if (lseek(fd, REG_SOFT_PAR0, SEEK_SET) == (off_t)-1)
                goto lseek_failed;
            uint64_t par0, par1;
            read(fd, &par0, sizeof(par0));
            read(fd, &par1, sizeof(par1));
            uint64_t res = par0 + par1;
            printf("%lld %lld soft res is %lx\n", par0, par1, res);
            ioctl(fd, HWGC_IOC_SOFT_PROVIDE, &res);
        }
    }

    close(fd);
    printf("Test completed\n");
    return EXIT_SUCCESS;
lseek_failed:
    perror("lseek failed");
    close(fd);
    return EXIT_FAILURE;
read_failed:
    perror("read failed");
    close(fd);
    return EXIT_FAILURE;
}