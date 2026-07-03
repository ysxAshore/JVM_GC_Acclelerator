#define _GNU_SOURCE
#include <sys/mman.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <linux/ioctl.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>

#define SHADOW_SIZE 0x200000000ULL
#define SHADOW_VA_BASE 0xfe00000000ULL

static void *shadow_base = NULL;

static void init_shadowChunks()
{
    void *requested = (void *)(uintptr_t)SHADOW_VA_BASE;
    int flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;

    shadow_base = mmap(requested,
                       SHADOW_SIZE,
                       PROT_READ | PROT_WRITE,
                       flags,
                       -1,
					   0);

    if (shadow_base == MAP_FAILED)
    {
        perror("mmap shadow 6GB");
        abort();
    }
}

static inline void *shadow_addr(uint64_t addr)
{
    uint64_t offset;

    if (addr < SHADOW_VA_BASE) {
        fprintf(stderr, "shadow address below range: 0x%llx\n",
                (unsigned long long)addr);
        abort();
    }

    offset = addr - SHADOW_VA_BASE;
    if (offset >= SHADOW_SIZE) {
        fprintf(stderr, "shadow address outside range: 0x%llx\n",
                (unsigned long long)addr);
        abort();
    }

    /*
     * 只有确认 mmap 返回 SHADOW_VA_BASE 后，
     * 恒等地址转换才成立。
     */
    return (void *)(uintptr_t)addr;
}

static inline uint64_t load64(uintptr_t addr)
{
    return *(volatile uint64_t *)shadow_addr(addr);
}

static inline void store64(uintptr_t addr, uint64_t val)
{
    addr = (uintptr_t)shadow_addr(addr);
    *(volatile uint64_t *)addr = val;
}

static inline uint64_t load32(uintptr_t addr)
{
    return *(volatile uint32_t *)shadow_addr(addr);
}

static inline void store32(uintptr_t addr, uint32_t val)
{
    addr = (uintptr_t)shadow_addr(addr);
    *(volatile uint32_t *)addr = val;
}

static inline uint64_t load16(uintptr_t addr)
{
    return *(volatile uint16_t *)shadow_addr(addr);
}

static inline void store16(uintptr_t addr, uint8_t val)
{
    addr = (uintptr_t)shadow_addr(addr);
    *(volatile uint16_t *)addr = val;
}

static inline uint64_t load8(uintptr_t addr)
{
    return *(volatile uint8_t *)shadow_addr(addr);
}

static inline void store8(uintptr_t addr, uint8_t val)
{
    addr = (uintptr_t)shadow_addr(addr);
    *(volatile uint8_t *)addr = val;
}

struct HWGCParameters
{
    uint32_t chunkSize;
    uint32_t ageThreshold;
    uint32_t heapRegionBias;
    uint32_t heapRegionShiftBy;
    uint32_t regionAttrShiftBy;
    uint32_t logOfHRGrainBytes;
    uint64_t stepperOffset;
    uint64_t youngWordsBase;
    uint64_t regionAttrBase;
    uint64_t plabAllocatorPtr;
    uint64_t regionAttrBiasedBase;
    uint64_t heapRegionBiasedBase;
    uint64_t parScanThreadStatePtr;
    uint32_t localBot;
    uint64_t taskQueueElemsBase;
    uint64_t humogousReclaimCandidateBoolBase;
    uint64_t cardTablePtr;
    uint64_t g1h;
    uint64_t intArrayKlass;
    uint64_t objectKlass;
    uint64_t lockPtr;
    uint64_t thread;
    uint64_t dummyRegion;
    uint64_t compressedOopBase;
    uint64_t compressedKlassPointerBase;
    uint8_t compressedOopShift;
    uint8_t compressedKlassPointerShift;
    uint8_t useCompressedOops;
    uint8_t useCompressedKlassPointers;
};

#define HWGC_EVENT_NONE 0
#define HWGC_EVENT_DONE 1
#define HWGC_EVENT_GROW 2
#define HWGC_EVENT_EXPAND 3
#define HWGC_EVENT_ALLOCATE 4
#define HWGC_EVENT_WAKE 5
#define HWGC_EVENT_ERROR 6

struct HWGCEvent
{
    uint32_t type;
    uint32_t reserved;
    uint64_t seq;
    uint64_t par0;
    uint64_t par1;
    uint64_t res0;
    uint64_t res1;
};
#define HWGC_IOC_MAGIC 'x'
#define HWGC_IOC_START _IOWR(HWGC_IOC_MAGIC, 1, struct HWGCParameters)
#define HWGC_IOC_WAIT_EVENT _IOR(HWGC_IOC_MAGIC, 2, struct HWGCEvent)
#define HWGC_IOC_REPLY_EVENT _IOW(HWGC_IOC_MAGIC, 3, struct HWGCEvent)
