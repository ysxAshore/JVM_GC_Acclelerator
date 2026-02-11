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

#define SHADOW_SIZE 0x200000000
static void * shadow_base = NULL;

static void init_shadowChunks() {
    shadow_base = mmap((void *)0xfe00000000,
                   SHADOW_SIZE,
                   PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE,
                   -1, 0);
	if(shadow_base == MAP_FAILED){
		perror("mmap shadow 6GB");
		abort();
	}
}

static inline void *shadow_addr(uint64_t addr) {
    return (char *)addr;
}

static inline uint64_t load64(uintptr_t addr) {
    return *(volatile uint64_t *)shadow_addr(addr);
}

static inline void store64(uintptr_t addr, uint64_t val) {
	addr = (uintptr_t)shadow_addr(addr);
    *(volatile uint64_t *)addr = val;
}

static inline uint64_t load32(uintptr_t addr) {
    return *(volatile uint32_t *)shadow_addr(addr);
}

static inline void store32(uintptr_t addr, uint32_t val) {
	addr = (uintptr_t)shadow_addr(addr);
    *(volatile uint32_t *)addr = val;
}

static inline uint64_t load8(uintptr_t addr) {
    return *(volatile uint8_t *)shadow_addr(addr);
}

static inline void store8(uintptr_t addr, uint8_t val) {
	addr = (uintptr_t)shadow_addr(addr);
    *(volatile uint8_t *)addr = val;
}

struct HWGCParameters
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
  uint64_t g1h;
  uint64_t intArrayKlass;
  uint64_t objectKlass;
  uint64_t lockPtr;
  uint64_t thread;
  uint64_t dummyRegion;
  uint64_t numaPtr;
  uint64_t compressedOopBase;
  uint64_t compressedKlassPointerBase;
  uint8_t compressedOopShift;
  uint8_t compressedKlassPointerShift;
  uint8_t useCompressedOops;
  uint8_t useCompressedKlassPointers;
};

enum hwgc_state
{
  HWGC_IDLE,
  HWGC_RUNNING,
  HWGC_WAIT_MALLOC,
  HWGC_WAIT_ENQUEUED,
  HWGC_WAIT_PAGEFAULT,
  HWGC_DEBUG,
  HWGC_DONE
};
#define HWGC_IOC_MAGIC 'H'
#define HWGC_IOC_START _IOW(HWGC_IOC_MAGIC, 0, struct HWGCParameters)
#define HWGC_IOC_WAIT_EVENT _IOR(HWGC_IOC_MAGIC, 1, int)
#define HWGC_IOC_SOFT_PROVIDE _IOW(HWGC_IOC_MAGIC, 2, uint64_t)
#define HWGC_IOC_DEBUG_WRITE _IOW(HWGC_IOC_MAGIC, 3, uint64_t)
