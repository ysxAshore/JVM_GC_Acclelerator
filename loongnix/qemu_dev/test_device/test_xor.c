/*
 * Userspace test for /dev/xor_tlbdev0.
 *
 * Build:
 *   gcc -O2 -Wall -o test_xor test_xor.c
 *
 * Run:
 *   sudo ./test_xor
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <string.h>

struct xor_tlb_op {
    uint64_t a;
    uint64_t b;
    uint64_t out;
    uint64_t result;
};

#define XOR_TLB_IOC_MAGIC 'x'
#define XOR_TLB_IOC_RUN   _IOWR(XOR_TLB_IOC_MAGIC, 1, struct xor_tlb_op)

int main(void)
{
    int fd;
    uint64_t *a, *b, *out;
    struct xor_tlb_op op;

    if (posix_memalign((void **)&a, 4096, 4096) ||
        posix_memalign((void **)&b, 4096, 4096) ||
        posix_memalign((void **)&out, 4096, 4096)) {
        perror("posix_memalign");
        return 1;
    }

    *a = 0x1122334455667788ULL;
    *b = 0xff00ff00aa55aa55ULL;
    *out = 0;

    fd = open("/dev/xor_tlbdev0", O_RDWR);
    if (fd < 0) {
        perror("open /dev/xor_tlbdev0");
        return 1;
    }

    memset(&op, 0, sizeof(op));
    op.a = (uint64_t)(uintptr_t)a;
    op.b = (uint64_t)(uintptr_t)b;
    op.out = (uint64_t)(uintptr_t)out;

    if (ioctl(fd, XOR_TLB_IOC_RUN, &op) < 0) {
        fprintf(stderr, "ioctl failed: %s\n", strerror(errno));
        return 1;
    }

    printf("a      = 0x%016llx\n", (unsigned long long)*a);
    printf("b      = 0x%016llx\n", (unsigned long long)*b);
    printf("out    = 0x%016llx\n", (unsigned long long)*out);
    printf("result = 0x%016llx\n", (unsigned long long)op.result);
    printf("expect = 0x%016llx\n", (unsigned long long)(*a ^ *b));

    close(fd);
    free(a);
    free(b);
    free(out);

    return (*out == (*a ^ *b)) ? 0 : 2;
}
