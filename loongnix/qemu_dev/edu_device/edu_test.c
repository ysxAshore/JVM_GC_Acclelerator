// test_edu.c
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <stdint.h>

int main()
{
    int fd = open("/dev/edu", O_RDWR);
    if (fd < 0) {
        perror("open /dev/edu");
        return 1;
    }

    uint32_t n = 5;
    printf("Computing %u! ...\n", n);

    if (ioctl(fd, 0, &n) < 0) {
        perror("ioctl");
        close(fd);
        return 1;
    }

    printf("%u! = %u\n", 5, n);

    close(fd);
    return 0;
}
