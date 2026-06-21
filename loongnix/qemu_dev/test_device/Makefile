#sudo ./jdk17u/build/linux-loongarch64-server-release/jdk/bin/java -XX:-UseCompressedOops -XX:-UseCompressedClassPointers -XX:ParallelGCThreads=1 -XX:ConcGCThreads=0 -XX:+UseG1GC G1GCDemo 
obj-m += xor_tlbdev_drv.o

# assign KDIR determined by arch
MACHINE := $(shell uname -m)

ifeq ($(MACHINE),x86_64)
	KDIR := ../../../../vm-env/busybox/linux-4.19.190.8.14
else
	KDIR := /lib/modules/$(shell uname -r)/build
endif

PWD := $(shell pwd)

all:
	make -C $(KDIR) M=$(PWD) ARCH=loongarch modules

clean:
	make -C $(KDIR) M=$(PWD) ARCH=loongarch clean
