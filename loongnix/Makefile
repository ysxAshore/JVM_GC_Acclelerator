obj-m += $(addsuffix .o, $(OBJ-M))

# assign KDIR determined by arch
MACHINE := $(shell uname -m)

ifeq ($(MACHINE),x86_64)
	KDIR := /home/sxyang/Projects/vm-env/busybox/linux-4.19.190.8.14
#	ARCH := loongarch
else
	KDIR := /lib/modules/$(shell uname -r)/build
#	ARCH :=
endif

#ARCH_FLAG := $(if $(ARCH),ARCH=$(ARCH))

PWD := $(shell pwd)

all:
	make -C $(KDIR) M=$(PWD) ARCH=loongarch modules

clean:
	make -C $(KDIR) M=$(PWD) ARCH=loongarch clean
