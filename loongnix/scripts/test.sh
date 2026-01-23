sudo insmod ../qemu_dev/hwgc_driver/hwgc_driver.ko
sudo rm -rf scratch hs*
sudo bash -c "../jdk/build_debug/linux-loongarch64-server-release/jdk/bin/java -XX:+PrintGCDetails -XX:-UseCompressedOops -XX:-UseCompressedClassPointers -XX:ParallelGCThreads=1 -XX:ConcGCThreads=1 -XX:+UseG1GC -jar ../benchmark/dacapo/dacapo-23.11-MR2-chopin.jar avrora"
