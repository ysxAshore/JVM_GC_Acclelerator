sudo insmod ../qemu_dev/hwgc_driver/hwgc_driver.ko
sudo rm -rf scratch hs*
sudo bash -c "../jdk/build/linux-loongarch64-server-release/jdk/bin/java -Xlog:gc+task -XX:+UseG1GC -jar ../test/benchmark/dacapo/dacapo-23.11-MR2-chopin.jar avrora"
