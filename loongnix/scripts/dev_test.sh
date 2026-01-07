#!/bin/sh
sudo insmod hwgc_driver.ko
sudo rm -r *.log scratch
#sudo ./jdk17u/build/linux-loongarch64-server-slowdebug/jdk/bin/java -XX:-UseCompressedOops -XX:-UseCompressedClassPointers -XX:ParallelGCThreads=1 -XX:ConcGCThreads=0 -XX:+UseG1GC -jar renaissance-gpl-0.16.1.jar dotty
sudo ./build/linux-loongarch64-server-release/jdk/bin/java -XX:-UseCompressedOops -XX:-UseCompressedClassPointers -XX:ParallelGCThreads=1 -XX:ConcGCThreads=0 -XX:+UseG1GC -jar ./dacapo/dacapo-23.11-MR2-chopin.jar eclipse
#sudo ./build/linux-loongarch64-server-release/jdk/bin/java -XX:-UseCompressedOops -XX:-UseCompressedClassPointers -XX:ParallelGCThreads=1 -XX:ConcGCThreads=0 -XX:+UseG1GC G1GCDemo
