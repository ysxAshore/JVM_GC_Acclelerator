#!/bin/sh
sudo insmod ../qemu_dev/hwgc_driver/hwgc_driver.ko

dacapo_list="h2 h2o jme jython kafka luindex lusearch pmd spring sunflow tomcat xalan zxing"

for test in $dacapo_list; do
   export JAVA_TOOL_OPTIONS="-XX:+PrintGCDetails -XX:-UseCompressedOops -XX:-UseCompressedClassPointers -XX:ParallelGCThreads=1 -XX:ConcGCThreads=1 -XX:+UseG1GC"
   sudo rm -rf scratch
   echo "run hwgc ${test}"
   sudo bash -c "../jdk/build/linux-loongarch64-server-release/jdk/bin/java -XX:+PrintGCDetails -XX:-UseCompressedOops -XX:-UseCompressedClassPointers -XX:ParallelGCThreads=1 -XX:ConcGCThreads=1 -XX:+UseG1GC -jar ../benchmark/dacapo/dacapo-23.11-MR2-chopin.jar ${test} > ${test}.log"
   sudo rm -rf scratch
   echo "run ref ${test}"
   ./../jdk/jdk17u/build/linux-loongarch64-server-release/jdk/bin/java  -jar ../benchmark/dacapo/dacapo-23.11-MR2-chopin.jar ${test} > ${test}_ref.log
done
