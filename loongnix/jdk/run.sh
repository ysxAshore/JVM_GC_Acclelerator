#!/bin/bash
suite=(
rx-scrabble
finagle-http
dec-tree
scala-stm-bench7
naive-bayes
)
for i in "${suite[@]}"; do
    sudo ./build_multi/linux-loongarch64-server-release/jdk/bin/java -XX:+PrintGCDetails -XX:+UseCompressedOops -XX:+UseCompressedClassPointers -XX:ParallelGCThreads=2 -XX:ConcGCThreads=2 -XX:+UseG1GC -jar ../test/benchmark/renaissance-gpl-0.16.1.jar -c test $i;
    echo "$? $i success" >> log
done
