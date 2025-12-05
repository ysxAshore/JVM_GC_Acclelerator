#!/usr/bin/env bash
set -euo pipefail

############################ 用户可调参数 ############################
JAVA=" ../../jdk17u/build/linux-x86_64-server-release/jdk/bin/java"
RENAISSANCE="../renaissance/renaissance-gpl-0.16.1.jar"
DACAPO="../dacapo/dacapo-23.11-MR2-chopin.jar"
LOG="../log/failure.log"
#####################################################################

# 清空旧日志
> "$LOG"

# 颜色
RED='\e[31m'; GREEN='\e[32m'; RESET='\e[0m'

# 统一跑单个 benchmark 的函数
# $1 = 套件名  $2 = benchmark名
run_bench() {
    local suite=$1 name=$2
    printf "%-30s" "${suite}::${name} "
	local macro=""
    [[ $b == "cassandra" ]] && macro="-Djava.security.manager=allow"

 #   if $JAVA "-Xlog:gc+task=info" $macro -jar "${suite}" "${name}" > "../log/${name}.log" 2> "../log/${name}.log";  then
    if $JAVA $macro -jar "${suite}" "${name}" > "log" 2> "log";  then
        printf "${GREEN}OK${RESET}\n"
    else
        local code=$?
        printf "${RED}FAIL${RESET} (exit=$code)\n"
    fi
}

# DaCapo 全部
# tradesoap tradebeans
echo ">>>> Running DaCapo benchmarks"
for b in $($JAVA -jar "$DACAPO" --list); do
   export JAVA_TOOL_OPTIONS="-XX:-UseCompressedOops -XX:-UseCompressedClassPointers -XX:ParallelGCThreads=1 -XX:ConcGCThreads=1"
   run_bench "$DACAPO" "$b"
done

# Renaissance 全部
# db-shootout neo4j-analytics reactors naive-bayes scala-stm-bench7
echo ">>>> Running Renaissance benchmarks"
renaissance_list="scrabble page-rank future-genetic akka-uct movie-lens scala-doku chi-square fj-kmeans rx-scrabble db-shootout neo4j-analytics finagle-http reactors dec-tree scala-stm-bench7 naive-bayes als par-mnemonics scala-kmeans philosophers log-regression gauss-mix mnemonics dotty finagle-chirper"
for b in $renaissance_list; do
    run_bench "$RENAISSANCE" "$b"
done
rm log
rm -rf harness*
rm -rf scratch
