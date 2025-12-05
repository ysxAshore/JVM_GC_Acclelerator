#!/bin/bash
file=$1

echo "dispatch task：$(grep -c 'dispatch task' "$file")"
echo "update card：$(grep -c 'update last_enqueued_card' "$file")"
echo "index == 0：$(grep -c 'index == 0' "$file")"
echo "enqueue malloc：$(grep -c 'run malloc' "$file")"
echo "allocate slow：$(grep -c 'do allocate copy slow' "$file")"
echo "mutex locker：$(grep -c 'mutex locker' "$file")"
echo "expand region：$(grep -c 'expand_single_region' "$file")"
echo "expand：$(grep -c 'expand' "$file")"
echo "expand malloc：$(grep -c 'expand new malloc' "$file")"
echo "grow array：$(grep -c 'len max' "$file")"
