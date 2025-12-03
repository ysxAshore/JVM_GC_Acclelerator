#!/bin/bash
file=$1

echo "dispatch task: $(grep -c 'dispatch task' "$file")"
echo "allocate copy slow: $(grep -c 'do allocate copy slow' "$file")"
echo "mutex locker: $(grep -c 'mutex locker' "$file")"
echo "index == 0:       $(grep -c 'index == 0' "$file")"
echo "run malloc:     $(grep -c 'run malloc' "$file")"
