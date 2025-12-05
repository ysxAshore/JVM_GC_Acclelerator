LOG_DIR="../log"
SCRIPT="./counter.sh"

for logfile in "$LOG_DIR"/*.log; do
  echo "Processing: $logfile"
  "$SCRIPT" "$logfile"
done
