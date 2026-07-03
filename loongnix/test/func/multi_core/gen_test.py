#!/usr/bin/env python3
import argparse
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Sequence, TextIO, Tuple


WORKER_ID_RE = re.compile(r"\bworker_id\s+(\d+)\b")
ACCESS_RE = re.compile(
    r"\baccess\s+([0-9a-fA-F]+)\s+\((\d+)\s+bytes\)\s+to\s+"
    r"(get|write)(?:\s+([0-9a-fA-F]+))?"
)
PAR_ASSIGN_RE = re.compile(
    r"\bpar\.(\w+)\s*=\s*(0x[0-9a-fA-F]+|\d+)\s*"
)


@dataclass
class WorkerTrace:
    worker_id: int
    par_fields: Dict[str, str] = field(default_factory=dict)
    access_count: int = 0


@dataclass
class TraceData:
    workers: Dict[int, WorkerTrace] = field(default_factory=dict)
    memory_restore: List[str] = field(default_factory=list)
    unscoped_accesses: int = 0
    unscoped_par_assignments: int = 0


def gen_store(addr: str, size: int, value: str, use_shadow: bool) -> str:
    if use_shadow:
        return (
            f"store{size * 8}(0x{addr}, "
            f"(uintptr_t)shadow_addr(0x{value}));"
        )
    return f"store{size * 8}(0x{addr}, 0x{value});"


def gen_map(addr: str) -> str:
    return f"(void)shadow_addr(0x{addr});"


def need_shadow(process_par: bool, field_name: str, value: str) -> bool:
    if process_par:
        if not value.startswith("0x"):
            return False
        if field_name.startswith("compressed"):
            return False
        return True

    # Keep the original trace format's pointer heuristic:
    # 40-bit values beginning with 'f' are VM addresses that must be remapped.
    normalized = value.lower()
    return len(normalized) == 10 and normalized.startswith("f")


def worker_for_line(
    data: TraceData, worker_id: Optional[int], is_par: bool
) -> WorkerTrace:
    # Backward compatibility for old single-worker traces without worker_id.
    if worker_id is None:
        worker_id = 0
        if is_par:
            data.unscoped_par_assignments += 1
        else:
            data.unscoped_accesses += 1

    return data.workers.setdefault(worker_id, WorkerTrace(worker_id))


def parse_trace(trace_path: Path) -> TraceData:
    data = TraceData()
    seen_addrs = set()

    with trace_path.open("r", encoding="utf-8", errors="replace") as trace:
        for line_no, line in enumerate(trace, 1):
            worker_match = WORKER_ID_RE.search(line)
            worker_id = int(worker_match.group(1)) if worker_match else None

            access_match = ACCESS_RE.search(line)
            if access_match:
                addr, size_text, op, value = access_match.groups()
                worker = worker_for_line(data, worker_id, is_par=False)
                worker.access_count += 1

                # Initial memory is determined by the first access to an address.
                # If that first access is a write, only map it; a later get observes
                # runtime-mutated data and must not be restored as initial state.
                if addr not in seen_addrs:
                    seen_addrs.add(addr)
                    size = int(size_text)

                    if op == "get":
                        if value is None:
                            raise ValueError(
                                f"{trace_path}:{line_no}: get access has no value"
                            )
                        data.memory_restore.append(
                            gen_store(
                                addr,
                                size,
                                value,
                                need_shadow(False, "", value),
                            )
                        )
                    else:
                        data.memory_restore.append(gen_map(addr))
                continue

            par_match = PAR_ASSIGN_RE.search(line)
            if par_match:
                field_name, value = par_match.groups()
                worker = worker_for_line(data, worker_id, is_par=True)
                worker.par_fields[field_name] = value

    if not data.workers:
        raise ValueError(f"{trace_path}: no worker access or par records found")

    missing_par = [
        worker_id
        for worker_id, worker in sorted(data.workers.items())
        if not worker.par_fields
    ]
    if missing_par:
        joined = ", ".join(str(worker_id) for worker_id in missing_par)
        raise ValueError(f"{trace_path}: no par dump found for worker(s): {joined}")

    return data


def par_assignment(array_index: int, field_name: str, value: str) -> str:
    lhs = f"pars[{array_index}].{field_name}"
    if need_shadow(True, field_name, value):
        return f"{lhs} = (uintptr_t)shadow_addr({value});"
    return f"{lhs} = {value};"


def c_string_literal(value: str) -> str:
    escaped = value.replace("\\", "\\\\").replace('"', '\\"')
    return f'"{escaped}"'


def generate_c(data: TraceData, default_devices: Sequence[str]) -> str:
    worker_ids = sorted(data.workers)
    worker_count = len(worker_ids)

    if len(default_devices) != worker_count:
        raise ValueError(
            f"device count ({len(default_devices)}) does not match "
            f"worker count ({worker_count})"
        )

    out: List[str] = []
    emit = out.append

    emit("/* === generated test.c ===")
    emit(" * Compile with: cc -O2 -pthread test.c -o test")
    emit(" * Run with defaults, or override one device path per trace worker:")
    emit(" *   ./test /dev/hwgc0 /dev/hwgc1")
    emit(" */")
    emit('#include "test.h"')
    emit("")
    emit(f"#define WORKER_COUNT {worker_count}")
    emit("")
    emit("struct WorkerContext {")
    emit("    int worker_id;")
    emit("    const char *device_path;")
    emit("    struct HWGCParameters *par;")
    emit("    int result;")
    emit("};")
    emit("")
    emit("static int read_exact(int fd, void *buffer, size_t length)")
    emit("{")
    emit("    unsigned char *cursor = (unsigned char *)buffer;")
    emit("")
    emit("    while (length > 0) {")
    emit("        ssize_t count = read(fd, cursor, length);")
    emit("        if (count < 0) {")
    emit("            if (errno == EINTR)")
    emit("                continue;")
    emit("            return -1;")
    emit("        }")
    emit("        if (count == 0) {")
    emit("            errno = EIO;")
    emit("            return -1;")
    emit("        }")
    emit("        cursor += (size_t)count;")
    emit("        length -= (size_t)count;")
    emit("    }")
    emit("    return 0;")
    emit("}")
    emit("")
    emit("static void *run_worker(void *opaque)")
    emit("{")
    emit("    struct WorkerContext *context =")
    emit("        (struct WorkerContext *)opaque;")
    emit("    int fd = -1;")
    emit("")
    emit("    context->result = 0;")
    emit("    fd = open(context->device_path, O_RDWR);")
    emit("    if (fd < 0) {")
    emit("        fprintf(stderr,")
    emit('                "worker %d: open(%s) failed: %s\\n",')
    emit("                context->worker_id, context->device_path,")
    emit("                strerror(errno));")
    emit("        context->result = errno ? errno : EIO;")
    emit("        return NULL;")
    emit("    }")
    emit("")
    emit("    if (ioctl(fd, HWGC_IOC_START, context->par) < 0) {")
    emit("        fprintf(stderr,")
    emit('                "worker %d: HWGC_IOC_START on %s failed: %s\\n",')
    emit("                context->worker_id, context->device_path,")
    emit("                strerror(errno));")
    emit("        context->result = errno ? errno : EIO;")
    emit("        close(fd);")
    emit("        return NULL;")
    emit("    }")
    emit("")
    emit("    struct HWGCEvent event;")
    emit("    while(1) {")
    emit("        memset(&event, 0, sizeof(event));")
    emit("        if (ioctl(fd, HWGC_IOC_WAIT_EVENT, &event) < 0) {")
    emit("            if (errno == EINTR)")
    emit("                continue;")
    emit("            fprintf(stderr,")
    emit('                    "worker %d: HWGC_IOC_WAIT_EVENT failed: %s\\n",')
    emit("                    context->worker_id, strerror(errno));")
    emit("            context->result = errno ? errno : EIO;")
    emit("            break;")
    emit("        }")
    emit("")
    emit("        switch(event.type) {")
    emit("            case HWGC_EVENT_DONE:")
    emit("                printf(\"worker %d on %s done\\n\", ")
    emit("                         context->worker_id, context->device_path);")
    emit("                goto out;")
    emit("            default: ")
    emit("                fprintf(stderr, ")
    emit("                        \"worker %d: HWGC_IOC_WAIT_EVENT failed: %s\\n\", ")
    emit("                        context->worker_id, strerror(errno));")
    emit("                goto out;")
    emit("        }")
    emit("    }")
    emit("")
    emit("out:")
    emit("    close(fd);")
    emit("    return NULL;")
    emit("}")
    emit("")
    emit("int main(int argc, char **argv)")
    emit("{")
    emit("    struct HWGCParameters pars[WORKER_COUNT] = {0};")
    emit("    struct WorkerContext contexts[WORKER_COUNT] = {0};")
    emit("    pthread_t threads[WORKER_COUNT];")
    emit("    size_t created = 0;")
    emit("    int failed = 0;")
    emit("    size_t i;")
    emit("")
    emit("    static const int trace_worker_ids[WORKER_COUNT] = {")
    for worker_id in worker_ids:
        emit(f"        {worker_id},")
    emit("    };")
    emit("    static const char *const default_devices[WORKER_COUNT] = {")
    for device in default_devices:
        emit(f"        {c_string_literal(device)},")
    emit("    };")
    emit("")
    emit("    if (argc != 1 && argc != WORKER_COUNT + 1) {")
    emit("        fprintf(stderr,")
    emit('                "usage: %s [device-for-worker-%d ...]\\n",')
    emit("                argv[0], trace_worker_ids[0]);")
    emit("        return EXIT_FAILURE;")
    emit("    }")
    emit("")
    emit("    init_shadowChunks();")
    emit("")
    emit("    /* One parameter block per trace worker. */")
    for array_index, worker_id in enumerate(worker_ids):
        emit(f"    /* trace worker_id {worker_id} -> pars[{array_index}] */")
        worker = data.workers[worker_id]
        for field_name, value in worker.par_fields.items():
            emit(f"    {par_assignment(array_index, field_name, value)}")
        emit("")
    emit("    /* Shared initial memory image, restored before either device starts. */")
    for statement in data.memory_restore:
        emit(f"    {statement}")
    emit("")
    emit("    for (i = 0; i < WORKER_COUNT; ++i) {")
    emit("        contexts[i].worker_id = trace_worker_ids[i];")
    emit("        contexts[i].device_path =")
    emit("            (argc == WORKER_COUNT + 1) ? argv[i + 1] :")
    emit("                                         default_devices[i];")
    emit("        contexts[i].par = &pars[i];")
    emit("        contexts[i].result = 0;")
    emit("")
    emit("        {")
    emit("            int rc = pthread_create(&threads[i], NULL,")
    emit("                                    run_worker, &contexts[i]);")
    emit("            if (rc != 0) {")
    emit("                fprintf(stderr,")
    emit('                        "worker %d: pthread_create failed: %s\\n",')
    emit("                        contexts[i].worker_id, strerror(rc));")
    emit("                failed = 1;")
    emit("                break;")
    emit("            }")
    emit("        }")
    emit("        ++created;")
    emit("    }")
    emit("")
    emit("    for (i = 0; i < created; ++i) {")
    emit("        int rc = pthread_join(threads[i], NULL);")
    emit("        if (rc != 0) {")
    emit("            fprintf(stderr,")
    emit('                    "worker %d: pthread_join failed: %s\\n",')
    emit("                    contexts[i].worker_id, strerror(rc));")
    emit("            failed = 1;")
    emit("        }")
    emit("        if (contexts[i].result != 0)")
    emit("            failed = 1;")
    emit("    }")
    emit("")
    emit("    return failed ? EXIT_FAILURE : EXIT_SUCCESS;")
    emit("}")
    emit("")

    return "\n".join(out)


def default_device_paths(worker_ids: Sequence[int]) -> List[str]:
    return [f"/dev/hwgc{worker_id}" for worker_id in worker_ids]


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Convert an interleaved multi-worker HWGC trace into a C test "
            "that dispatches one trace worker to one device/pthread."
        )
    )
    parser.add_argument("trace", type=Path, help="input trace log")
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="write generated C to this path instead of stdout",
    )
    parser.add_argument(
        "--devices",
        nargs="+",
        metavar="DEVICE",
        help=(
            "default device paths embedded in generated C, ordered by sorted "
            "worker_id; count must equal the number of workers"
        ),
    )
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = build_arg_parser().parse_args(argv)

    try:
        data = parse_trace(args.trace)
        worker_ids = sorted(data.workers)
        devices = (
            list(args.devices)
            if args.devices is not None
            else default_device_paths(worker_ids)
        )
        generated = generate_c(data, devices)
    except (OSError, ValueError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1

    output: TextIO
    close_output = False
    if args.output is None:
        output = sys.stdout
    else:
        output = args.output.open("w", encoding="utf-8")
        close_output = True

    try:
        output.write(generated)
    finally:
        if close_output:
            output.close()

    summary = ", ".join(
        f"worker {worker_id}: "
        f"{data.workers[worker_id].access_count} accesses, "
        f"{len(data.workers[worker_id].par_fields)} par fields"
        for worker_id in worker_ids
    )
    print(
        f"generated {len(worker_ids)} workers; {summary}; "
        f"{len(data.memory_restore)} initial memory operations",
        file=sys.stderr,
    )
    if data.unscoped_accesses or data.unscoped_par_assignments:
        print(
            "warning: records without worker_id were assigned to worker 0 "
            f"({data.unscoped_accesses} accesses, "
            f"{data.unscoped_par_assignments} par assignments)",
            file=sys.stderr,
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
