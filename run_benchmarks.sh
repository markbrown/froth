#!/bin/sh
# Run Froth benchmarks

FROTH="${FROTH:-./bin/froth}"
BENCHMARKS_DIR="${BENCHMARKS_DIR:-benchmarks}"

echo "Froth Benchmarks"
echo "================"
echo ""

for bench_file in "$BENCHMARKS_DIR"/*.froth; do
    bench_name=$(basename "$bench_file" .froth)
    # Extract description from first line comment
    desc=$(head -1 "$bench_file" | sed 's/^; Benchmark: //')
    printf "%-20s " "$bench_name:"
    result=$("$FROTH" "$bench_file" 2>&1)
    echo "$result ticks ($desc)"
done
