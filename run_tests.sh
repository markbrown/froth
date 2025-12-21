#!/bin/sh
# Test runner for Froth regression tests

set -e

FROTH="${FROTH:-./bin/froth}"
TESTS_DIR="${TESTS_DIR:-tests}"

pass=0
fail=0
errors=""

for test_file in "$TESTS_DIR"/*.froth; do
    test_name=$(basename "$test_file" .froth)
    expected_file="$TESTS_DIR/$test_name.expected"

    if [ ! -f "$expected_file" ]; then
        echo "SKIP: $test_name (no .expected file)"
        continue
    fi

    # Check for a .lib file specifying a library to load
    lib_file="$TESTS_DIR/$test_name.lib"
    if [ -f "$lib_file" ]; then
        lib_path=$(cat "$lib_file" | tr -d '\n')
        actual=$("$FROTH" -l "$lib_path" "$test_file" 2>&1) || true
    else
        actual=$("$FROTH" "$test_file" 2>&1) || true
    fi
    expected=$(cat "$expected_file")

    if [ "$actual" = "$expected" ]; then
        echo "PASS: $test_name"
        pass=$((pass + 1))
    else
        echo "FAIL: $test_name"
        errors="$errors\n=== $test_name ===\nExpected:\n$expected\nActual:\n$actual\n"
        fail=$((fail + 1))
    fi
done

echo ""
echo "Results: $pass passed, $fail failed"

if [ $fail -gt 0 ]; then
    printf "$errors"
    exit 1
fi
