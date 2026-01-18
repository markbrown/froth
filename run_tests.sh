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

    # Check for per-test options file
    opts_file="$TESTS_DIR/$test_name.opts"
    if [ -f "$opts_file" ]; then
        opts=$(cat "$opts_file" | tr -d '\n')
        actual=$("$FROTH" $opts "$test_file" 2>&1 | grep -v '^FROTH: ') || true
    else
        actual=$("$FROTH" "$test_file" 2>&1 | grep -v '^FROTH: ') || true
    fi
    expected=$(cat "$expected_file")

    if [ "$actual" = "$expected" ]; then
        echo "PASS: $test_name"
        pass=$((pass + 1))
    else
        echo "FAIL: $test_name"
        # Store diff for later display
        actual_file=$(mktemp)
        printf '%s\n' "$actual" > "$actual_file"
        diff_output=$(diff -u "$expected_file" "$actual_file" | tail -n +3) || true
        rm -f "$actual_file"
        errors="$errors\n=== $test_name ===\n$diff_output\n"
        fail=$((fail + 1))
    fi
done

echo ""
echo "Results: $pass passed, $fail failed"

if [ $fail -gt 0 ]; then
    printf "$errors"
    exit 1
fi
