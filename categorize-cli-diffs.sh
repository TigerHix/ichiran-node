#!/bin/bash

export ICHIRAN_TEST_DB_URL="postgresql://postgres:password@localhost:6777/jmdict_test"

echo "Running CLI tests and categorizing differences..."
bun test packages/cli/tests/cli-parity.test.ts 2>&1 | tee cli-full.txt | tail -5

echo -e "\n=== CATEGORIZING DIFFERENCES ==="

# Count types
echo "Total failures: $(grep -c '(fail)' cli-full.txt)"
echo "Total passes: $(grep -c '(pass)' cli-full.txt)"

echo -e "\n=== Difference Types ==="

# Extract just the diff sections
grep -A 100 "^@@" cli-full.txt > diffs-only.txt

# Gloss additions (+ lines with "gloss":)
GLOSS_ADD=$(grep -c '^+.*"gloss":' diffs-only.txt)
echo "Gloss additions: $GLOSS_ADD"

# Info changes
INFO_CHANGE=$(grep -c '^[+-].*"info":' diffs-only.txt)
echo "Info field changes: $INFO_CHANGE"

# Seq changes
SEQ_CHANGE=$(grep -c '^[+-].*"seq":' diffs-only.txt)
echo "Seq number changes: $SEQ_CHANGE"

# Score changes  
SCORE_CHANGE=$(grep -c '^[+-].*"score":' diffs-only.txt)
echo "Score changes: $SCORE_CHANGE"

# Total score changes (top level)
TOTAL_SCORE=$(grep -c '^[+-]       [0-9]' diffs-only.txt)
echo "Total score changes (path level): $TOTAL_SCORE"

echo -e "\n=== Sample Failures ==="
grep "(fail)" cli-full.txt | head -10

