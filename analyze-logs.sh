#!/bin/bash

echo "=== Analyzing エロそう segmentation logs ==="
echo ""
echo "PRODUCTION: Result is 'エロそう, だ'"
echo "TEST: Result is 'エロ, そう, だ'"
echo ""
echo "=== Key question: What queries involve エロ? ==="
echo ""

echo "--- PRODUCTION: Queries with エロ ---"
grep -n "text = 'エロ'" logs/ero-sou-prod.log | head -20

echo ""
echo "--- TEST: Queries with エロ ---"
grep -n "text = 'エロ'" logs/ero-sou-test.log | head -20

echo ""
echo "=== Check substring hash queries ==="
echo ""
echo "--- PRODUCTION: kana_text IN query ---"
grep -B 2 "kana_text WHERE text IN" logs/ero-sou-prod.log | grep -A 2 "Query #" | head -30

echo ""
echo "--- TEST: kana_text WHERE text IN query ---"
grep -B 2 "kana_text WHERE text IN" logs/ero-sou-test.log | grep -A 2 "Query #" | head -30
