#!/usr/bin/env bash
set -euo pipefail

# Download all required JMDict CSV files for Ichiran

cd "$(dirname "$0")"

echo "Updating JMDict CSV data files..."
echo ""

# CSV files
echo "Downloading JMDict CSV data files from GitLab..."
wget -q --show-progress -O kwpos.csv https://gitlab.com/yamagoya/jmdictdb/-/raw/master/jmdictdb/data/kwpos.csv
wget -q --show-progress -O conj.csv https://gitlab.com/yamagoya/jmdictdb/-/raw/master/jmdictdb/data/conj.csv
wget -q --show-progress -O conjo.csv https://gitlab.com/yamagoya/jmdictdb/-/raw/master/jmdictdb/data/conjo.csv
echo "✓ CSV rules ready"
echo ""

echo "All data files ready!"
ls -lh *.csv
