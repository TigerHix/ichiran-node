#!/usr/bin/env bash
set -euo pipefail

# Download all required data files for Ichiran

cd "$(dirname "$0")"

echo "Downloading Ichiran data files..."
echo ""

# JMDict (~140 MB)
if [ ! -f JMdict.xml ]; then
    echo "Downloading JMDict dictionary..."
    wget -q --show-progress http://ftp.edrdg.org/pub/Nihongo/JMdict_e.gz
    echo "Extracting..."
    gunzip JMdict_e.gz
    mv JMdict_e JMdict.xml
    echo "✓ JMdict.xml ready"
else
    echo "✓ JMdict.xml already exists"
fi
echo ""

# Kanjidic2 (~7 MB) - REQUIRED
if [ ! -f kanjidic2.xml ]; then
    echo "Downloading Kanjidic2 (REQUIRED for kanji readings)..."
    wget -q --show-progress https://www.edrdg.org/kanjidic/kanjidic2.xml.gz
    echo "Extracting..."
    gunzip kanjidic2.xml.gz
    echo "✓ kanjidic2.xml ready"
else
    echo "✓ kanjidic2.xml already exists"
fi
echo ""

# CSV files (always download latest)
echo "Downloading conjugation rules from GitLab..."
wget -q --show-progress -O kwpos.csv https://gitlab.com/yamagoya/jmdictdb/-/raw/master/jmdictdb/data/kwpos.csv
wget -q --show-progress -O conj.csv https://gitlab.com/yamagoya/jmdictdb/-/raw/master/jmdictdb/data/conj.csv
wget -q --show-progress -O conjo.csv https://gitlab.com/yamagoya/jmdictdb/-/raw/master/jmdictdb/data/conjo.csv
echo "✓ CSV rules ready"
echo ""

echo "All data files ready!"
ls -lh JMdict.xml kanjidic2.xml *.csv
