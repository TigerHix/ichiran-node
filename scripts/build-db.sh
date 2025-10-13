#!/bin/bash
# Build database from scratch
# Usage: ./scripts/build-db.sh [database_name]
# Default database: jmdict_test

set -e

# Configuration - allow database name as first argument
DB_NAME="${1:-jmdict_test}"
DB_URL="postgresql://postgres:password@localhost:6777/${DB_NAME}"

echo "========================================"
echo "Building Database: ${DB_NAME}"
echo "========================================"

# Drop and recreate database
echo ""
echo "Step 0: Drop and recreate database..."
psql "postgresql://postgres:password@localhost:6777/postgres" <<SQL
SELECT pg_terminate_backend(pid) 
FROM pg_stat_activity 
WHERE datname = '${DB_NAME}' AND pid <> pg_backend_pid();

DROP DATABASE IF EXISTS ${DB_NAME};
CREATE DATABASE ${DB_NAME};
SQL

export ICHIRAN_DB_URL="${DB_URL}"

# Initialize schema
echo ""
echo "Step 1: Initialize schema..."
bun run data init-db 2>&1 | tail -3

# Load JMdict
echo ""
echo "Step 2: Load JMdict (~3 min)..."
bun run data load-jmdict 2>&1 | tail -3

# Load conjugations
echo ""
echo "Step 3: Load conjugations (~2 min)..."
timeout 600 bun run data load-conjugations 2>&1 | tail -3

# Load secondary conjugations
echo ""
echo "Step 4: Load secondary conjugations (~20 min)..."
timeout 3600 bun run data load-secondary-conjugations 2>&1 | tail -3

# Load custom data
echo ""
echo "Step 5: Load custom data (~1 min)..."
bun run data load-custom --extra --municipality --ward 2>&1 | tail -3

# Apply errata
echo ""
echo "Step 6: Apply errata (~1 min)..."
bun run data apply-errata 2>&1 | tail -3

# Calculate best readings
echo ""
echo "Step 7: Calculate best readings (~10 sec)..."
bun run data best-readings 2>&1 | tail -3

# Load kanjidic (optional)
echo ""
echo "Step 8: Load kanjidic (~2 sec)..."
bun run data load-kanjidic --path ./data/kanjidic2.xml.gz 2>&1 | tail -3

# Show statistics
echo ""
echo "Step 9: Database statistics..."
bun run data stats 2>&1

echo ""
echo "========================================"
echo "✓ Database built successfully!"
echo "========================================"
echo ""
echo "To run tests:"
echo "  ICHIRAN_DB_URL=\"${DB_URL}\" bun test"
echo ""

