#!/bin/bash
set -e

# Load environment variables
if [ -f ".env" ]; then
    export $(grep -v '^#' .env | xargs)
else
    echo "ERROR: .env file not found"
    exit 1
fi

# Parse database URL from ICHIRAN_DB_URL
# Format: postgresql://user:password@host:port/database
if [ -z "$ICHIRAN_DB_URL" ]; then
    echo "ERROR: ICHIRAN_DB_URL not set in .env"
    exit 1
fi

# Extract components using bash parameter expansion
DB_URL_NO_PROTOCOL="${ICHIRAN_DB_URL#postgresql://}"
DB_USER_PASS="${DB_URL_NO_PROTOCOL%%@*}"
DB_HOST_PORT_DB="${DB_URL_NO_PROTOCOL#*@}"
DB_USER="${DB_USER_PASS%%:*}"
DB_PASS="${DB_USER_PASS#*:}"
DB_HOST_PORT="${DB_HOST_PORT_DB%%/*}"
DB_HOST="${DB_HOST_PORT%%:*}"
DB_PORT="${DB_HOST_PORT#*:}"
DB_NAME="${DB_HOST_PORT_DB##*/}"

echo "=== Pre-deployment checks ==="

# Check if SQL dump exists
if [ ! -f "jmdict.sql.gz" ]; then
    echo "❌ jmdict.sql.gz not found!"
    echo "Generating SQL dump from local PostgreSQL..."

    # Check if PostgreSQL is accessible
    if ! pg_isready -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" > /dev/null 2>&1; then
        echo "ERROR: PostgreSQL not accessible at $DB_HOST:$DB_PORT"
        echo "Please ensure PostgreSQL is running with the $DB_NAME database"
        exit 1
    fi

    # Generate SQL dump
    echo "Creating SQL dump (this may take 1-2 minutes)..."
    export PGPASSWORD="$DB_PASS"
    pg_dump -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -F p | gzip > jmdict.sql.gz

    # Verify dump was created
    if [ -f "jmdict.sql.gz" ]; then
        SIZE=$(du -h jmdict.sql.gz | cut -f1)
        echo "✅ SQL dump created successfully: $SIZE"
    else
        echo "ERROR: Failed to create SQL dump"
        exit 1
    fi
else
    SIZE=$(du -h jmdict.sql.gz | cut -f1)
    echo "✅ SQL dump found: $SIZE"
fi

echo ""
echo "=== Deploying to fly.io ==="
fly deploy "$@"
