#!/bin/bash
set -e

# Unbuffer stdout/stderr for immediate logging
export NODE_OPTIONS="--max-old-space-size=6144"
echo "=== Starting Ichiran (PostgreSQL + Node.js) ==="

# Trap errors and log them
trap 'echo "[FATAL] Script died at line $LINENO with exit code $?"' ERR

# Ensure /data is accessible (volume mount)
echo "[$(date +%T)] Ensuring /data directory permissions..."
chown postgres:postgres /data 2>/dev/null || echo "Note: /data already configured"
chmod 755 /data

# Check if PostgreSQL data directory needs initialization
if [ ! -s "$PGDATA/PG_VERSION" ]; then
    echo "[$(date +%T)] PostgreSQL data directory is empty - initializing..."
    echo "[$(date +%T)] This will take 1-2 minutes (only happens on first deployment)"

    # Create PGDATA directory
    mkdir -p "$PGDATA"
    chown -R postgres:postgres "$PGDATA"
    chmod 700 "$PGDATA"

    # Initialize PostgreSQL cluster
    echo "[$(date +%T)] Running initdb..."
    su postgres -c "initdb -D $PGDATA"

    # Start PostgreSQL temporarily to restore database
    echo "[$(date +%T)] Starting PostgreSQL temporarily for database restore..."
    su postgres -c "pg_ctl -D $PGDATA -l /tmp/postgres-init.log start"

    # Wait for PostgreSQL to be ready
    echo "[$(date +%T)] Waiting for PostgreSQL to accept connections..."
    until pg_isready -h localhost -p 5432 -U postgres > /dev/null 2>&1; do
        sleep 1
    done

    # Create database and restore from dump
    echo "[$(date +%T)] Creating jmdict database..."
    su postgres -c "psql -U postgres -c 'CREATE DATABASE jmdict;'"

    echo "[$(date +%T)] Restoring database from SQL dump (this takes ~1-2 minutes)..."
    echo "[$(date +%T)] SQL dump size: $(du -h /opt/jmdict.sql.gz | cut -f1)"

    # Use pv if available for progress, otherwise plain restore with periodic updates
    if command -v pv > /dev/null 2>&1; then
        gunzip -c /opt/jmdict.sql.gz | pv -s 600M -N "Restore progress" | su postgres -c "psql -U postgres -d jmdict" > /tmp/restore.log 2>&1
    else
        # Show periodic status updates during restore
        gunzip -c /opt/jmdict.sql.gz | su postgres -c "psql -U postgres -d jmdict" 2>&1 | tee /tmp/restore.log | grep -i "COPY\|CREATE\|ALTER" &
        RESTORE_PID=$!

        # Keep-alive loop: show we're still working
        while kill -0 $RESTORE_PID 2>/dev/null; do
            echo "[$(date +%T)] Still restoring database... ($(wc -l < /tmp/restore.log) SQL statements processed)"
            sleep 30
        done
        wait $RESTORE_PID
    fi

    echo "[$(date +%T)] Restore completed! Log lines: $(wc -l < /tmp/restore.log)"

    # Stop PostgreSQL
    echo "[$(date +%T)] Database restored! Stopping temporary PostgreSQL..."
    su postgres -c "pg_ctl -D $PGDATA stop"

    echo "[$(date +%T)] ✓ Database initialization complete!"
else
    echo "[$(date +%T)] PostgreSQL data directory already initialized (using existing data)"
fi

# Start PostgreSQL using the official postgres entrypoint in background
echo "[$(date +%T)] Starting PostgreSQL..."
su postgres -c "postgres -D $PGDATA" &
POSTGRES_PID=$!
echo "[$(date +%T)] PostgreSQL started with PID: $POSTGRES_PID"

# Wait for PostgreSQL to be ready (longer timeout for crash recovery scenarios)
echo "[$(date +%T)] Waiting for PostgreSQL to be ready..."
TIMEOUT=120  # 2 minutes to handle crash recovery
COUNTER=0
while [ $COUNTER -lt $TIMEOUT ]; do
    if pg_isready -h localhost -p 5432 -U postgres > /dev/null 2>&1; then
        echo "[$(date +%T)] PostgreSQL is ready!"
        break
    fi

    # Show progress every 10 seconds
    if [ $((COUNTER % 10)) -eq 0 ] && [ $COUNTER -gt 0 ]; then
        echo "[$(date +%T)] Still waiting for PostgreSQL... (${COUNTER}s elapsed)"
    fi

    sleep 1
    COUNTER=$((COUNTER + 1))
done

if [ $COUNTER -eq $TIMEOUT ]; then
    echo "[$(date +%T)] ERROR: PostgreSQL failed to start within $TIMEOUT seconds"
    echo "[$(date +%T)] This may indicate crash recovery in progress. Check logs below:"
    cat /tmp/postgres-init.log 2>/dev/null || true
    ps aux | grep postgres || true
    exit 1
fi

# Start Node.js application
echo "[$(date +%T)] Starting Ichiran API server..."
cd /app
node dist/server.js &
NODEJS_PID=$!
echo "[$(date +%T)] Node.js started with PID: $NODEJS_PID"

# Brief check that Node.js is responding
echo "[$(date +%T)] Verifying Node.js API is responding..."
sleep 2
for i in {1..10}; do
    if curl -sf http://localhost:3000/health > /dev/null 2>&1; then
        echo "[$(date +%T)] ✓ Node.js API is responding successfully!"
        break
    fi
    sleep 1
done

echo "[$(date +%T)] === Container is ready! ==="
echo "PostgreSQL PID: $POSTGRES_PID"
echo "Node.js PID: $NODEJS_PID"

# Wait for any process to exit
wait -n $POSTGRES_PID $NODEJS_PID
EXIT_CODE=$?
echo "[$(date +%T)] FATAL: Process exited with code: $EXIT_CODE"
ps aux | grep -E "postgres|node" || true
exit $EXIT_CODE
