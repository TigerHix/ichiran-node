# Combined Dockerfile for Ichiran with PostgreSQL and Node.js in same container
# This is necessary for performance - the app makes many micro-reads from the database
# Optimized for fly.io deployment with persistent volume for PostgreSQL data
FROM postgres:16

# Install Node.js 20 and curl (needed for health checks)
RUN apt-get update && apt-get install -y \
    curl \
    ca-certificates \
    gnupg \
    && mkdir -p /etc/apt/keyrings \
    && curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg \
    && echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_20.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list \
    && apt-get update \
    && apt-get install -y nodejs

# Install bun
RUN curl -fsSL https://bun.sh/install | bash && \
    ln -s /root/.bun/bin/bun /usr/local/bin/bun

# Set up PostgreSQL
# Use custom PGDATA path for fly.io volume mount
# Volume mounts at /data, PostgreSQL uses /data/pgdata subdirectory
ENV POSTGRES_PASSWORD=password
ENV POSTGRES_DB=postgres
ENV PGDATA=/data/pgdata

# Copy database dump (SQL format, gzipped) - will be used on first startup
# Keep at /opt since /data will be replaced by volume mount
COPY jmdict.sql.gz /opt/jmdict.sql.gz

# Copy application code
WORKDIR /app
COPY package.json bun.lock ./
COPY packages/core/package.json packages/core/
COPY packages/grammar/package.json packages/grammar/
COPY packages/api/package.json packages/api/
COPY packages/data/package.json packages/data/
COPY packages/cli/package.json packages/cli/
COPY tsconfig.base.json ./
RUN bun install --frozen-lockfile

COPY . .
RUN bun run build

# Copy startup script
COPY docker-entrypoint.sh /start.sh
RUN chmod +x /start.sh

# Expose ports
EXPOSE 5432 3000

# Set environment variables for the app
ENV ICHIRAN_DB_URL=postgresql://postgres:password@localhost:5432/jmdict
ENV PORT=3000

# Use startup script with CMD (not ENTRYPOINT to avoid conflicts with postgres base image)
CMD ["/start.sh"]
