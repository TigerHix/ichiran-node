FROM oven/bun:1.3.5-alpine AS build

WORKDIR /app
COPY package.json bun.lock tsconfig.base.json ./
COPY packages/api/package.json packages/api/package.json
COPY packages/browser-demo/package.json packages/browser-demo/package.json
COPY packages/cli/package.json packages/cli/package.json
COPY packages/core/package.json packages/core/package.json
COPY packages/data/package.json packages/data/package.json
COPY packages/grammar/package.json packages/grammar/package.json
COPY packages/node/package.json packages/node/package.json
COPY packages/reference-postgres/package.json packages/reference-postgres/package.json
COPY packages/testing/package.json packages/testing/package.json
RUN bun install --frozen-lockfile

COPY packages/api packages/api
COPY packages/core packages/core
COPY packages/node packages/node
RUN bun run --cwd packages/core build \
  && bun run --cwd packages/node build \
  && bun run --cwd packages/api build \
  && bun build packages/api/dist/index.js --target=bun --outfile=/tmp/ichiran-api.js

FROM oven/bun:1.3.5-alpine

WORKDIR /app
ENV NODE_ENV=production
ENV PORT=3000
ENV ICHIRAN_PACK_DIR=/app/analyzer

COPY --from=build /tmp/ichiran-api.js /app/ichiran-api.js
COPY dist/browser-alpha/ /app/analyzer/

EXPOSE 3000
CMD ["bun", "/app/ichiran-api.js"]
