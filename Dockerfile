# syntax=docker/dockerfile:1
FROM oven/bun:1.3.5-alpine AS build

WORKDIR /app
ARG ICHIRAN_SOURCE_COMMIT
ARG ICHIRAN_RELEASE_GENERATION
RUN test -n "$ICHIRAN_SOURCE_COMMIT" \
  && test -n "$ICHIRAN_RELEASE_GENERATION" \
  || (echo "ICHIRAN_SOURCE_COMMIT and ICHIRAN_RELEASE_GENERATION build args are required" >&2; exit 1)
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
COPY packages/data/src/browser-pack/release-publication.ts packages/data/src/browser-pack/release-publication.ts
COPY dist/browser-alpha.generations/${ICHIRAN_RELEASE_GENERATION}/ /tmp/analyzer/
RUN <<'VERIFY_RELEASE'
ICHIRAN_SOURCE_COMMIT="$ICHIRAN_SOURCE_COMMIT" \
ICHIRAN_RELEASE_GENERATION="$ICHIRAN_RELEASE_GENERATION" bun -e '
  import { readdir } from "node:fs/promises";
  import { analyzerReleaseGenerationIdentity } from "./packages/data/src/browser-pack/release-publication.ts";
  const root = "/tmp/analyzer";
  const manifest = JSON.parse(await Bun.file(`${root}/manifest.json`).text());
  const expected = ["manifest.json", "stats.json", manifest.hot?.file, manifest.details?.file].sort();
  const actual = (await readdir(root)).sort();
  if (manifest.sourceCommit !== process.env.ICHIRAN_SOURCE_COMMIT) {
    throw new Error(`Analyzer sourceCommit ${manifest.sourceCommit} != code ${process.env.ICHIRAN_SOURCE_COMMIT}`);
  }
  if (actual.join("\n") !== expected.join("\n")) {
    throw new Error(`Analyzer inventory ${actual.join(",")} != ${expected.join(",")}`);
  }
  const files = new Map(await Promise.all(actual.map(async name => [
    name,
    new Uint8Array(await Bun.file(`${root}/${name}`).arrayBuffer())
  ])));
  const generation = analyzerReleaseGenerationIdentity(files);
  if (generation !== process.env.ICHIRAN_RELEASE_GENERATION) {
    throw new Error(`Analyzer generation ${generation} != requested ${process.env.ICHIRAN_RELEASE_GENERATION}`);
  }
  '
VERIFY_RELEASE
RUN bun run --cwd packages/core build \
  && bun run --cwd packages/node build \
  && bun run --cwd packages/api build \
  && ICHIRAN_PACK_DIR=/tmp/analyzer ICHIRAN_SOURCE_COMMIT="$ICHIRAN_SOURCE_COMMIT" \
    bun -e 'import { openAnalyzer } from "./packages/node/dist/index.js"; const analyzer = await openAnalyzer(); analyzer.dispose();' \
  && bun build packages/api/dist/index.js --target=bun --outfile=/tmp/ichiran-api.js

FROM oven/bun:1.3.5-alpine

WORKDIR /app
ARG ICHIRAN_SOURCE_COMMIT
ENV NODE_ENV=production
ENV PORT=3000
ENV ICHIRAN_PACK_DIR=/app/analyzer
ENV ICHIRAN_SOURCE_COMMIT=$ICHIRAN_SOURCE_COMMIT

COPY --from=build /tmp/ichiran-api.js /app/ichiran-api.js
COPY --from=build /tmp/analyzer/ /app/analyzer/

EXPOSE 3000
CMD ["bun", "/app/ichiran-api.js"]
