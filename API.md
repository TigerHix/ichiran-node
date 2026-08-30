# Ichiran HTTP API

The HTTP server exposes the packed analyzer through Node.js. It loads one immutable
release at startup and does not use PostgreSQL.

## Start

```bash
bun run build
export ICHIRAN_PACK_DIR=/absolute/path/to/analyzer-release
export PORT=3000                     # optional; defaults to 3000
bun run dev
```

The release directory must contain `manifest.json`, `hot.bin.gz`, and
`details.bin.gz`. The API binds to `0.0.0.0` and allows cross-origin JSON requests.

## Requests

All analysis endpoints accept JSON. `text` must be a non-empty string. `limit` must
be a positive integer and defaults to `1`. Optional entity hints have the form:

```json
{
  "start": 0,
  "end": 2,
  "boost": 500
}
```

`start` and `end` are UTF-16 string offsets, with an exclusive end. `boost` is
optional.

### `POST /api/romanize`

```bash
curl -X POST http://localhost:3000/api/romanize \
  -H 'Content-Type: application/json' \
  -d '{"text":"こんにちは"}'
```

```json
{
  "text": "こんにちは",
  "romanized": "konnichiwa"
}
```

### `POST /api/romanize/info`

Returns romanization plus reverse-ordered token definitions in the historical info
format.

```json
{
  "text": "今日はいい天気です",
  "romanized": "kyō wa ii tenki desu",
  "info": [
    ["desu", "です\n1. [aux-v,cop] be; is"]
  ]
}
```

The example is abbreviated; actual definitions include the complete selected senses
and conjugation information.

### `POST /api/segment`

Returns the legacy-compatible detailed segmentation shape.

```bash
curl -X POST http://localhost:3000/api/segment \
  -H 'Content-Type: application/json' \
  -d '{"text":"ご注文はうさぎですか","limit":3}'
```

```json
{
  "text": "ご注文はうさぎですか",
  "segments": [],
  "limit": 3
}
```

`segments` above is abbreviated. The real value contains paths, romanized tokens,
dictionary entries, alternatives, conjugations, and integer path scores. Pass an
`entities` array alongside `text` and `limit` to apply entity hints.

### `POST /api/analyze`

This route is retained as an analyzer-only compatibility surface. It returns packed
segmentation and explicitly reports the grammar exclusion:

```json
{
  "segments": [],
  "grammars": {},
  "grammarExcluded": true
}
```

The separate experimental grammar package is not loaded and `maxMatches` has no role
in this milestone.

## Utility endpoints

| Method and path | Result |
|---|---|
| `GET /health` | Process health and timestamp |
| `GET /health/db` | Compatibility health route; reports `database: "not-required"` |
| `GET /api` | Endpoint index and examples |
| `POST /api/test` | Echo, process memory, uptime, and timestamp |
| `OPTIONS *` | CORS preflight |

Malformed JSON, missing text, and invalid limits or entity hints return `400`.
Bodies larger than 1 MiB return `413`; unknown routes return `404`; unexpected
runtime failures return `500`.

## Runtime boundary

`@ichiran/api` contains transport code only. Manifest verification and filesystem
loading live in `@ichiran/node`; analysis lives in `@ichiran/core`. The browser demo
uses the same core runtime, so HTTP and browser output do not drift into separate
implementations.
