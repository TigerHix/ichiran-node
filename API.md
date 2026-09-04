# Ichiran HTTP API

The server exposes the same packed `Analyzer` contract through a small versioned JSON
transport. It loads `ICHIRAN_PACK_DIR` at startup and does not use PostgreSQL.

## Endpoints

`GET /health` returns:

```json
{"status":"ok"}
```

`POST /v1/analyze` accepts `{ "text": string, "options"?: AnalyzeOptions }` and
returns the `AnalysisResult` directly:

```bash
curl -X POST http://localhost:3000/v1/analyze \
  -H 'Content-Type: application/json' \
  -d '{"text":"今日はいい天気です","options":{"limit":3}}'
```

`POST /v1/romanize` accepts `{ "text": string, "options"?: RomanizeOptions }` and
returns `{ "romanized": string }`.

`GET /v1/entries/:entryIndex` returns the `DictionaryEntry` identified by an
`entryIndex` from an analysis token. Add `?locale=zh-Hans` for Simplified Chinese;
the default is `en`.

`POST /v1/details` accepts the locale in its options:

```bash
curl -X POST http://localhost:3000/v1/details \
  -H 'Content-Type: application/json' \
  -d '{"text":"猫","options":{"pathIndex":0,"tokenIndex":0,"locale":"zh-Hans"}}'
```

Offsets in entity hints and result tokens are UTF-16 offsets, with an exclusive end.
Bodies larger than 1 MiB return 413. Every failure has one envelope:

```json
{"error":{"code":"invalid-input","message":"limit must be an integer from 1 to 10"}}
```

Product codes are `invalid-input`, `invalid-pack`, `not-found`, and `internal`.
Browser installation/lifecycle errors are not part of this HTTP contract.

The historical `/api/*` routes, `/health/db`, echo diagnostics, grammar placeholder,
legacy segmentation JSON, and romanization-info formatter were removed. See
[MIGRATION.md](./MIGRATION.md).
