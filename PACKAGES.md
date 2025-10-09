# Packages

Monorepo split into 5 packages with clear boundaries.

## Structure

```
@ichiran/core     - segmentation, dict, romanize, presentation, connection
@ichiran/grammar  - grammar runtime, predicates, defs
@ichiran/api      - HTTP server
@ichiran/cli      - user CLI
@ichiran/data     - DB init/ETL
```

## Dependencies

```
cli → core (+ grammar optional)
api → core + grammar
grammar → core
data → core
```

## Commands

### CLI
```bash
ichiran-cli "こんにちは"
ichiran-cli -i "text"           # with info
ichiran-cli -f "text"           # full JSON
ichiran-cli -l 5 -f "text"      # 5 alternatives
```

### Data
```bash
ichiran-data init-db
ichiran-data download [--jmdict|--kanjidic] [--force]
ichiran-data load-jmdict [--max N]
ichiran-data load-kanjidic
ichiran-data load-conjugations
ichiran-data load-secondary-conjugations
ichiran-data load-custom --extra|--municipality|--ward
ichiran-data apply-errata
ichiran-data stats
ichiran-data best-readings
ichiran-data kanji-stats
ichiran-data reading-stats
```

## Env Parsing

Core exports `setConnection(spec)` + connection primitives. NO env parsing in core.

API, CLI, data each parse `ICHIRAN_DB_URL` or `ICHIRAN_TEST_DB_URL` locally and call `setConnection`.

