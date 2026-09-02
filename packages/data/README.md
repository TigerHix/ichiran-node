# Source compiler and PostgreSQL migration oracle

`@ichiran/data` builds pack-format-v1 releases directly from pinned JMdict,
Kanjidic2, custom XML/CSV, conjugation CSVs, chronological errata, and a small
compatibility ledger. The normal compiler does not connect to PostgreSQL. See
[the source-compiler and Rust-kernel roadmap](../../docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

## Status

The source compiler is the active release producer. Exact inputs and hashes live in
`data/source-compiler-sources.lock.json`; acquisition and release instructions are in
`docs/source-compiler/M2-SOURCES.md` and `M6-RELEASE-WORKFLOW.md`. Kanjidic is used
only to resolve analyzer-required easy hints and is not exposed as a runtime API.

The commands below describe the frozen PostgreSQL migration oracle. They do not
produce or replace pinned source-compiler inputs.

## Loading Sequence

Matches Lisp `load-extras` from dict-load.lisp:185-194. Total time: ~26 minutes.

1. `ichiran-data download` - Download current live JMDict/Kanjidic2 into the ignored `work/live-data` destination
2. `ichiran-data init-db` - Create schema (**drops all tables!**)
3. `ichiran-data load-jmdict` - Load main dictionary (~214k entries, ~3 min)
4. `ichiran-data load-conjugations` - Generate verb/adjective conjugations (~686k forms, ~2 min)
5. `ichiran-data load-secondary-conjugations` - Generate compound conjugations (~1.7M forms, ~20 min)
6. `ichiran-data load-custom --extra --municipality --ward` - Load custom entries (geographic names, ~3.4k entries, ~1 min)
7. `ichiran-data apply-errata` - Apply corrections + reload :extra (~1 min)
8. `ichiran-data best-readings` - Calculate best_kana/best_kanji (~10 sec)
9. `ichiran-data load-kanjidic --path ./data/kanjidic2.xml.gz` - Load kanji data (auto-decompresses .gz, ~2 sec)
10. `ichiran-data stats` - Show database statistics

**Important Notes:**
- **Step 6 vs Step 7**: Step 6 loads all custom data types (:extra, :municipality, :ward) with conjugations. Step 7 (apply-errata) internally reloads ONLY :extra to pick up any errata-added entries (dict-errata.lisp:580). Both steps are required.

## Flags

- `--max N` - limit entries (testing)
- `--force` - re-download only in the live/legacy destination; it cannot overwrite compiler pins
- `--path <file>` - custom XML path
- `--no-download` - skip auto-download
- `--extra|--municipality|--ward` - custom data type

## Quick Start: Build Database

Use the automated build script:

```bash
./scripts/build-db.sh [database_name]
```

Default database: `jmdict_test`. Runs all steps 1-9, takes ~26 minutes.

Then run tests:
```bash
ICHIRAN_DB_URL="postgresql://postgres:password@localhost:6777/jmdict_test" bun test
```
