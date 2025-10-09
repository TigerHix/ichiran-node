# Data Loading

Brief guide to loading JMDict/Kanjidic data.

## Warning!

This package is not yet ready for production. It is NOT a working port of the Lisp version yet.

## Sequence

1. `ichiran-data download` - get JMDict XML + Kanjidic2 XML
2. `ichiran-data init-db` - create schema (drops tables!)
3. `ichiran-data load-jmdict` - main entries (~200k)
4. `ichiran-data load-kanjidic` - kanji readings
5. `ichiran-data load-conjugations` - verb/adj conjugations
6. `ichiran-data load-secondary-conjugations` - additional forms
7. `ichiran-data load-custom --extra` - geographic names
8. `ichiran-data apply-errata` - fix known issues
9. `ichiran-data best-readings` - calculate best readings
10. `ichiran-data stats` - show counts

## Flags

- `--max N` - limit entries (testing)
- `--force` - re-download files
- `--path <file>` - custom XML path
- `--no-download` - skip auto-download
- `--extra|--municipality|--ward` - custom data type

## Safety

`init-db` blocks if database is named `jmdict` (production). Use `jmdict_test` or similar.

