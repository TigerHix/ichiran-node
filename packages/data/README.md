# Data Loading

Brief guide to loading JMDict/Kanjidic data.

## Warning!

This package is not yet ready for production. It is NOT a working port of the Lisp version yet.

## Sequence

Matches Lisp `load-extras` from dict-load.lisp:185-194

1. `ichiran-data download` - get JMDict XML + Kanjidic2 XML
2. `ichiran-data init-db` - create schema (drops tables!)
3. `ichiran-data load-jmdict` - main entries (~200k)
4. `ichiran-data load-conjugations` - verb/adj conjugations
5. `ichiran-data load-secondary-conjugations` - additional forms
6. `ichiran-data load-custom --extra --municipality --ward` - custom entries (all types)
7. `ichiran-data apply-errata` - fix issues (internally reloads :extra)
8. `ichiran-data best-readings` - calculate best readings
9. `ichiran-data load-kanjidic` - kanji character data (optional)
10. `ichiran-data stats` - show counts

**Note:** Step 7 internally calls `load-custom --extra` again (dict-errata.lisp:580).
This is intentional - step 6 loads all custom data, step 7 reloads :extra to pick up any errata-added entries.

## Flags

- `--max N` - limit entries (testing)
- `--force` - re-download files
- `--path <file>` - custom XML path
- `--no-download` - skip auto-download
- `--extra|--municipality|--ward` - custom data type

## Safety

`init-db` blocks if database is named `jmdict` (production). Use `jmdict_test` or similar.

