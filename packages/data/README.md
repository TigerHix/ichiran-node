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

`@ichiran/data` is private repository-maintainer tooling. Build and run its compiler
from the same clean ichiran-node checkout whose `HEAD` is stamped into the release.
That checkout must contain the pinned `data/` inputs, the Rust surface-index compiler,
and (for baseline mode) `work/m2-baseline`. Relative output paths are resolved from the
checkout. In-repository output is confined to `work/`; fresh absolute output paths
outside the repository are also accepted. The compiler supports Linux and macOS,
plus Windows through WSL. Its surface compiler and release layout use POSIX executables
and symlinks, so native Windows is not supported.

```bash
bun run source:release -- baseline --out /tmp/ichiran-release \
  --pack-version candidate-name
```

The commands below describe the frozen PostgreSQL migration oracle. They do not
produce or replace pinned source-compiler inputs.

## Loading Sequence

Matches Lisp `load-extras` from dict-load.lisp:185-194. Total time: ~26 minutes.
Run each private command from the repository root as
`bun run migration:data -- <command>`. Preserving that working directory is part of
the frozen loader contract. These commands are not part of the compiler executable.

1. `download` - Download current live JMDict/Kanjidic2 into the ignored `work/live-data` destination
2. `init-db` - Create schema (**drops all tables!**)
3. `load-jmdict` - Load main dictionary (~214k entries, ~3 min)
4. `load-conjugations` - Generate verb/adjective conjugations (~686k forms, ~2 min)
5. `load-secondary-conjugations` - Generate compound conjugations (~1.7M forms, ~20 min)
6. `load-custom --extra --municipality --ward` - Load custom entries (geographic names, ~3.4k entries, ~1 min)
7. `apply-errata` - Apply corrections + reload :extra (~1 min)
8. `best-readings` - Calculate best_kana/best_kanji (~10 sec)
9. `load-kanjidic --path ./data/kanjidic2.xml.gz` - Load kanji data (auto-decompresses .gz, ~2 sec)
10. `stats` - Show database statistics

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
