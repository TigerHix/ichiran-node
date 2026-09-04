# Source compiler and PostgreSQL migration oracle

`@ichiran/data` builds pack-format-v2 releases directly from pinned JMdict,
Tomoshi Simplified Chinese data, the Komi zh-Hans sense-info catalog, Kanjidic2,
custom XML/CSV, conjugation CSVs, chronological errata, and a small
compatibility ledger. The normal compiler does not connect to PostgreSQL. See
[the source-compiler and Rust-kernel roadmap](../../docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

## Status

The source compiler is the active release producer. Exact inputs and hashes live in
`data/source-compiler-sources.lock.json`; acquisition and release instructions are in
`docs/source-compiler/M2-SOURCES.md` and `M6-RELEASE-WORKFLOW.md`. Kanjidic is used
only to resolve analyzer-required easy hints and is not exposed as a runtime API.
The separately named `data/source-compiler-historical-v1-sources.lock.json` exists
only to keep the retained format-1 parity evidence verifiable; the active compiler
accepts only source-lock format 2 and therefore cannot select it.

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

JMdict `s_inf` text belongs to the locale stores, not to the language-neutral
lexicon. English comes directly from JMdict. Simplified Chinese first uses reviewed
exact-source translations from `data/locales/zh-Hans/sense-info.json`, then the
versioned closed-pattern compiler policy for mechanically safe editorial forms, and
otherwise falls back to English. The compiler merges the result into Tomoshi's
zh-Hans layer immediately before locale-store encoding. No translation engine runs
in the product.

Generate the deterministic, context-rich queue for remaining translation and LQA work:

```bash
bun run source:zh-hans-info-worklist work/zh-hans-sense-info-worklist.json
```

Generate the full clustering, coverage, collision, and Codex translator/reviewer
queues against the same policy that production compilation uses:

```bash
bun run source:zh-hans-info-lqa work/zh-hans-sense-info-lqa.json
```

Translation promotion is an authoring-only, digest-bound workflow. A strict
candidate artifact records Codex/OpenAI translator metadata, uncertainty, and a
hash for every current JMdict context. Its separate decision artifact may be
signed by either a named human reviewer or an independent Codex/OpenAI review
run. Codex translation and review run IDs must differ. Known Apple or external-MT
draft paths and metadata are rejected before ingestion.

Existing rich Codex add/review files can be converted without interpreting or
changing their translations. The adapter recomputes context hashes from pinned
canonical entries and writes an input/output digest receipt:

```bash
bun run source:zh-hans-info-adapt --mode add \
  --rich-candidates work/codex-candidates.json --rich-review work/codex-review.json \
  --translator-model MODEL --translator-run-id TRANSLATOR_RUN --translated-at ISO_TIME \
  --reviewer-model MODEL --reviewer-run-id REVIEWER_RUN --reviewed-at ISO_TIME \
  --out-candidates work/strict-candidates.json \
  --out-decisions work/strict-decisions.json --out-receipt work/adaptation-receipt.json
```

Use `--mode revisions` without `--rich-candidates` for the existing-catalog
review shape. Only explicit, non-rule-resolved revisions become candidates;
retained approvals and exclusions remain in the receipt.

The adapter accepts both the original grammar candidate/review shape and the
register/freeform candidate plus `reviewOf`/`closure` reviewer shape. Shape
dispatch is strict; neither path interprets or rewrites translation content.

When independently reviewed batches share the same pinned source identities,
combine them before the catalog merge. The combine step checks every decision
digest, requires disjoint sources and distinct Codex run IDs, and records all
input artifact digests in the new combined origin:

```bash
bun run source:zh-hans-info-combine \
  --pair work/batch-a-candidates.json work/batch-a-decisions.json \
  --pair work/batch-b-candidates.json work/batch-b-decisions.json \
  --out-candidates work/combined-candidates.json \
  --out-decisions work/combined-decisions.json
```

The separate merge command accepts only validated decisions and uses explicit
outputs, leaving the source-locked catalog untouched unless that path is
deliberately selected:

```bash
bun run source:zh-hans-info-review \
  --candidates work/strict-candidates.json --decisions work/strict-decisions.json \
  --out-catalog work/next-sense-info.json \
  --out-provenance work/next-sense-info-provenance.json
```

Review provenance is emitted in normalized format 2. Candidate-digest-keyed
batch records hold the shared corpus identities, artifact origins, translator,
and reviewer once; compact decision rows hold only per-source targets, action,
decision, uncertainty, and rationale. Context arrays remain in the immutable
digest-bound candidate artifact and are reproducible from the pinned JMdict.
The reader migrates historical format-1 provenance in memory for replay checks,
but never emits the repeated format-1 layout.

After every approved batch has been merged, normalize the resulting catalog in
a separate file. This removes exact rows whose targets are now reproduced by
the deterministic policy and hard-fails if a catalog target disagrees with that
policy; it never silently chooses one translation over the other:

```bash
bun run source:zh-hans-info-catalog-normalize \
  --catalog work/next-sense-info.json \
  --out-catalog work/next-sense-info-normalized.json
```

The command also applies the already-reviewed `after an amount` target update
before proving it matches the finite attachment rule and pruning that now
redundant row. The update is emitted separately in the command statistics.

Run normalization only after the atomic combined-review merge, then repin the
catalog byte count and SHA-256 in every active source lock.

The release report records exact-catalog and deterministic-rule occurrence and
unique-string coverage, per-rule counts, fallbacks, and unused catalog records. The
catalog itself is a hashed source-lock input.

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
9. `load-kanjidic` - Load kanji data from the ignored live-data cache, downloading it when absent (~2 sec)
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
