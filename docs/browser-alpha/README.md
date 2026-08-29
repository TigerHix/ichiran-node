# Browser Analyzer Alpha

The alpha produces an installable browser analyzer that uses no PostgreSQL, Node.js
service, or network lookup after its one-time data installation. PostgreSQL remains the
frozen build-time oracle for the first compiler.

The accepted scope is:

- a zero-dependency portable analyzer over immutable binary data;
- a Node-only compiler owned by `@ichiran/data`;
- a dedicated browser Worker and OPFS installer;
- a mobile-first PWA derived from Nemu's token and detail interaction model;
- top-N, entity hints, romanization, full offline dictionary details, and a legacy
  serializer;
- exact normalized oracle parity except for reviewed, checked-in morphology corrections.

Grammar/GiNZA/Bunpro, full Kanjidic character data, Komi/Nemu integration, automatic
updates, SQL emulation, and mandatory WASM are outside this milestone.

## Gates

| Metric | Required |
|---|---:|
| Compressed one-time transfer | no more than 25 MiB |
| Installed analyzer data | no more than 64 MiB |
| Resident hot image | no more than 24 MiB |
| Ordinary top-one p95 at 6x CPU throttle | no more than 75 ms |
| Pathological morphology p95 at 6x CPU throttle | no more than 250 ms |
| Main-thread analyzer work | none |

Actual iPhone 13-class validation is a production gate after this alpha. The alpha uses
repeatable desktop Chromium runs at 6x CPU throttling as the agreed provisional proxy.

## Baseline

The branch starts from `ba1966a0699e4aec9b5cfe2f18b448c21adcc590`. The oracle metadata
is frozen in `oracle.json`. The in-scope clean-checkout baseline is:

```text
@ichiran/core: 824 pass, 2 skipped, 0 failed
@ichiran/data: 20 pass, 0 failed
@ichiran/cli: 409 parity cases present and skipped unless RUN_PARITY_TESTS=true
```

Run database-backed baseline tests against the local oracle with:

```bash
ICHIRAN_DB_URL='postgresql:///ichiran_test?host=%2Fvar%2Frun%2Fpostgresql' \
  bun test --timeout 30000 --max-concurrency 1 packages/core/tests
```

The unrelated root build/typecheck is already red on the baseline because the API package
imports grammar exports that are absent on `main`. Browser-alpha commands must remain
independently green and must not absorb that grammar repair.
