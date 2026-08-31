# Portable analyzer remediation checklist

This checklist turns the thermo-nuclear review into explicit implementation and
acceptance work. An item is complete only when the behavior is implemented and
the named proof passes. Passing parity alone is not enough; resource safety,
failure recovery, release provenance, and browser lifecycle behavior are product
requirements too.

## Non-negotiable invariants

- Analyzer output remains exactly compatible with the pinned Ichiran oracle.
- PostgreSQL and compiler-only packages never enter browser or production runtime artifacts.
- Grammar remains out of scope and excluded from the analyzer product.
- The browser keeps all analyzer work off the main thread and remains fully usable offline.
- The installed data pack and the code reading it have one verifiable, compatible identity.
- Common phone-sized requests stay instant-feeling; longer input degrades predictably rather than catastrophically.
- Important state has one owner: analyzer options, release manifests, install generations, and database provenance are not redefined by each adapter.

## Mental checklist for every change

1. **Name the invariant.** What exact behavior, byte identity, latency bound, or lifecycle rule must remain true?
2. **Reproduce the failure first.** Prefer a deterministic unit/integration test over a comment or defensive branch.
3. **Fix the canonical boundary.** Validate or normalize once in core; adapters may narrow further but must not invent competing contracts.
4. **Keep state ownership obvious.** One request context, one active Worker generation, one installed-pack commit record, one manifest parser.
5. **Avoid hidden unbounded work.** Check input length, collection cardinality, numeric ranges, nested loops, path copying, decompression, and retry behavior.
6. **Preserve stable parity semantics.** Tie ordering, top-N truncation, annotations, entity boosts, and legacy presentation need explicit regression coverage.
7. **Make failure atomic.** A crash or interruption must expose either the old valid generation or the new valid generation, never a mixture.
8. **Test generation changes.** Exercise old page/new Service Worker, old pack/new code, reinstall, corruption, and restart—not only clean first install.
9. **Measure scaling, not one happy-path percentile.** Include short, paragraph-sized, pathological, top-N, and adversarial option shapes.
10. **Verify the rendered product.** Build/typecheck is insufficient for UI work; inspect console health and exercise the flow at desktop and phone viewports.
11. **Delete displaced complexity.** A refactor is complete when the old parser, state path, compatibility shim, or giant responsibility is removed.
12. **Keep claims executable.** Documentation may only claim checks that a command actually performs from measured inputs.

## A. Analyzer kernel and resource safety

- [x] Define canonical `AnalyzeOptions` bounds for top-N, text length, entity count/spans, and finite boost values.
- [x] Enforce those bounds in core so Node, browser Worker callers, and future hosts share the same safety contract.
- [x] Return clear 4xx errors from the HTTP adapter for invalid or oversized analyzer requests.
- [x] Add regression tests proving huge `limit` values cannot allocate proportional arrays or terminate the process.
- [x] Replace eager full-path copying in `findAnalyzerPaths` with compact backpointers and final-only materialization.
- [x] Preserve exact stable tie ordering, entity behavior, transition adjustments, gaps, and top-N output.
- [x] Add deterministic operation/allocation-shape coverage plus real-pack long-input benchmark evidence.
- [x] Extend the benchmark corpus beyond the current short-input ceiling so paragraph scaling remains visible.
- [x] Split `PortableAnalyzer` into cohesive request, candidate/materialization, scoring/path, and output-projection ownership without creating framework machinery.

## B. Browser lifecycle and interaction polish

- [x] Give Service Worker generations an explicit activation/update handshake.
- [x] Do not delete a shell cache while a live client can still request that generation's hashed assets.
- [x] Add an upgrade E2E covering an old tab, a new deployment, lazy chunk loading, reload, and offline reopen.
- [x] Make Analyzer Worker failure terminal-and-explicit or recreate the Worker; never leave future promises pending.
- [x] Cover a request active during crash and a second request after crash with deterministic tests.
- [x] Ensure install error recovery cannot hang while asking the failed Worker for status.
- [x] Prevent obsolete analysis requests from needlessly delaying the latest user intent, while retaining serialized install/clear mutation.
- [x] Surface update, restart, corruption, storage, and retry states with actionable UI copy and accessible live regions.
- [x] Re-run desktop and phone-layout interaction QA with clean console output.

## C. Release, manifest, and provenance integrity

- [x] Move release-manifest types, validation, and canonical digest serialization to one runtime-neutral module.
- [x] Make compiler, browser installer, Node loader, staging audit, and oracle tools consume that module.
- [x] Either support every declared encoding everywhere or remove unsupported encodings from the canonical contract.
- [x] Publish a whole immutable release generation atomically and enforce an exact file inventory.
- [x] Add interruption/fault tests proving publication cannot leave a mixed generation.
- [x] Derive the persisted-size gate from the actual finalized shell rather than a caller-asserted integer.
- [x] Bind the measured shell identity into release verification.
- [x] Compare production runtime code identity/compatibility with the staged data pack during Docker build/deploy.
- [x] Make deploy fail before upload when the release is stale, mixed, corrupt, or contains unexpected files.
- [x] Verify the fallback PostgreSQL oracle against the locked database identity before using it as authority.
- [x] Compute and compare the actual normalized schema digest; never copy the expected digest into evidence as if measured.
- [x] Share one small read-only database provenance helper between compiler, parity, and canonicalization workflows.
- [x] Split release build, verification, publication, and database acquisition out of the monolithic release CLI.
- [x] Update documentation so every provenance/atomicity/size claim maps to an executable check.

## D. Final qualification

- [x] Core, Node, CLI, API, browser, compiler, and reference typechecks pass.
- [x] Unit and integration suites pass with no resource-safety or lifecycle tests skipped.
- [x] Current-Lisp oracle comparisons and frozen-PostgreSQL fallback comparisons pass against verified sources.
- [x] Packed release is deterministic and byte-identical across two independent development builds from the same frozen tree.
- [ ] Repeat the two-build release proof from a committed clean tree without `--allow-dirty` before deployment.
- [x] Release verification, browser build audit, and exact-inventory checks pass.
- [x] Browser E2E passes install, interruption, corruption, cross-tab mutation, upgrade, restart, and zero-network analysis.
- [x] Short-input latency remains within the existing gates and long-input scaling improves materially.
- [x] Desktop and phone-view Playwright QA confirms page identity, nonblank UI, no framework overlay, clean console, and working primary interactions.
- [x] Review the final diff for new files over 1,000 lines, duplicate contracts, stale compatibility paths, and documentation claims without enforcement.

## Qualification evidence — 2026-08-31

- Default unit/integration matrix: 152 passed, 3,781 assertions, zero failures. The data release-integrity suite added 10 passing checks with 28 assertions. Optional real-pack/oracle suites were run separately; no resource-safety or lifecycle test was skipped.
- Authoritative parity: 1,241/1,241 current-Lisp comparisons exact, plus 301/301 frozen-PostgreSQL fallback clean-semantic comparisons exact. The separate PostgreSQL scorer differential passed 1,297 assertions.
- Browser: 11/11 Playwright E2Es passed in one uninterrupted run, including fresh-profile offline restart, interrupted installation, strict A/B generation recovery, both cross-tab ABA lock orders, corruption, shell upgrade, and zero-network analysis. Every page context fails on unexpected `console.error` or uncaught page errors.
- Performance under a measured 6.05x single-CPU slowdown: ordinary p95 38.9 ms (75 ms gate), pathological-morphology p95 98.6 ms (250 ms gate), and dense-contiguous-boundary p95 130.3 ms (500 ms gate). The 4,096-unit paragraph-scaling diagnostic was 408.1 ms p95. No main-thread task exceeded 50 ms.
- Release generation `0723050e2d9d95c4387db485d296f386e8caae57dcd138fef84eb670bf3640ef` reproduced across two independent output roots. Exact four-file inventory and all bytes matched: manifest `21ec02a1…123`, stats `a66f8592…025`, hot pack `35d02c84…6d7`, and details pack `ad10bc48…18a`.
- Both release roots passed standalone verification. The finalized shell measured 681,649 bytes across 18 files with cache identity `b07692702eef25a8`; the analyzer pack is 25,662,818 wire bytes and 39,096,725 persisted bytes.

## Remaining external release gates

- Commit the reviewed source and repeat the two-build proof without `--allow-dirty`; development artifacts must not be presented as deployable clean-commit provenance.
- Run the explicitly deferred physical-Safari smoke/performance check on the iPhone 13 baseline and current target device. Playwright already covers 320 px, 390 px, and desktop layouts, but it is not a physical-device substitute.
