# M2 ordering decision

On 2026-08-31 the user selected source-native deterministic precedence and
authorized an intentional rebaseline.

The permanent direct-root order is:

1. semantic mutation phase descending;
2. form creation event descending;
3. form ordinal descending;
4. entry seq descending.

The 3,436 qualified `(route,surface)` classes whose PostgreSQL `ctid DESC`
order differs remain migration evidence. They are not compiler inputs and will
not become a compatibility ledger. M6 must compare both byte and analyzer
behavior, enumerate the 3,149 changed first candidates, and review actual
user-visible analysis deltas before the new source-built pack is qualified.

This decision resolves the M2 feasibility blocker. M2 is **PASS with an approved
ordering rebaseline**; exact byte equality is still required everywhere outside
the reviewed ordering-dependent representation.
