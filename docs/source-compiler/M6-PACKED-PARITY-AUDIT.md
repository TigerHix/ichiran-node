# M6 packed analyzer parity audit

## Scope and decision

This audit compares the completed source-native pack at
`work/m6-source-release-final` with the qualified
`portable-core-260118-baseline` pack at `work/m2-baseline`. It covers every
non-raw-equal result in the 401 ordinary and hard full-JSON fixtures. It does
not authorize a case list in the runtime or release gate.

The result is **PASS with reviewed ordering deltas**. There are no missing
semantic locators, reverse-morphology results, direct candidates, or generated
candidates in any affected class. All 32 source-versus-qualified fixture
occurrences are consequences of the two already-approved order boundaries:

- 16 occurrences (14 unique requests) differ only within contiguous
  equal-score path or alternative runs. The existing canonical comparator
  proves these outputs equal.
- 16 occurrences (13 unique requests) expose a changed direct order,
  generated physical partition, or generated winner. Every one maps to a row
  in the direct-order or exhaustive generated-order proof below.

No unrelated analyzer regression was found. The existing packed-parity command
still reports two failing aggregate full-JSON tests because it intentionally
accepts only the first category; this audit does not weaken that comparator or
add an allowlist.

## Pack and proof identities

| Artifact | Bytes | SHA-256 |
| --- | ---: | --- |
| Source `hot.bin.gz` | 12,606,987 | `0e1028bb7b32f9b23a2bc43b2e1d8469e826cb8304bd5e3a8aaa10462892fdd6` |
| Qualified `hot.bin.gz` | 12,662,917 | `35d02c84d4cc531d299d7d5530994351b75bdba429d5276c20bc2f67cdc8d6d7` |
| Source `details.bin.gz` | 12,317,325 | `ad10bc4876d9a05224f62f5b438080ea1ff4e6a88ab3090be0f871035e95918a` |
| Qualified `details.bin.gz` | 12,317,325 | `ad10bc4876d9a05224f62f5b438080ea1ff4e6a88ab3090be0f871035e95918a` |
| Direct-order attestation | 1,295 | `12ca177bf7765e4337f3c1cc4d836a7bcfc84b3f60b08e07d6eb238ad72dc4cf` |
| Direct-order full rows | 4,076,458 | `5f4660a0afbc1a21021f3c4db49014554a3b7991a48960c2238a050ae05a1854` |
| Generated-order attestation | 8,858 | `3ecb9af387502836b45a98a6570bbaadeaf0ba2a0dc530928bcdeae1d7ae36c1` |
| Generated-order report | 44,166 | `239e02ed657f739adb9808816198e96bf81031b66872e9f36c881140ac354454` |
| Generated-order full rows | 39,744,311 | `6160662ad10a4c4dade2fef1b11dbfb689cf4f55ab225862f6b77435be2c708e` |

The source and qualified detail sections are byte equal. Surface index and
morphology are also byte equal. The release gate separately pins the complete
source and qualified analyzer-support and analyzer-annotations identities and
count groups in `data/source-compiler-generated-order-attestation.json`.

The generated proof compares 212,198 surfaces: 173,111 exact, 9,799 with a
physical grouping change, and 29,288 with ordering only. Across 548,607
locators on each side, source-only, qualified-only, reverse-source-only, and
reverse-packed-only counts are all zero. Missing ranks, rank conflicts,
incomplete rank partitions, and collision-status changes are also zero.

## Fixture totals

| Pack and fixture set | Raw exact | Canonical-order only | Other mismatch | Total |
| --- | ---: | ---: | ---: | ---: |
| Qualified ordinary | 250 | 2 | 0 | 252 |
| Qualified hard | 148 | 1 | 0 | 149 |
| Source ordinary | 230 | 10 | 12 | 252 |
| Source hard | 137 | 8 | 4 | 149 |
| Source total | 367 | 18 | 16 | 401 |

Directly comparing the two packed runtimes yields 369 raw-equal fixture
occurrences and 32 changed occurrences. Two canonical-order-only source results
are already byte/order identical to the qualified runtime even though both
packs differ canonically from the captured Lisp output:
`それいっちゃうとぜんぶおわっちゃうんじゃないのかな。|1` and
`いっといてくれればよかったとおもわざるをえない|1`. This accounts
for the apparent 34 source non-raw fixture results versus 32 actual pack deltas.

Romanization is 5/5 exact, info is 3/3 exact, and the pinned upstream regression
set is 27/27 exact under both packs.

## Complete oracle corpus

The final gate also runs the complete oracle harness against the source-built
pack. Its 1,241 chosen-authority operations comprise 940 current-Lisp snapshots
and 301 operations whose frozen PostgreSQL result is the qualified authority.
Those 301 operations are then compared a second time through the independent
clean-semantic representation, for 1,542 total comparisons.

After the compatibility-only `じゃない` split-ordinal repair, the chosen-authority
result is 1,225 byte exact plus 16 individually reviewed source-order results;
the clean-semantic fallback result is 296 exact plus five individually reviewed
source-order results. There are no errors or unreviewed differences. The 21
reviewed rows are diagnostic evidence only: the analyzer and release contain no
request list, exception table, or runtime allowlist.

The full report binds the tested release to its source-compiler lock while the
reference side remains bound independently to the frozen browser-alpha oracle
lock. Reproduce it with:

```sh
bun packages/core/tools/oracle-parity.ts \
  --repository "$PWD" \
  --release work/m6-source-release-final \
  --database "$ICHIRAN_DB_URL" \
  --source-compiler-pack \
  --allow-failures \
  --out work/m6-evidence/source-pack-oracle-parity-full.json \
  --samples 1241
```

`--allow-failures` makes this one diagnostic run finish and retain every concrete
difference; it does not accept those differences anywhere else. Qualification
requires the retained review to account for every non-exact row and to report an
empty runtime allowlist. Running the same command without that flag is expected
to exit nonzero because the approved source-order rows remain intentionally
observable.

## Canonical equal-score ordering rows

For every row below the raw packed outputs differ, but
`firstCanonicalDifference(qualified, source)` is null. The legacy skeleton and
the candidate and conjugation-metadata multisets are exact. `O` means ordinary,
`H` means hard, and `O+H` means the same request occurs in both sets.

| Fixture | Request key |
| --- | --- |
| O+H | `コンビニのおにぎりに書いてある開け方が一番むずい。\|1` |
| O+H | `紙コップのコーヒーがやけに深刻な味のときは相談相手が欲しい。\|1` |
| O | `非可逆圧縮音源高解像度化風味の謎が解けない。\|1` |
| O | `あとからならべなおせばきれいになるからいまはなりゆきでいこう。\|1` |
| O | `こぼれるまえにふいておけばよかったのにぼうっとしてた。\|1` |
| O | `しってるつもりでいたのにきいてみたらぜんぜんちがってた。\|1` |
| O | `ひらけたらすぐみせてくださいってつたえておいて。\|1` |
| O | `ざっくりきめとこうっていってたのにやたらこまかくなった。\|1` |
| O | `ぜんぜんきいてないっていわれがちだけどじつはきいてる。\|1` |
| H | `やらないわけにはいかなくなってきた\|1` |
| H | `しないではいられないわけでもないのにしちゃう\|1` |
| H | `しておけばよかったとおもわずにはいられない\|1` |
| H | `してくれないわけでもないがしてはくれない\|1` |
| H | `しなければならないわけでもないこともない\|1` |

## Observable direct and generated ordering rows

These seven unique requests account for ten fixture occurrences because the
first three occur in both fixture sets. Their candidate and conjugation
metadata multisets are exact; only the displayed reading order changes. The
proof pointer identifies the complete source and qualified candidate orders.

| Fixture | Request | Observable change | Proof row and row SHA-256 |
| --- | --- | --- | --- |
| O+H | `机の角に足の小指をぶつける確率は宇宙の悪意だ。` | `kado/kaku/tsuno` → `kado/tsuno/kaku` | `direct-order.jsonl:3327`, `093339b9abcece34aa74312436cf0882a90f9dc9d45d7ffe54e6e3ce032c5afb` |
| O+H | `午前二時のポエムは午前九時の現実に勝てない。` | `futatoki/niji` → `niji/futatoki` | `direct-order.jsonl:2847`, `7f57276234388b214babd20daecd11b5c8212e5730fcfe09aee0237d9502f221` |
| O+H | `窓際の席だけが世界に開いている。` | `hiraiteiru/aiteiru` → `aiteiru/hiraiteiru` | `full-locator-universe-final-v2.ndjson:37922` (`開いて`), `05fadaf1e4a1ec0f86d5581634fce70d060c38f59b751c710fd89345a215f36c` |
| O | `朝活仲間募集掲示板荒らし対策会議を開いた。` | `hiraita/aita` → `aita/hiraita` | `full-locator-universe-final-v2.ndjson:37919` (`開いた`), `27b5bdebf4f600a948f3135e302bd56bfce5989d10102ee3cb8ae31809c771b6` |
| O | `バロック音楽即興演奏基礎練習会を開いた。` | `hiraita/aita` → `aita/hiraita` | same complete `開いた` row above |
| O | `湿度高め設定推奨理由説明不足感が否めない。` | `inamenai/iyamenai` → `iyamenai/inamenai` | `full-locator-universe-final-v2.ndjson:26258`, `4cf690139aeb109ddecb468ee7d425fe4d94b01a763463ff312b8fe704bb8b65` |
| O | `囲碁定石外し実戦的研究会資料を配布した。` | `teki/mato` → `mato/teki` | `direct-order.jsonl:3235`, `e1510ae53a7430ac035f160c2c07ff2fd13e9da06d13f169b3a351a74aa19f96` |

The three generated rows have `groupingChanged=false`,
`orderingChanged=true`, and `winnerChanged=true`. Every locator-only array is
empty. The three direct rows contain the same serialized candidate records on
both sides and differ only in order.

## Observable physical partition and winner rows

These six requests each map to one exhaustive generated-order row. The row
contains the complete source and qualified class partitions, not only a count.
Every row has empty source-only, qualified-only, reverse-source-only, and
reverse-packed-only locator arrays.

| Fixture | Request | Exact cause | Proof row and row SHA-256 |
| --- | --- | --- | --- |
| O | `おちついてはなせばわかるはずなのにかおをみるといえなくなる。` | `いえ`: qualified groups locator `1008860/84` with `1587040/771` and `1587040/796`; source keeps them in separate physical classes | `full-locator-universe-final-v2.ndjson:800`, `7f503c07c8448b4d2dd70b68f6e2a34f6d6360fa3ef2b01f5af0911df4b49094` |
| O | `ぬれたままかばんにいれっぱなしにするのはよくない。` | `よくない`: source groups the `良い` and `良くある` locators, yielding one additional equal-score alternative | `full-locator-universe-final-v2.ndjson:22296`, `a583b2abe89fa26548f56236554ffbc0103ecccd6ed7c10b24a9c6f29418e550` |
| O | `きくにきけないことをメッセでそっとおくってみた。` | `きけない`: source groups `聞く` potential-negative with `聞ける` nonpast-negative | `full-locator-universe-final-v2.ndjson:6203`, `bb858df7f13384d72a18ec86495ad248462f8fcb005e263048e9ec8ffa39d5f2` |
| O | `すこしおちつけばみえてくるはずだとじぶんにいいきかせた。` | `みえて`: source groups the `見える` and `視える` conjunctive locators | `full-locator-universe-final-v2.ndjson:20581`, `eaba80f3538445c3dbbaef15f83a0aacdeea104b3c9b06d4e84e2c1cfa097473` |
| O | `既読つけない術を覚えたら返信タイミング迷子。` | `つけない`: source separates the three locators that qualified grouped, yielding three alternatives instead of one | `full-locator-universe-final-v2.ndjson:13717`, `f1774809ee12bea5460ff1b1c03ac636ac843c2f1894e952889c39c323c3eca3` |
| H | `しないでおこうとしてもしないでいられない` | `いられない`: class membership is the same; source-native order selects the passive-negative `居る` class before the qualified potential-negative class | `full-locator-universe-final-v2.ndjson:1576`, `88b12c6fa76e11c3ab1ee87b9741b694e3ac39bd8a1c48fbf3cffd381a028738` |

The first five rows are physical-partition consequences. The last is a
winner-order consequence. They are observable analyzer differences, but not
new semantic candidates or omissions: their complete locator universes are
identical and they are members of the already-reviewed 9,799 grouping and
35,306 winner rows.

## Reproduction

Run the existing packed gate against each immutable directory:

```sh
ICHIRAN_PACK_DIR=work/m2-baseline bun scripts/packed-parity.ts
ICHIRAN_PACK_DIR=work/m6-source-release-final bun scripts/packed-parity.ts
```

Reproduce the complete direct and generated proof evidence with the commands in
`M6-CANONICAL-ROOTS.md` and `M6-GENERATED-SUPPORT.md`. The qualified control
gate took 11.69 seconds, peaked at 465,172 KiB RSS, and used no swaps. The source
gate took 11.16 seconds, peaked at 486,088 KiB RSS, and used no swaps.
