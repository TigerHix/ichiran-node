# Multilingual dictionary architecture

Ichiran uses one language-neutral Japanese analyzer and independently replaceable
definition layers. Locale selection never changes segmentation, morphology, scoring,
entry indexes, or conjugation analysis.

## Release layout

Manifest format 2 authenticates these assets:

```text
manifest.json
hot.bin.gz
lexicon.bin.gz
gloss.en.bin.gz
gloss.zh-Hans.bin.gz
stats.json
```

`hot.bin` contains the resident surface index, roots, morphology, and analyzer support.
`lexicon.bin` contains Japanese forms, sense ordinals, and coded sense properties,
including the JMdict applicability tags used as restrictions. Each
`gloss.<locale>.bin` contains localized gloss and information text.
The `locales` manifest object is keyed by validated BCP-47-like language tags, so a
future locale is an additional manifest entry and file rather than a schema change.
`en` and `zh-Hans` are required in the current release.

Every locale-store header embeds the installed SHA-256 of `lexicon.bin`. Hosts pass
that authenticated digest into the runtime when opening the stores. A locale built
against different sense ordinals is therefore rejected before any definition can be
returned.

## Runtime contract

```ts
const analyzer = await Analyzer.open({
  hot,
  lexicon: { source: lexiconSource, sha256: manifest.lexicon.installedSha256 },
  locales
});

await analyzer.details(text, {
  pathIndex: 0,
  tokenIndex: 0,
  locale: 'zh-Hans'
});
await analyzer.entry(entryIndex, { locale: 'zh-Hans' });
```

English is the default. A malformed locale is an `invalid-input` error; a valid locale
that is not installed is `not-found`.
For `zh-Hans`, missing translated senses fall back individually to English; the
Japanese lexicon record is read only once and remains the source of forms, POS,
restrictions, and sense identity. Analysis and romanization do not read either locale
store.

## Independent presentation locale

Dictionary language and application language are separate settings:

```ts
const definitionLocale = 'zh-Hans';
const uiLocale = 'en';

const details = await analyzer.details(text, {
  pathIndex: 0,
  tokenIndex: 0,
  locale: definitionLocale
});
const presentation = createPresentation(uiLocale);
```

`TokenDetails` carries dictionary prose only in `meanings[].gloss` and
`meanings[].info`. Analyzer-owned concepts remain structured: `suffixId`,
`entityKind`, conjugation type IDs, negative/formal flags, counter value/ordinal
flags, POS codes, and field codes. `@ichiran/presentation` maps those concepts to
small bundled `en` or `zh-Hans` catalogs. Selecting a UI locale never loads or
changes a dictionary asset, and selecting a definition locale never changes analyzer
terminology.

The presentation package requires exact key coverage and matching interpolation
placeholders. Its reviewed-locale record is bound to a canonical SHA-256 of the
English source catalog, so an English edit makes the Simplified Chinese LQA record
stale until translation and review are updated. Unknown future analyzer codes remain
visible as raw codes instead of silently falling back to English.

Browser and native installers download and authenticate all locale assets declared in
the manifest. Node verifies the same assets and exposes them as random-access files.
All hosts iterate the locale record rather than hard-coding storage mechanics for a
fixed language list.

## Sources and licensing

English definitions are derived from JMdict by the Electronic Dictionary Research and
Development Group. Simplified Chinese definitions are derived from
[Tomoshi Dictionary Data](https://github.com/tomoshi-app/tomoshi-dict-data) by Y1Z.
Both are used under CC BY-SA 4.0. The compiler records the pinned source identities,
alignment/fallback counts, and modifications into the release evidence and stats.

Tomoshi rows are matched by JMdict sequence and sense ordinal. A row whose English
anchor no longer matches the pinned JMdict snapshot is not guessed into place; that
sense uses English fallback. This keeps provider alignment at the compiler boundary
and keeps the runtime independent of Tomoshi's source schema.

JMdict sense information (`s_inf`) follows the same locale boundary. English notes
come from JMdict. Simplified Chinese resolution is an ordered compile-time pipeline:
the reviewed exact-source translation memory in
`data/locales/zh-Hans/sense-info.json`, then a versioned closed-pattern policy for
mechanical editorial forms, then field-level English fallback. The policy accepts
only complete, anchored forms whose variable payload is Japanese text (for example,
`read X` to `读作「X」`) or a finite reviewed grammar/place-name table. Etymology,
wordplay, and free prose are deliberately outside that policy.

The source compiler attaches resolved notes to the existing Tomoshi sense group (or
creates an info-only group), and then emits the ordinary `gloss.zh-Hans.bin`. Pattern
rules are compiler code, not runtime translation; release statistics identify the
policy version and separate exact-catalog and per-rule coverage. There is no second
runtime lookup, translation service, or new binary format.

Exact matching is the stale-source guard for semantic translations: edited JMdict
prose becomes an English field-level fallback until its reviewed translation is
updated. The generated worklist and LQA report include sequence, sense/info ordinals,
headwords, and English glosses so Codex translator and reviewer passes operate with
dictionary context. LQA also reports clusters, rule collisions, catalog/rule
disagreements, suspicious targets, and separate translator/reviewer queues.

Semantic translations use two distinct Codex runs. The translator produces
context-bound candidates; the reviewer must approve, revise, or reject every
candidate. The adapter recomputes context hashes from pinned JMdict, and the atomic
merge rejects stale inputs, incomplete decisions, reused run identities, implicit
catalog overwrites, and rule-resolved sources. Only exact source/target pairs reach
the catalog. A compact normalized provenance sidecar records batch identities and
review decisions but is never compiled into the runtime locale pack.
