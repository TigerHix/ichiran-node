# Mac native handoff

The Apple host consumes the same multilingual release and Rust analyzer as the
browser and Node hosts. The current native boundary is C ABI version 7; format-1
`details.bin` stores and ABI-v5 binaries are intentionally unsupported.

The complete build, integration, and qualification procedure lives in
[`apple/README.md`](../../apple/README.md). The data/store design is documented in
[`docs/MULTILINGUAL-DICTIONARY-ARCHITECTURE.md`](../../docs/MULTILINGUAL-DICTIONARY-ARCHITECTURE.md).

## Native boundary

The host verifies and installs:

```text
manifest.json
hot.bin
lexicon.bin
gloss.en.bin
gloss.zh-Hans.bin
```

`hot.bin` is opened once by `ichiran_kernel_open`. The lexicon and locale files stay
file-backed: open each resident index with the `ichiran_lexicon_*` or
`ichiran_locale_*` functions, then satisfy the tagged ranges returned by legacy or
token-details operations. A locale store is accepted only when its embedded lexicon
SHA-256, locale tag, and entry count match the open lexicon. English is both the
default locale and the per-field fallback for missing translated glosses or notes.

Import [`ichiran_kernel.h`](./include/ichiran_kernel.h) through the generated
XCFramework module map and reject the library unless
`ichiran_kernel_abi_version()` equals `ICHIRAN_KERNEL_ABI_VERSION`. Swift owns pack
installation, range reads, and handle lifetimes; it must not reproduce analyzer,
dictionary-localization, fallback, or presentation logic.

## Build and qualify

```sh
apple/scripts/build-xcframework.sh
apple/scripts/run-tests.sh /absolute/path/to/release \
  --same-pack --source-lock data/source-compiler-update-2026-09-02.lock.json
```

The XCFramework build audits all exported symbols and Apple slices. Qualification
exercises English and Simplified Chinese lookup, fallback, analysis, romanization,
restart, corruption recovery, concurrent calls, and exact buffer/handle ownership.
The release must be built from the clean commit being qualified so its
`manifest.sourceCommit` matches that checkout.
