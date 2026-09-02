# @ichiran/node

Node.js host adapter for `@ichiran/core`. It reads one immutable analyzer release,
checks the manifest and compressed/installed SHA-256 identities, and opens the shared
core runtime. Hot data is decompressed into memory. Details are verified into a
temporary file and read by exact range so the complete detail store is never resident.

```ts
import { openNodeRuntime } from '@ichiran/node';

const runtime = await openNodeRuntime('/absolute/path/to/analyzer-release');
try {
  console.log(await runtime.romanize('今日はいい天気です'));
} finally {
  runtime.dispose();
}
```

Without an explicit path, `openNodeRuntime()` reads `ICHIRAN_PACK_DIR`. The directory
must contain:

```text
manifest.json
hot.bin.gz
details.bin.gz
```

This package also exports `romanizeWithInfo`, the Node-facing formatter for the
historical info output, and analyzer entity-hint types.

`dispose()` releases WASM resources and removes the runtime's verified temporary
detail file. Call it only after outstanding analyzer operations have completed.

Analyzer lookup, morphology, scoring, top-N selection, details, romanization, and
legacy serialization all remain in `@ichiran/core`. This package owns only Node I/O,
gzip decoding, release verification, and the compatibility info presentation. It has
no PostgreSQL dependency.
