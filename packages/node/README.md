# @ichiran/node

Node.js host adapter for `@ichiran/core`. It reads one immutable analyzer release,
checks the manifest and compressed/installed SHA-256 identities, decompresses the two
assets, and opens the shared core runtime.

```ts
import { openNodeRuntime } from '@ichiran/node';

const runtime = await openNodeRuntime('/absolute/path/to/analyzer-release');
console.log(await runtime.romanize('今日はいい天気です'));
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

Analyzer lookup, morphology, scoring, top-N selection, details, romanization, and
legacy serialization all remain in `@ichiran/core`. This package owns only Node I/O,
gzip decoding, release verification, and the compatibility info presentation. It has
no PostgreSQL dependency.
