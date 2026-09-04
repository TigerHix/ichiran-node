# @ichiran/node

The Node filesystem adapter for `@ichiran/core`. Its only export is `openAnalyzer`.
It verifies the release manifest and every compressed and installed SHA-256 identity,
then opens the same Rust/WASM analyzer used in the browser.

```ts
import { openAnalyzer } from '@ichiran/node';

const analyzer = await openAnalyzer('/absolute/path/to/analyzer-release');
try {
  console.log(await analyzer.romanize('今日はいい天気です'));
  console.log(await analyzer.analyze('今日はいい天気です', { limit: 3 }));
  console.log(await analyzer.details('猫', {
    pathIndex: 0, tokenIndex: 0, locale: 'zh-Hans'
  }));
} finally {
  analyzer.dispose();
}
```

Without an argument, `openAnalyzer()` reads `ICHIRAN_PACK_DIR`. A deployment may set
`ICHIRAN_SOURCE_COMMIT` to reject a release built for different analyzer code. The
directory contains `manifest.json` plus the hot, lexicon, and locale assets named by it.

Hot data is decoded in memory. Lexicon and locale data are verified into temporary
files and served by exact ranges; `dispose()` removes those files. Analyzer behavior, result types, and
errors are owned by `@ichiran/core`. This package contains no formatter, legacy
serializer, grammar behavior, or PostgreSQL dependency.
