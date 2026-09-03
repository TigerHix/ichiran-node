import { describe, expect, test } from 'bun:test';
import { readFile } from 'node:fs/promises';

import * as core from '../src/index.js';
import { romanizeWord as compilerRomanizeWord } from '../src/compiler.js';
import { TypeScriptOracleRuntime } from '../src/qualification.js';

describe('core public entry points', () => {
  test('keeps TypeScript analyzer execution on the qualification-only entry point', () => {
    expect(Object.keys(core).sort()).toEqual([
      'ANALYZER_PACK_VERSION_MAX_UTF8_BYTES',
      'ANALYZER_RELEASE_FORMAT_VERSION',
      'AnalyzerInputError',
      'DetailStoreError',
      'IchiranRuntime',
      'MAX_ANALYZER_ENTITIES',
      'MAX_ANALYZER_ENTITY_ABS_BOOST',
      'MAX_ANALYZER_LIMIT',
      'MAX_ANALYZER_TEXT_LENGTH',
      'MAX_ANALYZER_WORD_LENGTH',
      'PORTABLE_LEGACY_INFO',
      'RUST_KERNEL_WASM_URL',
      'analyzerManifestDigestInput',
      'joinRomanizedParts',
      'parseAnalyzerReleaseManifest',
      'validateAnalyzerEntities',
      'validateAnalyzerLimit',
      'validatePortableAnalyzeRequest'
    ]);
    expect(typeof compilerRomanizeWord).toBe('function');
    expect(typeof TypeScriptOracleRuntime.open).toBe('function');
  });

  test('keeps the executable TypeScript detail reader out of the production graph', async () => {
    const [root, runtime] = await Promise.all([
      readFile(new URL('../src/index.ts', import.meta.url), 'utf8'),
      readFile(new URL('../src/runtime.ts', import.meta.url), 'utf8')
    ]);
    expect(root).not.toContain("from './details.js'");
    expect(runtime).not.toContain("from './details.js'");
    expect(root).not.toContain("from './analyzer-result.js'");
    expect(runtime).not.toContain("from './analyzer-result.js'");
    expect(root).not.toContain("export * from './runtime.js'");
    expect(root).not.toContain("export * from './release-manifest.js'");
  });
});
