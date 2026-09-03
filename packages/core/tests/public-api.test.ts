import { describe, expect, test } from 'bun:test';
import { readFile } from 'node:fs/promises';

import * as core from '../src/index.js';
import * as release from '../src/release.js';
import { romanizeWord as compilerRomanizeWord } from '../src/compiler.js';
import { TypeScriptOracleRuntime } from '../src/qualification.js';

describe('core public entry points', () => {
  test('keeps TypeScript analyzer execution on the qualification-only entry point', () => {
    expect(Object.keys(core).sort()).toEqual([
      'ANALYZER_WASM_URL',
      'Analyzer',
      'AnalyzerError'
    ]);
    expect(typeof release.parseAnalyzerReleaseManifest).toBe('function');
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
    expect(root).not.toContain('PortableLegacy');
    expect(runtime).not.toContain('legacy_begin_utf16');
  });
});
