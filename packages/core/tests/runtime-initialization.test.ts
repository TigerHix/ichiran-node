import { describe, expect, test } from 'bun:test';

describe('Rust WASM initialization', () => {
  test('recovers from a failed first initialization attempt in an isolated process', () => {
    const runtimeUrl = new URL('../src/runtime.ts', import.meta.url).href;
    const script = `
      import { readFile } from 'node:fs/promises';
      import { ANALYZER_WASM_URL, Analyzer } from ${JSON.stringify(runtimeUrl)};
      const lexicon = {
        byteLength: 96,
        async read(_offset, byteLength) { return new Uint8Array(byteLength); }
      };
      const english = {
        byteLength: 128,
        async read(_offset, byteLength) { return new Uint8Array(byteLength); }
      };
      const source = {
        hot: new Uint8Array(),
        lexicon: { source: lexicon, sha256: '00'.repeat(32) },
        locales: { en: english }
      };
      let firstRejected = false;
      try {
        await Analyzer.open({ ...source, wasm: Uint8Array.of(0) });
      } catch {
        firstRejected = true;
      }
      if (!firstRejected) throw new Error('bad WASM unexpectedly initialized');
      try {
        const wasm = new Uint8Array(await readFile(ANALYZER_WASM_URL));
        await Analyzer.open({ ...source, wasm });
        throw new Error('invalid pack unexpectedly opened');
      } catch (error) {
        if (!(error instanceof Error) || error.code !== 'invalid-pack') throw error;
      }
    `;
    const result = Bun.spawnSync([process.execPath, '--eval', script], {
      stdout: 'pipe',
      stderr: 'pipe'
    });
    expect(result.exitCode).toBe(0);
    expect(result.stderr.toString()).toBe('');
  });
});
