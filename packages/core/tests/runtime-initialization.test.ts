import { describe, expect, test } from 'bun:test';

describe('Rust WASM initialization', () => {
  test('recovers from a failed first initialization attempt in an isolated process', () => {
    const runtimeUrl = new URL('../src/runtime.ts', import.meta.url).href;
    const script = `
      import { readFile } from 'node:fs/promises';
      import { IchiranRuntime, RUST_KERNEL_WASM_URL } from ${JSON.stringify(runtimeUrl)};
      const details = {
        byteLength: 96,
        async read(_offset, byteLength) { return new Uint8Array(byteLength); }
      };
      let firstRejected = false;
      try {
        await IchiranRuntime.open({ hot: new Uint8Array(), details, wasm: Uint8Array.of(0) });
      } catch {
        firstRejected = true;
      }
      if (!firstRejected) throw new Error('bad WASM unexpectedly initialized');
      try {
        const wasm = new Uint8Array(await readFile(RUST_KERNEL_WASM_URL));
        await IchiranRuntime.open({ hot: new Uint8Array(), details, wasm });
        throw new Error('invalid pack unexpectedly opened');
      } catch (error) {
        if (!(error instanceof Error) || error.code !== 'invalid-header') throw error;
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
