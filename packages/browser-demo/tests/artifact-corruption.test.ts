import { describe, expect, test } from 'bun:test';

import { isArtifactCorruption } from '../src/worker/artifact-corruption.js';

function codedError(name: string, code: string): Error {
  const error = new Error('runtime failure') as Error & { code: string };
  error.name = name;
  error.code = code;
  return error;
}

describe('Worker artifact corruption classification', () => {
  test('quarantines only explicit immutable header, checksum, and payload failures', () => {
    for (const [name, code] of [
      ['RustKernelError', 'invalid-header'],
      ['RustKernelError', 'unsupported-version'],
      ['RustKernelError', 'corrupt-section'],
      ['RustKernelError', 'corrupt-payload'],
      ['RustKernelError', 'corrupt-index'],
      ['RustKernelError', 'corrupt-block'],
      ['PackFormatError', 'invalid-directory'],
      ['DetailStoreError', 'corrupt-block']
    ]) {
      expect(isArtifactCorruption(codedError(name, code))).toBe(true);
    }
  });

  test('does not quarantine Rust internal or caller failures', () => {
    expect(isArtifactCorruption(codedError('RustKernelError', 'internal'))).toBe(false);
    expect(isArtifactCorruption(codedError('RustKernelError', 'out-of-range'))).toBe(false);
    expect(isArtifactCorruption(codedError('RustKernelError', 'invalid-input'))).toBe(false);
  });

  test('does not quarantine module, WASM shell, or storage read failures', () => {
    expect(isArtifactCorruption(new TypeError('dynamic import failed'))).toBe(false);
    expect(isArtifactCorruption(new WebAssembly.CompileError('WASM instantiate failed'))).toBe(false);
    expect(isArtifactCorruption(new DOMException('OPFS unavailable', 'NotReadableError'))).toBe(false);
    expect(isArtifactCorruption(codedError('Error', 'corrupt-block'))).toBe(false);
    expect(isArtifactCorruption('corrupt-block')).toBe(false);
  });
});
