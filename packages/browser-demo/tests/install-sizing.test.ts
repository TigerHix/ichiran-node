import { describe, expect, test } from 'bun:test';

import type { PackAssetManifest } from '../src/protocol.js';
import { temporaryInstallBytes } from '../src/worker/install.js';

function asset(encoding: 'identity' | 'gzip', downloadBytes: number): PackAssetManifest {
  const file = encoding === 'gzip' ? 'hot.bin.gz' : 'hot.bin';
  return {
    file,
    encoding,
    downloadBytes,
    downloadSha256: 'a'.repeat(64),
    installedBytes: downloadBytes,
    installedSha256: 'b'.repeat(64)
  };
}

describe('install workspace sizing', () => {
  test('does not reserve a second copy for identity assets', () => {
    expect(temporaryInstallBytes([
      asset('identity', 11),
      asset('identity', 29)
    ])).toBe(0);
  });

  test('reserves only the largest reusable gzip download buffer', () => {
    expect(temporaryInstallBytes([
      asset('identity', 100),
      asset('gzip', 29),
      asset('gzip', 17)
    ])).toBe(29);
  });
});
