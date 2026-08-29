import { createHash } from 'node:crypto';
import { gunzipSync } from 'node:zlib';
import { describe, expect, test } from 'bun:test';
import {
  ANALYZER_HOT_MAX_BYTES,
  analyzerManifestDigestInput,
  assertAnalyzerReleaseSize,
  buildAnalyzerRelease
} from '../src/browser-pack/release-manifest.js';

const COMMIT = '0123456789abcdef0123456789abcdef01234567';
const LOCK = 'abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789';

function digest(value: string): string {
  return createHash('sha256').update(value).digest('hex');
}

describe('browser analyzer release manifest', () => {
  test('is deterministic and records both transport and installed bytes', () => {
    const options = {
      packVersion: 'alpha.1',
      sourceCommit: COMMIT,
      sourcesLockSha256: LOCK,
      hot: new TextEncoder().encode('hot hot hot hot'),
      details: new TextEncoder().encode('details details details')
    } as const;
    const first = buildAnalyzerRelease(options);
    const second = buildAnalyzerRelease(options);

    expect(first).toEqual(second);
    expect(new Uint8Array(gunzipSync(first.hotDownload))).toEqual(options.hot);
    expect(new Uint8Array(gunzipSync(first.detailsDownload))).toEqual(options.details);
    const { manifestSha256: _digest, ...unsigned } = first.manifest;
    expect(first.manifest.manifestSha256).toBe(digest(analyzerManifestDigestInput(unsigned)));
    expect(first.manifest.hot.installedBytes).toBe(options.hot.byteLength);
    expect(first.manifest.hot.downloadBytes).toBe(first.hotDownload.byteLength);
  });

  test('enforces the raw-hot gate independently of compression', () => {
    const release = buildAnalyzerRelease({
      packVersion: 'alpha.1',
      sourceCommit: COMMIT,
      sourcesLockSha256: LOCK,
      hot: new Uint8Array(ANALYZER_HOT_MAX_BYTES + 1),
      details: new Uint8Array([1])
    });
    expect(() => assertAnalyzerReleaseSize(release)).toThrow('hot.bin');
  });
});
