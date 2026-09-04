import { createHash } from 'node:crypto';
import { gunzipSync } from 'node:zlib';
import { describe, expect, test } from 'bun:test';
import {
  ANALYZER_HOT_MAX_BYTES,
  ANALYZER_PERSISTED_MAX_BYTES,
  ANALYZER_WIRE_MAX_BYTES,
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
  test('keeps the current phone budgets explicit', () => {
    expect(ANALYZER_HOT_MAX_BYTES).toBe(25 * 1024 * 1024);
    expect(ANALYZER_PERSISTED_MAX_BYTES).toBe(64 * 1024 * 1024);
    expect(ANALYZER_WIRE_MAX_BYTES).toBe(36 * 1024 * 1024);
  });

  test('is deterministic and records both transport and installed bytes', () => {
    const options = {
      packVersion: 'alpha.1',
      sourceCommit: COMMIT,
      sourcesLockSha256: LOCK,
      hot: new TextEncoder().encode('hot hot hot hot'),
      lexicon: new TextEncoder().encode('lexicon lexicon'),
      locales: {
        en: new TextEncoder().encode('English glosses'),
        'zh-Hans': new TextEncoder().encode('简体中文释义')
      }
    } as const;
    const first = buildAnalyzerRelease(options);
    const second = buildAnalyzerRelease(options);

    expect(first).toEqual(second);
    expect(new Uint8Array(gunzipSync(first.hotDownload))).toEqual(options.hot);
    expect(new Uint8Array(gunzipSync(first.lexiconDownload))).toEqual(options.lexicon);
    expect(new Uint8Array(gunzipSync(first.localeDownloads.en!))).toEqual(options.locales.en);
    expect(new Uint8Array(gunzipSync(first.localeDownloads['zh-Hans']!)))
      .toEqual(options.locales['zh-Hans']);
    const { manifestSha256: _digest, ...unsigned } = first.manifest;
    expect(first.manifest.manifestSha256).toBe(digest(analyzerManifestDigestInput(unsigned)));
    expect(first.manifest.hot.installedBytes).toBe(options.hot.byteLength);
    expect(first.manifest.hot.downloadBytes).toBe(first.hotDownload.byteLength);

    const sizes = assertAnalyzerReleaseSize(first);
    const markerBytes = new TextEncoder().encode(JSON.stringify({
      state: 'ready',
      manifest: first.manifest,
      installId: '00000000-0000-4000-8000-000000000000',
      installedAt: '1970-01-01T00:00:00.000Z',
      slot: 'a'
    })).byteLength;
    expect(sizes.installedMarkerBytes).toBe(markerBytes);
    expect(sizes.installedIdentityPayloadBytes).toBe(36);
    expect(sizes.persistedBytes).toBe(
      options.hot.byteLength
      + options.lexicon.byteLength
      + options.locales.en.byteLength
      + options.locales['zh-Hans'].byteLength
      + markerBytes
      + 36
    );
  });

  test('enforces the raw-hot gate independently of compression', () => {
    const release = buildAnalyzerRelease({
      packVersion: 'alpha.1',
      sourceCommit: COMMIT,
      sourcesLockSha256: LOCK,
      hot: new Uint8Array(ANALYZER_HOT_MAX_BYTES + 1),
      lexicon: new Uint8Array([1]),
      locales: { en: new Uint8Array([2]), 'zh-Hans': new Uint8Array([3]) }
    });
    expect(() => assertAnalyzerReleaseSize(release)).toThrow('hot.bin');
  });

  test('cannot build a release whose version would overflow marker metadata', () => {
    expect(() => buildAnalyzerRelease({
      packVersion: '界'.repeat(43),
      sourceCommit: COMMIT,
      sourcesLockSha256: LOCK,
      hot: new Uint8Array([1]),
      lexicon: new Uint8Array([2]),
      locales: { en: new Uint8Array([3]), 'zh-Hans': new Uint8Array([4]) }
    })).toThrow('128 UTF-8 bytes');
  });
});
