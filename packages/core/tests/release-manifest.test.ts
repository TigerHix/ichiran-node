import { createHash } from 'node:crypto';
import { describe, expect, test } from 'bun:test';

import {
  ANALYZER_PACK_VERSION_MAX_UTF8_BYTES,
  analyzerManifestDigestInput,
  parseAnalyzerReleaseManifest,
  type AnalyzerReleaseManifestWithoutDigest
} from '../src/release-manifest.js';

const sha256 = (text: string) => createHash('sha256').update(text).digest('hex');

function manifest(packVersion = 'fixture') {
  const installed = sha256('installed');
  const unsigned: AnalyzerReleaseManifestWithoutDigest = {
    formatVersion: 2,
    packVersion,
    sourceCommit: '1'.repeat(40),
    sourcesLockSha256: '2'.repeat(64),
    hot: {
      file: 'hot.bin',
      encoding: 'identity',
      downloadBytes: 9,
      downloadSha256: installed,
      installedBytes: 9,
      installedSha256: installed
    },
    lexicon: {
      file: 'lexicon.bin.gz',
      encoding: 'gzip',
      downloadBytes: 4,
      downloadSha256: '3'.repeat(64),
      installedBytes: 10,
      installedSha256: '4'.repeat(64)
    },
    locales: {
      en: {
        file: 'gloss.en.bin.gz', encoding: 'gzip',
        downloadBytes: 5, downloadSha256: '5'.repeat(64),
        installedBytes: 11, installedSha256: '6'.repeat(64)
      },
      'zh-Hans': {
        file: 'gloss.zh-Hans.bin.gz', encoding: 'gzip',
        downloadBytes: 6, downloadSha256: '7'.repeat(64),
        installedBytes: 12, installedSha256: '8'.repeat(64)
      }
    }
  };
  return { ...unsigned, manifestSha256: sha256(analyzerManifestDigestInput(unsigned)) };
}

describe('release manifest contract', () => {
  test('authenticates both supported encodings', () => {
    const value = manifest();
    expect(parseAnalyzerReleaseManifest(value, sha256)).toEqual(value);
  });

  test('rejects fields outside the authenticated contract', () => {
    expect(() => parseAnalyzerReleaseManifest({ ...manifest(), surprise: true }, sha256))
      .toThrow('unsupported fields');
    const value = manifest();
    expect(() => parseAnalyzerReleaseManifest({
      ...value,
      hot: { ...value.hot, surprise: true }
    }, sha256)).toThrow('unsupported fields');
  });

  test('bounds packVersion by UTF-8 bytes for the installed marker', () => {
    expect(parseAnalyzerReleaseManifest(
      manifest('x'.repeat(ANALYZER_PACK_VERSION_MAX_UTF8_BYTES)),
      sha256
    ).packVersion).toHaveLength(ANALYZER_PACK_VERSION_MAX_UTF8_BYTES);
    expect(() => parseAnalyzerReleaseManifest(manifest('界'.repeat(43)), sha256))
      .toThrow('128 UTF-8 bytes');
  });

  test('authenticates additional locale assets without changing the schema', () => {
    const value = manifest();
    const unsigned: AnalyzerReleaseManifestWithoutDigest = {
      ...value,
      locales: {
        ...value.locales,
        de: {
          file: 'gloss.de.bin', encoding: 'identity',
          downloadBytes: 9, downloadSha256: sha256('installed'),
          installedBytes: 9, installedSha256: sha256('installed')
        }
      }
    };
    const withDigest = {
      ...unsigned,
      manifestSha256: sha256(analyzerManifestDigestInput(unsigned))
    };
    expect(Object.keys(parseAnalyzerReleaseManifest(withDigest, sha256).locales))
      .toEqual(['de', 'en', 'zh-Hans']);
  });
});
