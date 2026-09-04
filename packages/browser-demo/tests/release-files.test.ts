import { createHash } from 'node:crypto';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import { afterEach, describe, expect, test } from 'bun:test';
import { analyzerReadyStateSize } from '@ichiran/core/release';

import {
  QUALIFIED_BASELINE_ARTIFACT,
  assertAnalyzerReadyStateSize,
  currentSourceIdentity,
  verifyAnalyzerRelease,
  type ReleaseManifest
} from '../scripts/release-files.js';

const repositoryRoot = resolve(import.meta.dir, '..', '..', '..');
const temporaryDirectories: string[] = [];

function sha256(value: string | Uint8Array): string {
  return createHash('sha256').update(value).digest('hex');
}

async function fixture(
  sourceCommit?: string,
  sourceLock?: string
): Promise<{
  readonly directory: string;
  readonly manifest: ReleaseManifest;
  readonly hot: Uint8Array;
  readonly lexicon: Uint8Array;
  readonly locales: Readonly<Record<string, Uint8Array>>;
}> {
  const directory = await mkdtemp(join(tmpdir(), 'ichiran-browser-release-audit-'));
  temporaryDirectories.push(directory);
  const identity = await currentSourceIdentity(repositoryRoot, sourceLock);
  const installedHot = Uint8Array.of(1, 2, 3, 4);
  const installedLexicon = Uint8Array.of(5, 6, 7);
  const installedEn = Uint8Array.of(8, 9);
  const installedZhHans = Uint8Array.of(10, 11);
  const hot = new Uint8Array(gzipSync(installedHot));
  const lexicon = new Uint8Array(gzipSync(installedLexicon));
  const en = new Uint8Array(gzipSync(installedEn));
  const zhHans = new Uint8Array(gzipSync(installedZhHans));
  const unsigned = {
    formatVersion: 2 as const,
    packVersion: 'test.release',
    sourceCommit: sourceCommit ?? identity.sourceCommit,
    sourcesLockSha256: identity.sourcesLockSha256,
    hot: {
      file: 'hot.bin.gz',
      encoding: 'gzip' as const,
      downloadBytes: hot.byteLength,
      downloadSha256: sha256(hot),
      installedBytes: installedHot.byteLength,
      installedSha256: sha256(installedHot)
    },
    lexicon: {
      file: 'lexicon.bin.gz',
      encoding: 'gzip' as const,
      downloadBytes: lexicon.byteLength,
      downloadSha256: sha256(lexicon),
      installedBytes: installedLexicon.byteLength,
      installedSha256: sha256(installedLexicon)
    },
    locales: {
      en: {
        file: 'gloss.en.bin.gz', encoding: 'gzip' as const,
        downloadBytes: en.byteLength, downloadSha256: sha256(en),
        installedBytes: installedEn.byteLength, installedSha256: sha256(installedEn)
      },
      'zh-Hans': {
        file: 'gloss.zh-Hans.bin.gz', encoding: 'gzip' as const,
        downloadBytes: zhHans.byteLength, downloadSha256: sha256(zhHans),
        installedBytes: installedZhHans.byteLength, installedSha256: sha256(installedZhHans)
      }
    }
  };
  const manifest: ReleaseManifest = {
    ...unsigned,
    manifestSha256: sha256(JSON.stringify(unsigned))
  };
  await Promise.all([
    writeFile(join(directory, manifest.hot.file), hot),
    writeFile(join(directory, manifest.lexicon.file), lexicon),
    ...Object.entries(manifest.locales).map(([locale, asset]) => writeFile(
      join(directory, asset.file),
      locale === 'en' ? en : zhHans
    )),
    writeFile(join(directory, 'manifest.json'), `${JSON.stringify(manifest, null, 2)}\n`)
  ]);
  return { directory, manifest, hot, lexicon, locales: { en, 'zh-Hans': zhHans } };
}

afterEach(async () => {
  await Promise.all(temporaryDirectories.splice(0).map(directory =>
    rm(directory, { recursive: true, force: true })));
});

describe('browser analyzer release file gate', () => {
  test('enforces the analyzer ready-state budget at the exact boundary', async () => {
    const value = await fixture();
    const base = assertAnalyzerReadyStateSize(value.manifest).persistedBytes;
    const limit = 64 * 1024 * 1024;
    let atLimit = {
      ...value.manifest,
      lexicon: {
        ...value.manifest.lexicon,
        installedBytes: value.manifest.lexicon.installedBytes + limit - base
      }
    };
    for (;;) {
      const difference = analyzerReadyStateSize(atLimit).persistedBytes - limit;
      if (difference === 0) break;
      atLimit = {
        ...atLimit,
        lexicon: { ...atLimit.lexicon, installedBytes: atLimit.lexicon.installedBytes - difference }
      };
    }
    expect(assertAnalyzerReadyStateSize(atLimit).persistedBytes).toBe(limit);
    expect(() => assertAnalyzerReadyStateSize({
      ...atLimit,
      lexicon: { ...atLimit.lexicon, installedBytes: atLimit.lexicon.installedBytes + 1 }
    })).toThrow(`limit is ${limit}`);
  });

  test('accepts only a current, internally hashed release', async () => {
    const value = await fixture();
    const verified = await verifyAnalyzerRelease(value.directory, repositoryRoot);
    expect(verified.manifest).toEqual(value.manifest);
    expect(verified.hotBytes).toEqual(value.hot);
    expect(verified.lexiconBytes).toEqual(value.lexicon);
    expect(verified.localeBytes).toEqual(value.locales);
  });

  test('rejects stale source identity before staging', async () => {
    const value = await fixture('0'.repeat(40));
    await expect(verifyAnalyzerRelease(value.directory, repositoryRoot)).rejects.toThrow(
      'Analyzer release is stale: sourceCommit'
    );
  });

  test('verifies a release against an explicit tracked update lock', async () => {
    const sourceLock = 'data/source-compiler-update-2026-09-02.lock.json';
    const value = await fixture(undefined, sourceLock);
    const verified = await verifyAnalyzerRelease(value.directory, repositoryRoot, { sourceLock });
    expect(verified.manifest.sourcesLockSha256).toBe(value.manifest.sourcesLockSha256);
  });

  test('rejects asset bytes that do not match the manifest', async () => {
    const value = await fixture();
    const corrupt = Uint8Array.from(value.hot);
    corrupt[0] ^= 0xff;
    await writeFile(join(value.directory, value.manifest.hot.file), corrupt);
    await expect(verifyAnalyzerRelease(value.directory, repositoryRoot)).rejects.toThrow(
      'hot download checksum'
    );
  });

  test('rejects an unknown qualified artifact name', async () => {
    const value = await fixture();
    await expect(
      verifyAnalyzerRelease(value.directory, repositoryRoot, {
        qualifiedArtifact: 'not-a-qualified-artifact'
      })
    ).rejects.toThrow('Unknown qualified analyzer artifact');
  });

  test('does not relabel an arbitrary release as the qualified baseline', async () => {
    const value = await fixture();
    await expect(
      verifyAnalyzerRelease(value.directory, repositoryRoot, {
        qualifiedArtifact: QUALIFIED_BASELINE_ARTIFACT
      })
    ).rejects.toThrow(`predates analyzer manifest format 2`);
  });
});
