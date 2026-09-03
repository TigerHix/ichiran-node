import { createHash } from 'node:crypto';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import { afterEach, describe, expect, test } from 'bun:test';

import {
  QUALIFIED_BASELINE_ARTIFACT,
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
  readonly details: Uint8Array;
}> {
  const directory = await mkdtemp(join(tmpdir(), 'ichiran-browser-release-audit-'));
  temporaryDirectories.push(directory);
  const identity = await currentSourceIdentity(repositoryRoot, sourceLock);
  const installedHot = Uint8Array.of(1, 2, 3, 4);
  const installedDetails = Uint8Array.of(5, 6, 7);
  const hot = new Uint8Array(gzipSync(installedHot));
  const details = new Uint8Array(gzipSync(installedDetails));
  const unsigned = {
    formatVersion: 1 as const,
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
    details: {
      file: 'details.bin.gz',
      encoding: 'gzip' as const,
      downloadBytes: details.byteLength,
      downloadSha256: sha256(details),
      installedBytes: installedDetails.byteLength,
      installedSha256: sha256(installedDetails)
    }
  };
  const manifest: ReleaseManifest = {
    ...unsigned,
    manifestSha256: sha256(JSON.stringify(unsigned))
  };
  await Promise.all([
    writeFile(join(directory, manifest.hot.file), hot),
    writeFile(join(directory, manifest.details.file), details),
    writeFile(join(directory, 'manifest.json'), `${JSON.stringify(manifest, null, 2)}\n`)
  ]);
  return { directory, manifest, hot, details };
}

afterEach(async () => {
  await Promise.all(temporaryDirectories.splice(0).map(directory =>
    rm(directory, { recursive: true, force: true })));
});

describe('browser analyzer release file gate', () => {
  test('accepts only a current, internally hashed release', async () => {
    const value = await fixture();
    const verified = await verifyAnalyzerRelease(value.directory, repositoryRoot);
    expect(verified.manifest).toEqual(value.manifest);
    expect(verified.hotBytes).toEqual(value.hot);
    expect(verified.detailsBytes).toEqual(value.details);
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
    ).rejects.toThrow(`does not match qualified artifact ${QUALIFIED_BASELINE_ARTIFACT}`);
  });
});
