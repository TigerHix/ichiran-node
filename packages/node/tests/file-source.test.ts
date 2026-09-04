import { afterEach, describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { access, mkdir, mkdtemp, readFile, readdir, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { gzipSync } from 'node:zlib';

import type { AnalyzerReleaseAsset } from '@ichiran/core/release';
import { openVerifiedAssetSource } from '../src/file-source.js';

const roots: string[] = [];

function digest(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function fixture(installed = Buffer.from('0123456789')): Promise<{
  readonly release: string;
  readonly temporary: string;
  readonly installed: Buffer;
  readonly downloaded: Buffer;
  readonly asset: AnalyzerReleaseAsset;
}> {
  const root = await mkdtemp(join(tmpdir(), 'ichiran-node-detail-test-'));
  roots.push(root);
  const release = join(root, 'release');
  const temporary = join(root, 'temporary');
  await Promise.all([mkdir(release), mkdir(temporary)]);
  const downloaded = gzipSync(installed, { level: 9 });
  await writeFile(join(release, 'lexicon.bin.gz'), downloaded);
  return {
    release,
    temporary,
    installed,
    downloaded,
    asset: {
      file: 'lexicon.bin.gz',
      encoding: 'gzip',
      downloadBytes: downloaded.byteLength,
      downloadSha256: digest(downloaded),
      installedBytes: installed.byteLength,
      installedSha256: digest(installed)
    }
  };
}

async function expectEmpty(path: string): Promise<void> {
  expect(await readdir(path)).toEqual([]);
}

afterEach(async () => {
  await Promise.all(roots.splice(0).map(path => rm(path, { recursive: true, force: true })));
});

describe('file-backed Node release assets', () => {
  test('verifies once, serves exact positional ranges, and removes owned bytes on dispose', async () => {
    const value = await fixture();
    const source = await openVerifiedAssetSource(value.release, value.asset, value.temporary);
    expect(source.byteLength).toBe(value.installed.byteLength);
    expect(Array.from(await source.read(2, 5))).toEqual(Array.from(value.installed.subarray(2, 7)));
    expect((await readFile(source.path)).equals(value.installed)).toBe(true);

    source.dispose();
    await expect(access(source.path)).rejects.toThrow();
    await expect(source.read(0, 1)).rejects.toThrow('outside the available source');
    await expectEmpty(value.temporary);
  });

  test('rejects a compressed checksum mismatch and removes the partial spool', async () => {
    const value = await fixture();
    await expect(openVerifiedAssetSource(value.release, {
      ...value.asset,
      downloadSha256: '0'.repeat(64)
    }, value.temporary)).rejects.toThrow('does not match the analyzer manifest');
    await expectEmpty(value.temporary);
  });

  test('rejects a truncated gzip even when its outer identity matches and removes the spool', async () => {
    const value = await fixture();
    const truncated = value.downloaded.subarray(0, value.downloaded.byteLength - 4);
    await writeFile(join(value.release, value.asset.file), truncated);
    await expect(openVerifiedAssetSource(value.release, {
      ...value.asset,
      downloadBytes: truncated.byteLength,
      downloadSha256: digest(truncated)
    }, value.temporary)).rejects.toThrow();
    await expectEmpty(value.temporary);
  });

  test('rejects decoded checksum and length mismatches and removes each spool', async () => {
    const value = await fixture();
    await expect(openVerifiedAssetSource(value.release, {
      ...value.asset,
      installedSha256: '0'.repeat(64)
    }, value.temporary)).rejects.toThrow('decoded bytes do not match');
    await expectEmpty(value.temporary);

    await expect(openVerifiedAssetSource(value.release, {
      ...value.asset,
      installedBytes: value.asset.installedBytes - 1
    }, value.temporary)).rejects.toThrow('decoded bytes exceed');
    await expectEmpty(value.temporary);
  });

  test('uses a verified identity asset in place and never deletes release data', async () => {
    const value = await fixture();
    const path = join(value.release, 'lexicon.bin');
    await writeFile(path, value.installed);
    const asset: AnalyzerReleaseAsset = {
      file: 'lexicon.bin',
      encoding: 'identity',
      downloadBytes: value.installed.byteLength,
      downloadSha256: digest(value.installed),
      installedBytes: value.installed.byteLength,
      installedSha256: digest(value.installed)
    };
    const source = await openVerifiedAssetSource(value.release, asset, value.temporary);
    expect(Array.from(await source.read(7, 3))).toEqual(Array.from(value.installed.subarray(7)));
    source.dispose();
    expect((await readFile(path)).equals(value.installed)).toBe(true);
    await expectEmpty(value.temporary);
  });
});
