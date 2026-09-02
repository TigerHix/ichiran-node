import { afterAll, describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { access, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { gunzipSync } from 'node:zlib';

import { openNodeRuntime } from '../src/index.js';
import { openVerifiedDetailSource } from '../src/file-details.js';
import {
  analyzerManifestDigestInput,
  IchiranRuntime,
  RUST_KERNEL_WASM_URL,
  type AnalyzerReleaseManifest
} from '@ichiran/core';

const releaseDirectory = process.env.ICHIRAN_PACK_DIR;

describe.skipIf(!releaseDirectory)('Node packed runtime release', () => {
  test('analyzes, presents, and describes without PostgreSQL', async () => {
    const runtime = await openNodeRuntime(releaseDirectory!);
    try {
      expect(await runtime.romanize('今日')).toBe('kyō');
      const analysis = await runtime.analyze('今日は良い天気です', { limit: 2 });
      expect(analysis.paths.length).toBeGreaterThan(0);
      expect(Array.isArray(await runtime.legacy('今日', { limit: 1 }))).toBe(true);
      const entryIndex = analysis.paths[0]!.tokens.find(token => token.entryIndex !== null)?.entryIndex;
      expect(entryIndex).not.toBeNull();
      const detail = await runtime.describe(entryIndex!);
      expect(detail.seq).toBeGreaterThan(0);
      expect(runtime.entryIndexForSequence(detail.seq)).toBe(entryIndex!);
      expect(
        ['surface', 'roots', 'morphology', 'support', 'annotations'].filter(field => field in runtime)
      ).toEqual([]);
    } finally {
      runtime.dispose();
    }
  });

  test('keeps analysis detail-cold and disposes the verified file source', async () => {
    const manifest = JSON.parse(
      await readFile(join(releaseDirectory!, 'manifest.json'), 'utf8')
    ) as AnalyzerReleaseManifest;
    const source = await openVerifiedDetailSource(releaseDirectory!, manifest.details);
    const path = source.path;
    const reads: Array<readonly [number, number]> = [];
    const downloadedHot = new Uint8Array(
      await readFile(join(releaseDirectory!, manifest.hot.file))
    );
    const hot = manifest.hot.encoding === 'gzip'
      ? new Uint8Array(gunzipSync(downloadedHot))
      : downloadedHot;
    let runtime: IchiranRuntime | null = null;
    try {
      runtime = await IchiranRuntime.open({
        hot,
        wasm: new Uint8Array(await readFile(RUST_KERNEL_WASM_URL)),
        details: {
          byteLength: source.byteLength,
          async read(offset, byteLength) {
            reads.push([offset, byteLength]);
            return source.read(offset, byteLength);
          },
          dispose: () => source.dispose()
        }
      });
      expect(reads).toHaveLength(2);
      const openedReads = [...reads];
      const analysis = await runtime.analyze('今日', { limit: 1 });
      expect(reads).toEqual(openedReads);
      const entryIndex = analysis.paths[0]!.tokens.find(token => token.entryIndex !== null)?.entryIndex;
      expect(entryIndex).not.toBeNull();
      expect((await runtime.describe(entryIndex!)).seq).toBeGreaterThan(0);
      expect(reads).toHaveLength(3);
    } finally {
      if (runtime) runtime.dispose();
      else source.dispose();
    }
    await expect(access(path)).rejects.toThrow();
  });

  test('keeps concurrent legacy detail sessions independent', async () => {
    const inputs = ['食べさせられました', '三個'] as const;
    const reference = await openNodeRuntime(releaseDirectory!);
    const expected: unknown[] = [];
    try {
      for (const input of inputs) {
        expected.push(await reference.legacy(input, { limit: 1 }));
      }
    } finally {
      reference.dispose();
    }

    const manifest = JSON.parse(
      await readFile(join(releaseDirectory!, 'manifest.json'), 'utf8')
    ) as AnalyzerReleaseManifest;
    const source = await openVerifiedDetailSource(releaseDirectory!, manifest.details);
    const downloadedHot = new Uint8Array(
      await readFile(join(releaseDirectory!, manifest.hot.file))
    );
    const hot = manifest.hot.encoding === 'gzip'
      ? new Uint8Array(gunzipSync(downloadedHot))
      : downloadedHot;
    const reads: Array<readonly [number, number]> = [];
    let armed = false;
    let releaseFirstReads: () => void = () => undefined;
    const firstReads = new Promise<void>(resolve => {
      releaseFirstReads = resolve;
    });
    let runtime: IchiranRuntime | null = null;
    try {
      runtime = await IchiranRuntime.open({
        hot,
        wasm: new Uint8Array(await readFile(RUST_KERNEL_WASM_URL)),
        details: {
          byteLength: source.byteLength,
          async read(offset, byteLength) {
            if (armed) {
              reads.push([offset, byteLength]);
              if (reads.length === inputs.length) releaseFirstReads();
              if (reads.length <= inputs.length) await firstReads;
            }
            return source.read(offset, byteLength);
          },
          dispose: () => source.dispose()
        }
      });
      armed = true;
      const actual = await Promise.all(inputs.map(input => runtime!.legacy(input, { limit: 1 })));
      expect(actual).toEqual(expected);
      expect(reads).toHaveLength(inputs.length);
      expect(reads[0]).not.toEqual(reads[1]);
    } finally {
      if (runtime) runtime.dispose();
      else source.dispose();
    }
  });
});

describe('Node release verification', () => {
  let temporary: string | null = null;
  afterAll(async () => {
    if (temporary) await rm(temporary, { recursive: true });
  });

  test.skipIf(!releaseDirectory)('rejects a manifest whose checksum was not updated', async () => {
    temporary = await mkdtemp(join(tmpdir(), 'ichiran-node-release-'));
    const manifest = JSON.parse(await readFile(join(releaseDirectory!, 'manifest.json'), 'utf8'));
    manifest.packVersion = `${manifest.packVersion}-tampered`;
    await writeFile(join(temporary, 'manifest.json'), JSON.stringify(manifest));
    await expect(openNodeRuntime(temporary)).rejects.toThrow('manifest checksum');
  });

  test('rejects a pack built for different runtime code before reading assets', async () => {
    const identityDirectory = await mkdtemp(join(tmpdir(), 'ichiran-node-identity-'));
    const unsigned = {
      formatVersion: 1 as const,
      packVersion: 'identity-fixture',
      sourceCommit: '1'.repeat(40),
      sourcesLockSha256: '2'.repeat(64),
      hot: {
        file: 'hot.bin',
        encoding: 'identity' as const,
        downloadBytes: 1,
        downloadSha256: '3'.repeat(64),
        installedBytes: 1,
        installedSha256: '3'.repeat(64)
      },
      details: {
        file: 'details.bin',
        encoding: 'identity' as const,
        downloadBytes: 1,
        downloadSha256: '4'.repeat(64),
        installedBytes: 1,
        installedSha256: '4'.repeat(64)
      }
    };
    const manifest = {
      ...unsigned,
      manifestSha256: createHash('sha256')
        .update(analyzerManifestDigestInput(unsigned))
        .digest('hex')
    };
    try {
      await writeFile(join(identityDirectory, 'manifest.json'), JSON.stringify(manifest));
      await expect(openNodeRuntime(identityDirectory, { expectedSourceCommit: '5'.repeat(40) }))
        .rejects.toThrow('does not match runtime');
    } finally {
      await rm(identityDirectory, { recursive: true, force: true });
    }
  });
});
