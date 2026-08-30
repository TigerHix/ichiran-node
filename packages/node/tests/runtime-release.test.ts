import { afterAll, describe, expect, test } from 'bun:test';
import { mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

import { openNodeRuntime } from '../src/index.js';

const releaseDirectory = process.env.ICHIRAN_PACK_DIR;

describe.skipIf(!releaseDirectory)('Node packed runtime release', () => {
  test('analyzes, presents, and describes without PostgreSQL', async () => {
    const runtime = await openNodeRuntime(releaseDirectory!);
    expect(await runtime.romanize('今日')).toBe('kyō');
    const analysis = await runtime.analyze('今日は良い天気です', { limit: 2 });
    expect(analysis.paths.length).toBeGreaterThan(0);
    expect(Array.isArray(await runtime.legacy('今日', { limit: 1 }))).toBe(true);
    const entryIndex = analysis.paths[0]!.tokens.find(token => token.entryIndex !== null)?.entryIndex;
    expect(entryIndex).not.toBeNull();
    expect((await runtime.describe(entryIndex!)).seq).toBeGreaterThan(0);
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
});
