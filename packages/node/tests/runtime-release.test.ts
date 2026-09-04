import { afterAll, describe, expect, test } from 'bun:test';
import { access, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { gunzipSync } from 'node:zlib';

import * as node from '../src/index.js';
import { openAnalyzer } from '../src/index.js';
import { openVerifiedAssetSource } from '../src/file-source.js';
import {
  ANALYZER_WASM_URL,
  Analyzer,
  AnalyzerError
} from '@ichiran/core';
import type { AnalyzerReleaseManifest } from '@ichiran/core/release';
import type { RandomAccessSource } from '@ichiran/core';

const releaseDirectory = process.env.ICHIRAN_PACK_DIR;

function trackedSource(
  label: string,
  source: RandomAccessSource,
  reads: Array<readonly [string, number, number]>
): RandomAccessSource {
  return {
    byteLength: source.byteLength,
    async read(offset, byteLength) {
      reads.push([label, offset, byteLength]);
      return source.read(offset, byteLength);
    },
    dispose: () => source.dispose?.()
  };
}

test('exports one Node loader', () => {
  expect(Object.keys(node)).toEqual(['openAnalyzer']);
});

describe.skipIf(!releaseDirectory)('Node packed analyzer release', () => {
  test('analyzes, romanizes, and reads dictionary entries without PostgreSQL', async () => {
    const analyzer = await openAnalyzer(releaseDirectory!);
    try {
      expect(await analyzer.romanize('今日')).toBe('kyō');
      expect(await analyzer.romanize('猫🙂。', { normalizePunctuation: true })).toContain('🙂');
      const analysis = await analyzer.analyze('今日は良い天気です', { limit: 2 });
      expect(analysis.paths.length).toBeGreaterThan(0);
      for (const path of analysis.paths) {
        for (const token of path.tokens) {
          expect(token.alternatives.every(
            alternative => alternative.candidateId !== token.candidateId
          )).toBeTrue();
        }
      }
      for (const chunk of analysis.chunks) {
        if (chunk.type !== 'word') continue;
        for (const path of chunk.paths) {
          for (const token of path.tokens) {
            expect(token.alternatives.every(
              alternative => alternative.candidateId !== token.candidateId
            )).toBeTrue();
          }
        }
      }
      const ambiguous = await analyzer.analyze('何他', { limit: 3 });
      expect(ambiguous.paths[0]!.tokens[1]!.alternatives.map(value => value.reading))
        .toContain('ほか');
      const hinted = await analyzer.analyze('はし', { limit: 3 });
      expect(JSON.stringify(hinted)).not.toMatch(/[\u200b\u200c]/u);
      const synthetic = await analyzer.analyze('架空固有名', {
        limit: 1,
        entities: [{ start: 0, end: 5, boost: 10_000 }]
      });
      expect(synthetic.paths[0]!.tokens[0]!.pos).toEqual(['n-pr']);
      const entryIndex = analysis.paths[0]!.tokens.find(token => token.entryIndex !== null)?.entryIndex;
      expect(entryIndex).toBeNumber();
      expect((await analyzer.entry(entryIndex!)).seq).toBeGreaterThan(0);
      await expect(analyzer.entry(Number.MAX_SAFE_INTEGER)).rejects.toMatchObject({
        code: 'invalid-input'
      });
    } finally {
      analyzer.dispose();
    }
  });

  test('projects canonical token details without consumer-side dictionary reconstruction', async () => {
    const analyzer = await openAnalyzer(releaseDirectory!);
    try {
      const cat = await analyzer.details('猫', { limit: 3, pathIndex: 0, tokenIndex: 0 });
      expect(cat.meanings.length).toBeGreaterThan(0);
      expect(cat.alternatives).toEqual([]);
      const chineseCat = await analyzer.details('猫', {
        limit: 3, pathIndex: 0, tokenIndex: 0, locale: 'zh-Hans'
      });
      expect(chineseCat.meanings.length).toBeGreaterThan(0);
      expect(chineseCat.meanings.map(meaning => meaning.gloss))
        .not.toEqual(cat.meanings.map(meaning => meaning.gloss));

      const inflected = await analyzer.details('食べました', {
        limit: 3,
        pathIndex: 0,
        tokenIndex: 0
      });
      expect(inflected.meanings).toEqual([]);
      expect(inflected.conjugations.length).toBeGreaterThan(0);

      const compound = await analyzer.details('読んでいました', {
        limit: 3,
        pathIndex: 0,
        tokenIndex: 0
      });
      expect(compound.components.length).toBeGreaterThan(0);
      expect(compound.conjugations).toEqual([]);

      const counter = await analyzer.details('三個', {
        limit: 3,
        pathIndex: 0,
        tokenIndex: 0
      });
      expect(counter.counter).not.toBeNull();
      expect(counter.counter?.value).toBe('3');
      expect(counter.meanings.length).toBeGreaterThan(0);
      expect(counter.meanings.every(meaning => meaning.pos.includes('ctr'))).toBeTrue();

      const hinted = await analyzer.details('はし', {
        limit: 3,
        pathIndex: 0,
        tokenIndex: 0
      });
      expect(JSON.stringify(hinted)).not.toMatch(/[\u200b\u200c]/u);

      const entity = await analyzer.details('架空固有名', {
        limit: 1,
        pathIndex: 0,
        tokenIndex: 0,
        entities: [{ start: 0, end: 5, boost: 10_000 }]
      });
      expect(entity).toMatchObject({
        entityKind: 'proper-noun',
        meanings: []
      });

      try {
        await analyzer.details('猫', { limit: 1, pathIndex: 99, tokenIndex: 0 });
        throw new Error('missing token lookup unexpectedly succeeded');
      } catch (error) {
        expect(error).toBeInstanceOf(AnalyzerError);
        expect((error as AnalyzerError).code).toBe('not-found');
      }
    } finally {
      analyzer.dispose();
    }
  });

  test('uses the stable public error set', async () => {
    const analyzer = await openAnalyzer(releaseDirectory!);
    try {
      await expect(analyzer.analyze('猫'.repeat(257))).rejects.toMatchObject({
        name: 'AnalyzerError',
        code: 'invalid-input'
      });
      await expect(analyzer.entry(-1)).rejects.toMatchObject({
        name: 'AnalyzerError',
        code: 'invalid-input'
      });
      await expect(analyzer.romanize('猫', {
        method: 42 as never
      })).rejects.toMatchObject({
        name: 'AnalyzerError',
        code: 'invalid-input'
      });
    } finally {
      analyzer.dispose();
    }
  });

  test('keeps analysis definition-cold and disposes every verified file source', async () => {
    const manifest = JSON.parse(
      await readFile(join(releaseDirectory!, 'manifest.json'), 'utf8')
    ) as AnalyzerReleaseManifest;
    const lexicon = await openVerifiedAssetSource(releaseDirectory!, manifest.lexicon);
    const en = await openVerifiedAssetSource(releaseDirectory!, manifest.locales.en!);
    const zhHans = await openVerifiedAssetSource(releaseDirectory!, manifest.locales['zh-Hans']!);
    const paths = [lexicon.path, en.path, zhHans.path];
    const reads: Array<readonly [string, number, number]> = [];
    const downloadedHot = new Uint8Array(
      await readFile(join(releaseDirectory!, manifest.hot.file))
    );
    const hot = manifest.hot.encoding === 'gzip'
      ? new Uint8Array(gunzipSync(downloadedHot))
      : downloadedHot;
    let analyzer: Analyzer | null = null;
    try {
      analyzer = await Analyzer.open({
        hot,
        wasm: new Uint8Array(await readFile(ANALYZER_WASM_URL)),
        lexicon: {
          sha256: manifest.lexicon.installedSha256,
          source: trackedSource('lexicon', lexicon, reads)
        },
        locales: {
          en: trackedSource('en', en, reads),
          'zh-Hans': trackedSource('zh-Hans', zhHans, reads)
        }
      });
      const openedReads = [...reads];
      const analysis = await analyzer.analyze('今日', { limit: 1 });
      expect(reads).toEqual(openedReads);
      await analyzer.details('今日', { limit: 1, pathIndex: 0, tokenIndex: 0 });
      expect(reads.length).toBeGreaterThan(openedReads.length);
      const englishReads = reads.length;
      await analyzer.details('今日', {
        limit: 1, pathIndex: 0, tokenIndex: 0, locale: 'zh-Hans'
      });
      expect(reads.length).toBeGreaterThan(englishReads);
      const entryIndex = analysis.paths[0]!.tokens.find(token => token.entryIndex !== null)?.entryIndex;
      expect((await analyzer.entry(entryIndex!, { locale: 'zh-Hans' })).senses.length)
        .toBeGreaterThan(0);
    } finally {
      if (analyzer) analyzer.dispose();
      else for (const source of [lexicon, en, zhHans]) source.dispose();
    }
    for (const path of paths) await expect(access(path)).rejects.toThrow();
  });
});

describe('Node release verification', () => {
  let temporary: string | null = null;
  afterAll(async () => {
    if (temporary) await rm(temporary, { recursive: true });
  });

  test('requires an explicit directory or environment setting', async () => {
    const previous = process.env.ICHIRAN_PACK_DIR;
    delete process.env.ICHIRAN_PACK_DIR;
    try {
      await expect(openAnalyzer()).rejects.toEqual(
        new AnalyzerError('invalid-input', 'Pass a pack directory or set ICHIRAN_PACK_DIR')
      );
    } finally {
      if (previous !== undefined) process.env.ICHIRAN_PACK_DIR = previous;
    }
  });

  test.skipIf(!releaseDirectory)('rejects a manifest whose checksum was not updated', async () => {
    temporary = await mkdtemp(join(tmpdir(), 'ichiran-node-release-'));
    const manifest = JSON.parse(await readFile(join(releaseDirectory!, 'manifest.json'), 'utf8'));
    manifest.packVersion = `${manifest.packVersion}-tampered`;
    await writeFile(join(temporary, 'manifest.json'), JSON.stringify(manifest));
    await expect(openAnalyzer(temporary)).rejects.toMatchObject({ code: 'invalid-pack' });
  });

  test.skipIf(!releaseDirectory)('honors the source commit deployment gate', async () => {
    const previous = process.env.ICHIRAN_SOURCE_COMMIT;
    process.env.ICHIRAN_SOURCE_COMMIT = '5'.repeat(40);
    try {
      await expect(openAnalyzer(releaseDirectory!)).rejects.toMatchObject({
        code: 'invalid-pack',
        message: expect.stringContaining('does not match runtime')
      });
    } finally {
      if (previous === undefined) delete process.env.ICHIRAN_SOURCE_COMMIT;
      else process.env.ICHIRAN_SOURCE_COMMIT = previous;
    }
  });
});
