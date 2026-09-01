import { beforeAll, describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import {
  compileQualifiedCustomData,
  type CustomCompilation
} from '../src/source-compiler/chronological-custom.js';
import { canonicalEntriesDigest } from '../src/source-compiler/digest.js';
import { loadJmdictEntries } from '../src/source-compiler/jmdict.js';
import {
  CUSTOM_SOURCE_HASHES,
  loadGeographicDrafts
} from '../src/source-compiler/custom-sources.js';
import type { CanonicalEntry } from '../src/source-compiler/model.js';

const paths = {
  extra: fileURLToPath(new URL('../../../data/sources/extra.xml', import.meta.url)),
  municipality: fileURLToPath(new URL('../../../data/sources/jichitai.csv', import.meta.url)),
  ward: fileURLToPath(new URL('../../../data/sources/gyoseiku.csv', import.meta.url))
};
const jmdict = fileURLToPath(new URL('../JMdict_e.gz', import.meta.url));

let compilation: CustomCompilation;

beforeAll(async () => {
  const base: CanonicalEntry[] = [];
  for await (const entry of loadJmdictEntries(jmdict, 'edrdg-jmdict-e-2026-01-01')) {
    base.push(entry);
  }
  expect(base).toHaveLength(214_698);
  compilation = await compileQualifiedCustomData(base, paths, base.length);
}, 30_000);

describe('pinned custom sources', () => {
  test('have the reviewed source bytes', async () => {
    const sources = [
      [paths.extra, CUSTOM_SOURCE_HASHES.extra],
      [paths.municipality, CUSTOM_SOURCE_HASHES.municipality],
      [paths.ward, CUSTOM_SOURCE_HASHES.ward]
    ] as const;
    for (const [path, expected] of sources) {
      const digest = createHash('sha256').update(await readFile(path)).digest('hex');
      expect(digest).toBe(expected);
    }
  });

  test('produce the stable municipality and ward proposal order', async () => {
    const drafts = await loadGeographicDrafts(paths.municipality, paths.ward);
    expect(drafts).toHaveLength(3_750);
    expect(drafts.filter(draft => draft.kind === 'municipality')).toHaveLength(3_575);
    expect(drafts.filter(draft => draft.kind === 'ward')).toHaveLength(175);
    expect(drafts[0]).toMatchObject({ text: '東京都', reading: 'とうきょうと' });
    expect(drafts.at(-1)).toMatchObject({
      text: '北区',
      reading: 'きたく',
      definition: 'Kita Ward, Kumamoto'
    });
  });
});

describe('chronological custom compilation', () => {
  test('reproduces all qualified custom root identities', async () => {
    expect(compilation.createdRoots).toHaveLength(3_270);
    expect(compilation.createdRoots.slice(0, 5).map(entry => entry.seq)).toEqual([
      12_294_525,
      12_294_526,
      12_294_576,
      900_000,
      900_001
    ]);
    expect(compilation.createdRoots.slice(5).map(entry => entry.seq)).toEqual(
      Array.from({ length: 3_265 }, (_, ordinal) => 12_294_577 + ordinal)
    );

    const identity = createHash('sha256')
      .update(compilation.createdRoots.map(entry => entry.seq).sort((a, b) => a - b).join('\n') + '\n')
      .digest('hex');
    expect(identity).toBe('e992430575166c17f7ba8e8bc82386f271005ecf9203a9ed2a851ab2d160695a');
    expect(await canonicalEntriesDigest(compilation.createdRoots)).toEqual({
      entries: 3_270,
      sha256: '628ca94479064b50b13f1ecdac6294d0707eb426da1e16619126c84865915923'
    });
  });

  test('records every mutation in one deterministic event sequence', async () => {
    expect(compilation.edits).toHaveLength(3_703);
    expect(compilation.skipped).toBe(52);
    expect(compilation.nextEvent).toBe(218_401);
    expect(compilation.edits.every((edit, ordinal) => edit.event === 214_698 + ordinal)).toBe(true);

    const counts = new Map<string, number>();
    for (const edit of compilation.edits) {
      const key = `${edit.sourceId}:${edit.kind}`;
      counts.set(key, (counts.get(key) ?? 0) + 1);
    }
    expect(Object.fromEntries(counts)).toEqual({
      'ichiran-extra-260118:create-root': 5,
      'ichiran-jichitai-260118:add-sense': 293,
      'ichiran-jichitai-260118:replace-gloss': 75,
      'ichiran-jichitai-260118:create-root': 3_157,
      'ichiran-gyoseiku-260118:add-sense': 65,
      'ichiran-gyoseiku-260118:create-root': 108
    });
    expect(createHash('sha256').update(JSON.stringify(compilation.edits)).digest('hex'))
      .toBe('000c103b84daef02b85b00128acd149c4ef42e8dab7fede85f0eb86b4841e304');
  });

  test('returns final new roots and explicit updates to existing details', async () => {
    expect(compilation.updatedEntries).toHaveLength(220);
    expect(await canonicalEntriesDigest(compilation.updatedEntries)).toEqual({
      entries: 220,
      sha256: 'fd9bb40e39e48e7588ddf542abe6681cf86682f7e8ee7eaf14f6ae6356e01572'
    });

    const tokyo = compilation.edits.find(edit =>
      edit.kind === 'add-sense' && edit.definition === 'Tokyo Metropolis');
    expect(tokyo).toMatchObject({ event: 214_703, seq: 1_447_690 });
    const aomori = compilation.edits.find(edit =>
      edit.kind === 'replace-gloss' && edit.definition === 'Aomori Prefecture');
    expect(aomori).toMatchObject({
      event: 214_704,
      seq: 2_845_121,
      oldGloss: 'Aomori (city, prefecture)'
    });
  });
});
