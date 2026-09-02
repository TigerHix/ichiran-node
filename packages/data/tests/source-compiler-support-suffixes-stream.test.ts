import { afterAll, beforeAll, describe, expect, test } from 'bun:test';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { compileBoundedCanonicalSuffixes } from '../src/source-compiler/analyzer-support-suffixes-stream.js';
import { compileCanonicalRoots } from '../src/source-compiler/canonical-roots.js';
import { GeneratedProjectionSpoolWriter } from '../src/source-compiler/generated-projection-spool.js';
import type { GeneratedProjectionStreamResult } from '../src/source-compiler/generated-projection-stream.js';
import type { CanonicalEntry } from '../src/source-compiler/model.js';

const paths = {
  jmdict: fileURLToPath(new URL('../JMdict_e.gz', import.meta.url)),
  extra: fileURLToPath(new URL('../../../data/sources/extra.xml', import.meta.url)),
  municipality: fileURLToPath(new URL('../../../data/sources/jichitai.csv', import.meta.url)),
  ward: fileURLToPath(new URL('../../../data/sources/gyoseiku.csv', import.meta.url)),
  errata: fileURLToPath(new URL('../../../data/source-compiler-errata.json', import.meta.url)),
  compatibility: fileURLToPath(new URL('../../../data/source-compiler-compatibility.json', import.meta.url))
};
let entries: readonly CanonicalEntry[];
let directory: string;

beforeAll(async () => {
  entries = (await compileCanonicalRoots(paths)).entries;
  directory = mkdtempSync(join(tmpdir(), 'ichiran-suffix-spool-'));
}, 30_000);

afterAll(() => rmSync(directory, { recursive: true, force: true }));

describe('bounded source-native suffix forms', () => {
  test('uses complete physical target forms and lexical target metadata', () => {
    const pathsPath = join(directory, 'paths.bin');
    const occurrencesPath = join(directory, 'occurrences.bin');
    const writer = new GeneratedProjectionSpoolWriter(pathsPath, occurrencesPath);
    writer.writePath({
      ordinal: 0,
      rootSeq: 2_027_910,
      firstAlias: 0,
      secondAlias: null,
      targetSeq: 3_000_000,
      viaTargetSeq: null
    });
    writer.writeOccurrence({
      pathOrdinal: 0,
      precedence: 0,
      firstRule: 0,
      secondRule: null,
      route: 'kana',
      kind: 'emission',
      installed: true,
      surface: 'つつあった',
      physicalCounterpart: null
    });
    writer.writeOccurrence({
      pathOrdinal: 0,
      precedence: 0,
      firstRule: 0,
      secondRule: null,
      route: 'kanji',
      kind: 'emission',
      installed: false,
      surface: '仮の物理形',
      physicalCounterpart: 'つつあられた'
    });
    writer.writePath({
      ordinal: 1,
      rootSeq: 2_027_910,
      firstAlias: 2,
      secondAlias: null,
      targetSeq: 3_000_000,
      viaTargetSeq: null
    });
    writer.writePath({
      ordinal: 2,
      rootSeq: 1_631_750,
      firstAlias: 1,
      secondAlias: null,
      targetSeq: 1_003_340,
      viaTargetSeq: null
    });
    writer.writeOccurrence({
      pathOrdinal: 2,
      precedence: 1,
      firstRule: 1,
      secondRule: null,
      route: 'kana',
      kind: 'emission',
      installed: true,
      surface: 'がり',
      physicalCounterpart: null
    });
    writer.writePath({
      ordinal: 3,
      rootSeq: 2_017_560,
      firstAlias: 3,
      secondAlias: null,
      targetSeq: 2_654_250,
      viaTargetSeq: null
    });
    writer.writeOccurrence({
      pathOrdinal: 3,
      precedence: 2,
      firstRule: 2,
      secondRule: null,
      route: 'kana',
      kind: 'emission',
      installed: true,
      surface: 'た',
      physicalCounterpart: null
    });
    writer.writePath({
      ordinal: 4,
      rootSeq: 1_013_240,
      firstAlias: 4,
      secondAlias: null,
      targetSeq: 2_827_332,
      viaTargetSeq: null
    });
    writer.writeOccurrence({
      pathOrdinal: 4,
      precedence: 3,
      firstRule: 3,
      secondRule: null,
      route: 'kana',
      kind: 'emission',
      installed: true,
      surface: 'らしき',
      physicalCounterpart: null
    });
    writer.close();

    const lexical = entries.find(entry => entry.seq === 1_003_340)!;
    const tai = entries.find(entry => entry.seq === 2_654_250)!;
    const rashii = entries.find(entry => entry.seq === 2_827_332)!;
    const projection = {
      pathsPath,
      occurrencesPath,
      targets: [{
        seq: 3_000_000,
        kanji: ['仮の物理形'],
        kana: ['つつあった', 'つつあられた'],
        secondaryForms: [],
        conjugatable: false,
        origin: 'generated'
      }, {
        seq: lexical.seq,
        kanji: lexical.kanji.map(form => form.text),
        kana: lexical.kana.map(form => form.text),
        secondaryForms: [
          ...lexical.kanji.filter(form => form.conjugatable).map(form => ({
            route: 'kanji' as const, text: form.text, counterpart: form.best
          })),
          ...lexical.kana.filter(form => form.conjugatable).map(form => ({
            route: 'kana' as const, text: form.text, counterpart: form.best
          }))
        ],
        conjugatable: true,
        origin: 'lexical'
      }, ...[tai, rashii].map(entry => ({
        seq: entry.seq,
        kanji: entry.kanji.map(form => form.text),
        kana: entry.kana.map(form => form.text),
        secondaryForms: [],
        conjugatable: true,
        origin: 'lexical' as const
      }))],
      aliasProperties: [
        { pos: 'v5r-i', type: 2, negative: false, formal: false },
        { pos: 'v5r', type: 13, negative: null, formal: null },
        { pos: 'v5r-i', type: 53, negative: false, formal: false },
        { pos: 'adj-i', type: 13, negative: null, formal: null },
        { pos: 'adj-i', type: 54, negative: null, formal: null }
      ]
    } as GeneratedProjectionStreamResult;
    const result = compileBoundedCanonicalSuffixes({
      entries,
      projection
    });

    expect(result.suffixes.find(value => value.text === 'つつあった')?.values[0]?.form)
      .toEqual(expect.objectContaining({
        seq: 3_000_000,
        ord: 0,
        conjugations: expect.arrayContaining([
          expect.objectContaining({ from: 2_027_910, type: 2 }),
          expect.objectContaining({ from: 2_027_910, type: 53 })
        ])
      }));
    expect(result.suffixes.find(value => value.text === 'つつあられた')?.values[0]?.form)
      .toEqual(expect.objectContaining({
        seq: 3_000_000,
        ord: 1,
        conjugations: expect.arrayContaining([
          expect.objectContaining({ from: 2_027_910, type: 2 }),
          expect.objectContaining({ from: 2_027_910, type: 53 })
        ])
      }));
    expect(result.suffixes.find(value => value.text === 'ガリ')?.values[0]?.form)
      .toEqual(expect.objectContaining({
        seq: 1_003_340,
        ord: 1,
        conjugations: null
      }));
    expect(result.suffixes.find(value => value.text === 'がり')?.values[0]?.form)
      .toEqual(expect.objectContaining({
        seq: 1_003_340,
        ord: 0,
        conjugations: [expect.objectContaining({ from: 1_631_750, type: 13 })]
      }));
    expect(result.suffixes.some(value => value.text === 'だ')).toBe(false);
    expect(result.suffixes.some(value => value.text === 'らしき')).toBe(false);
  });
});
