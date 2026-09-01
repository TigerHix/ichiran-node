import { beforeAll, describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { compileCanonicalCounters } from '../src/source-compiler/analyzer-support-counters.js';
import { compileCanonicalSuffixesFromGenerated } from '../src/source-compiler/analyzer-support-suffixes.js';
import { compileCanonicalRoots } from '../src/source-compiler/canonical-roots.js';
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

beforeAll(async () => {
  entries = (await compileCanonicalRoots(paths)).entries;
}, 30_000);

describe('source-native analyzer support facts', () => {
  test('counter declarations and canonical POS produce the complete qualified semantic set', () => {
    const counters = compileCanonicalCounters(entries);
    const digest = createHash('sha256')
      .update(counters.map(value => JSON.stringify(value)).sort().join('\n') + '\n')
      .digest('hex');

    expect(new Set(counters.map(value => value.key)).size).toBe(760);
    expect(counters).toHaveLength(799);
    expect(digest).toBe('22a711feb7d0395e1c880c5b0012e25e62ee0eb5868d33cafc157a376f888376');
    expect(counters.find(value => value.key === '週間後')).toEqual(expect.objectContaining({
      className: 'CounterText',
      suffix: 'かんご',
      suffixDescriptions: ['[after ...]']
    }));
  });

  test('suffix declarations compile without generated forms or a database', () => {
    const result = compileCanonicalSuffixesFromGenerated(entries, new Map());

    expect(result.suffixes).toHaveLength(76);
    expect(result.suffixes.reduce((count, value) => count + value.values.length, 0)).toBe(77);
    expect(result.suffixClasses).toHaveLength(47);
    expect(result.suffixes.find(value => value.text === 'ねば')).toEqual({
      text: 'ねば',
      values: [{ keyword: ':nakereba', form: null }]
    });
    expect(result.suffixes.find(value => value.text === 'ちゃ')?.values.map(value => value.keyword))
      .toEqual([':teba', ':chau']);
    expect(result.suffixes.find(value => value.text === 'ください')?.values[0]?.form)
      .toEqual(expect.objectContaining({ conjugations: ':root' }));
    expect(result.suffixes.find(value => value.text === 'です')?.values[0]?.form)
      .toEqual(expect.objectContaining({ conjugations: null }));
  });
});
