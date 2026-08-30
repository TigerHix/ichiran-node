import { describe, expect, test } from 'bun:test';
import { resolve } from 'node:path';
import corpus from '../../../browser-alpha/bench/corpus.json';
import cli from '../../cli/tests/data/cli.json';
import hardCli from '../../cli/tests/data/hard-cli.json';
import { loadAnalyzerParityCorpus } from '../../core/tools/parity-corpus.js';

const repository = resolve(import.meta.dir, '..', '..', '..');
const cliPath = 'packages/cli/tests/data/cli.json';
const hardPath = 'packages/cli/tests/data/hard-cli.json';

function cliSlice(start: number, end: number) {
  return cli.fullJson.slice(start, end).map((request, offset) => ({
    source: cliPath,
    index: start + offset,
    text: request.text,
    limit: request.limit
  }));
}

describe('browser benchmark corpus', () => {
  test('materializes every exact acceptance slice with source metadata', async () => {
    const parity = await loadAnalyzerParityCorpus(repository);
    expect(corpus.formatVersion).toBe(2);
    expect(corpus.groups.ordinary).toEqual(cliSlice(3, 102));
    expect(corpus.groups['pathological-morphology']).toEqual(
      hardCli.fullJson.slice(0, 50).map((request, index) => ({
        source: hardPath,
        index,
        text: request.text,
        limit: request.limit
      }))
    );
    expect(corpus.groups['segmentation-short']).toEqual(
      parity.segmentation
        .map((fixture, index) => ({
          source: 'packages/reference-postgres/tests/data/segmentation.json',
          index,
          text: fixture.input,
          limit: 1
        }))
        .filter(request => request.text.length <= 12)
    );
    expect(corpus.groups['segmentation-short']).toHaveLength(459);
    expect(corpus.groups['long-noun-compound']).toEqual(cliSlice(102, 152));
    expect(corpus.groups['hiragana-colloquial']).toEqual(cliSlice(152, 202));
    expect(corpus.groups['modern-mixed-script']).toEqual(cliSlice(202, 252));
    expect(corpus.groups['top-n']).toEqual(cliSlice(1, 3));
    expect(corpus.groups.entities).toEqual(parity.entities.map((fixture, index) => ({
      source: 'packages/reference-postgres/tests/entity-hints.test.ts',
      index,
      title: fixture.title,
      text: fixture.text,
      limit: 1,
      entities: fixture.entities
    })));
    expect(corpus.groups.counters).toEqual(parity.counters.map((request, index) => ({
      source: 'packages/reference-postgres/tests/counters.test.ts',
      index,
      ...request
    })));
    expect(corpus.groups.numbers).toEqual([
      { source: 'packages/reference-postgres/tests/numbers.test.ts', index: 0, text: '100万', limit: 1 },
      { source: 'packages/reference-postgres/tests/numbers.test.ts', index: 1, text: '100万500', limit: 1 },
      { source: 'packages/reference-postgres/tests/numbers.test.ts', index: 2, text: '〇', limit: 1 },
      { source: 'packages/reference-postgres/tests/numbers.test.ts', index: 3, text: '一万一', limit: 1 },
      { source: 'packages/reference-postgres/tests/numbers.test.ts', index: 4, text: '二千二万一', limit: 1 },
      { source: 'packages/reference-postgres/tests/numbers.test.ts', index: 5, text: '百二十四億二千三百万四百三十', limit: 1 },
      { source: 'packages/reference-postgres/tests/number-split.test.ts', index: 0, text: '二〇二〇', limit: 1 }
    ]);
    expect(corpus.groups['describe-random-access']).toEqual(cliSlice(3, 53));
  });
});
