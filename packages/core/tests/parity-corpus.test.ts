import { describe, expect, test } from 'bun:test';

import {
  fixtureKey,
  loadAnalyzerParityCorpus
} from '../tools/parity-corpus.js';

describe('oracle parity corpus', () => {
  test('locks broad fixtures and deterministic analyzer probes', async () => {
    const corpus = await loadAnalyzerParityCorpus(process.cwd());
    expect(corpus.segmentation).toHaveLength(534);
    expect(corpus.cli).toHaveLength(252);
    expect(corpus.hard).toHaveLength(149);
    expect(corpus.counters).toHaveLength(200);
    expect(corpus.entities).toHaveLength(54);

    expect(new Set(corpus.probes.map(value => value.category))).toEqual(new Set([
      'top-n',
      'counter-number',
      'normalization',
      'punctuation-chunks',
      'generated-exception'
    ]));
    expect(corpus.probes.filter(value => value.category === 'top-n')
      .map(value => value.request.limit)).toEqual([1, 2, 3, 5, 10]);
    expect(corpus.probes.filter(value => value.category === 'punctuation-chunks')
      .map(value => value.request.normalizePunctuation)).toEqual([false, true]);
    expect(corpus.probes.some(value => value.name === 'generated-two-stage')).toBeTrue();
    expect(corpus.probes.some(value => value.name === 'generated-shared-target')).toBeTrue();
  });

  test('normalization mode is part of a probe request key without changing old fixture keys', () => {
    expect(fixtureKey({ text: '猫', limit: 1 })).toBe('猫|1');
    expect(fixtureKey({
      text: '猫。犬',
      limit: 5,
      normalizePunctuation: false
    })).toBe('猫。犬|5|normalizePunctuation=false');
  });
});
