import { expect, test } from 'bun:test';

import { projectProductAnalysis } from '../src/public-analysis.js';
import type { PortableAnalysisResult, PortableAnalysisToken } from '../src/analyzer.js';

function token(): PortableAnalysisToken {
  const selected = {
    candidateId: 7,
    text: 'は',
    trueText: null,
    route: 'kana' as const,
    reading: '\u200cは',
    romanized: 'wa',
    pos: ['proper-noun'],
    score: 10,
    entryIndex: null,
    root: { seq: 1, form: 'は', reading: '\u200cは' },
    inflection: [],
    components: [],
    counter: ['Value: 3', false] as const
  };
  return {
    ...selected,
    start: 0,
    end: 1,
    alternatives: [selected, { ...selected, candidateId: 8, reading: 'ば' }],
    skipped: 0,
    entity: true
  };
}

test('qualification projection mirrors the clean Rust product wire contract', () => {
  const selected = token();
  const value: PortableAnalysisResult = {
    input: 'は',
    normalized: 'は',
    computeMs: 0,
    chunks: [{
      type: 'word', start: 0, end: 1, text: 'は',
      paths: [{ score: 10, tokens: [selected] }]
    }],
    paths: [{ score: 10, tokens: [selected] }]
  };

  const projected = projectProductAnalysis(value);
  for (const current of [
    projected.paths[0]!.tokens[0]!,
    projected.chunks[0]!.type === 'word'
      ? projected.chunks[0]!.paths[0]!.tokens[0]!
      : null
  ]) {
    expect(current).not.toBeNull();
    expect(current!.reading).toBe('は');
    expect(current!.root?.reading).toBe('は');
    expect(current!.pos).toEqual(['n-pr']);
    expect(current!.counter).toEqual(['3', false]);
    expect(current!.alternatives.map(candidate => candidate.candidateId)).toEqual([8]);
  }
  expect(value.paths[0]!.tokens[0]!.alternatives).toHaveLength(2);
});
