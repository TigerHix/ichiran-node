import { describe, test, expect } from 'bun:test';
import { matchText, grammarCatalog } from '@ichiran/grammar';
import { compilePattern } from '../src/compiler.js';
import type { MatchOutcome, Token } from '@ichiran/grammar';
import { setupTests } from '../../../test-utils/test-setup.js';

setupTests();

function makeToken(text: string): Token {
  return { text } as Token;
}

describe('Grammar matcher backtracking gaps', () => {
  test('sequence reconsiders earlier alt branches', async () => {
    const matcher = compilePattern({
      sequence: [
        {
          alt: [
            { token: ['text:寿司'] },
            {
              sequence: [
                { token: ['text:寿司'] },
                { token: ['text:が'] },
              ],
            },
          ],
        },
        { token: ['text:友達'] },
        { token: ['text:より'] },
        { token: ['text:好き'] },
      ],
    });

    const tokens = [
      makeToken('寿司'),
      makeToken('が'),
      makeToken('友達'),
      makeToken('より'),
      makeToken('好き'),
    ];

    const outcomes = await matcher(tokens, 0);
    const match = outcomes.find(o => o.index === tokens.length);
    expect(match?.index).toBe(tokens.length);
  });

  test('repeat shrinks when trailing matcher fails', async () => {
    const matcher = compilePattern({
      sequence: [
        {
          repeat: {
            pattern: {
              alt: [
                { token: ['text:A'] },
                { token: ['text:B'] },
              ],
            },
            min: 0,
            max: 2,
            greedy: true,
          },
        },
        { token: ['text:B'] },
        { token: ['text:C'] },
      ],
    });

    const tokens = [makeToken('A'), makeToken('B'), makeToken('C')];

    const outcomes = await matcher(tokens, 0);
    const match = outcomes.find(o => o.index === tokens.length);
    expect(match?.index).toBe(tokens.length);
  });

  test('repeat keeps distinct capture labels', async () => {
    const matcher = compilePattern({
      repeat: {
        pattern: {
          alt: [
            { capture: 'foo@0', pattern: { token: ['text:A'] } },
            { capture: 'bar|alt', pattern: { token: ['text:A'] } },
          ],
        },
        min: 1,
        max: 1,
      },
    });

    const tokens = [makeToken('A')];
    const outcomes = await matcher(tokens, 0);
    const labels = outcomes.flatMap(o => o.captures.map(c => c.label));

    expect(labels).toContain('foo@0');
    expect(labels).toContain('bar|alt');
    expect(labels.length).toBe(2);
  });

  test('repeat.until stops before sentinel (greedy)', async () => {
    const matcher = compilePattern({
      sequence: [
        {
          repeat: {
            pattern: { token: ['text:X'] },
            min: 0,
            max: 10,
            greedy: true,
            until: { peek: { token: ['text:Y'] } },
          },
        },
        { token: ['text:Y'] },
      ],
    });

    const tokens = [
      makeToken('X'),
      makeToken('X'),
      makeToken('Y'),
      makeToken('X'),
    ];
    const outcomes = await matcher(tokens, 0);
    const match = outcomes.find(o => o.index === 3); // stop at Y
    expect(match?.index).toBe(3);
  });

  test('repeat.until allows zero repetitions when sentinel immediate', async () => {
    const matcher = compilePattern({
      sequence: [
        {
          repeat: {
            pattern: { token: ['text:X'] },
            min: 0,
            max: 10,
            greedy: true,
            until: { peek: { token: ['text:Y'] } },
          },
        },
        { token: ['text:Y'] },
      ],
    });

    const tokens = [makeToken('Y')];
    const outcomes = await matcher(tokens, 0);
    const match = outcomes.find(o => o.index === 1);
    expect(match?.index).toBe(1);
  });

  test('repeat keeps distinct capture spans', async () => {
    const matcher = compilePattern({
      repeat: {
        pattern: {
          alt: [
            {
              sequence: [
                {
                  capture: 'segment',
                  pattern: {
                    sequence: [
                      { token: ['text:A'] },
                      { token: ['text:B'] },
                    ],
                  },
                },
                { token: ['text:C'] },
                { token: ['text:D'] },
              ],
            },
            {
              sequence: [
                { token: ['text:A'] },
                {
                  capture: 'segment',
                  pattern: {
                    sequence: [
                      { token: ['text:B'] },
                      { token: ['text:C'] },
                    ],
                  },
                },
                { token: ['text:D'] },
              ],
            },
          ],
        },
        min: 1,
        max: 1,
      },
    });

    const tokens = [makeToken('A'), makeToken('B'), makeToken('C'), makeToken('D')];
    const outcomes = await matcher(tokens, 0);
    const signatures = outcomes.map(outcome => outcome.captures.map(capture => `${capture.label}:${capture.tokens.map(t => t.text).join('')}`));

    expect(signatures).toContainEqual(['segment:AB']);
    expect(signatures).toContainEqual(['segment:BC']);
    expect(signatures.length).toBe(2);
  });

  test('repeat greedy vs lazy preference propagation', async () => {
    const tokens = [makeToken('X'), makeToken('X'), makeToken('X')];

    const greedyMatcher = compilePattern({
      repeat: {
        pattern: { token: ['text:X'] },
        min: 1,
        max: 3,
        greedy: true,
      },
    });

    const lazyMatcher = compilePattern({
      repeat: {
        pattern: { token: ['text:X'] },
        min: 1,
        max: 3,
        greedy: false,
      },
    });

    const greedyOutcomes = await greedyMatcher(tokens, 0);
    const greedyOutcome = greedyOutcomes.reduce((best, outcome) => {
      if (!best || outcome.index > best.index) return outcome;
      return best;
    }, null as MatchOutcome | null);

    const lazyOutcomes = await lazyMatcher(tokens, 0);
    const lazyOutcome = lazyOutcomes.reduce((best, outcome) => {
      if (!best || outcome.index < best.index) return outcome;
      return best;
    }, null as MatchOutcome | null);

    expect(greedyOutcome?.index).toBe(3);
    expect(lazyOutcome?.index).toBe(1);
  });
});

describe('Macro compilation tests', () => {
  test('NPTopic macro compiles successfully', () => {
    expect(() => compilePattern({ macro: 'NPTopic' })).not.toThrow();
  });

  test('NaAdjNi macro compiles successfully', () => {
    expect(() => compilePattern({ macro: 'NaAdjNi' })).not.toThrow();
  });

  test('NaAdjNiNaru macro compiles successfully', () => {
    expect(() => compilePattern({ macro: 'NaAdjNiNaru' })).not.toThrow();
  });

  test('NaAdjNiSuru macro compiles successfully', () => {
    expect(() => compilePattern({ macro: 'NaAdjNiSuru' })).not.toThrow();
  });

  test('IAdjKuNaru macro compiles successfully', () => {
    expect(() => compilePattern({ macro: 'IAdjKuNaru' })).not.toThrow();
  });

  test('IAdjKuSuru macro compiles successfully', () => {
    expect(() => compilePattern({ macro: 'IAdjKuSuru' })).not.toThrow();
  });

  test('IAdjKunakute macro compiles successfully', () => {
    expect(() => compilePattern({ macro: 'IAdjKunakute' })).not.toThrow();
  });

  test('NaAdjNegativeConnective macro compiles successfully', () => {
    expect(() => compilePattern({ macro: 'NaAdjNegativeConnective' })).not.toThrow();
  });

  test('NaAdjDeshite macro compiles successfully', () => {
    expect(() => compilePattern({ macro: 'NaAdjDeshite' })).not.toThrow();
  });
});

describe('Macro integration tests', () => {
  test('IAdjKunakute matches negative connective forms in real sentences', async () => {
    // Test the n5.i-adj-kute grammar with negative examples from its test set
    const testSentences = [
      '寒くなくて、助かりました。',  // It wasn't cold, which was a relief
      '遅くなくて、間に合った。',      // It wasn't late, so I made it in time
    ];

    for (const sentence of testSentences) {
      const hits = await matchText(sentence, grammarCatalog.filter(g => g.id === 'n5.i-adj-kute'));
      expect(hits.length).toBeGreaterThan(0);
      expect(hits.some(h => h.grammarId === 'n5.i-adj-kute')).toBe(true);
    }
  });

  test('NaAdjNiSuru macro matches causative forms in real sentences', async () => {
    // Test the n5.na-adj-ni-shimasu grammar with examples from its test set
    const testSentences = [
      '部屋をきれいにする。',        // I'll make the room clean
      '図書室では静かにしてください。', // Please keep quiet in the library
    ];

    for (const sentence of testSentences) {
      const hits = await matchText(sentence, grammarCatalog.filter(g => g.id === 'n5.na-adj-ni-shimasu'));
      expect(hits.length).toBeGreaterThan(0);
      expect(hits.some(h => h.grammarId === 'n5.na-adj-ni-shimasu')).toBe(true);
    }
  });

  test('NaAdjNiNaru macro matches change-of-state forms in real sentences', async () => {
    // Test the n5.na-adj-ni-narimasu grammar with examples from its test set
    const testSentences = [
      'だんだん静かになります。',      // It's gradually becoming quiet
      'この町はしだいににぎやかになった。', // This town gradually became lively
    ];

    for (const sentence of testSentences) {
      const hits = await matchText(sentence, grammarCatalog.filter(g => g.id === 'n5.na-adj-ni-narimasu'));
      expect(hits.length).toBeGreaterThan(0);
      expect(hits.some(h => h.grammarId === 'n5.na-adj-ni-narimasu')).toBe(true);
    }
  });

  test('IAdjKuNaru macro matches i-adjective change-of-state forms in real sentences', async () => {
    // Test the n4.i-adj-ku-suru-naru grammar with くなる examples
    const testSentences = [
      'だんだん寒くなってきた。',      // It has gradually gotten cold
      '夜になるとこの道は暗くなる。',   // This road gets dark at night
    ];

    for (const sentence of testSentences) {
      const hits = await matchText(sentence, grammarCatalog.filter(g => g.id === 'n4.i-adj-ku-suru-naru'));
      expect(hits.length).toBeGreaterThan(0);
      expect(hits.some(h => h.grammarId === 'n4.i-adj-ku-suru-naru')).toBe(true);
    }
  });

  test('NaAdjNegativeConnective macro matches negative connective forms in real sentences', async () => {
    // Test the n5.na-adj-de grammar with negative examples
    const testSentences = [
      'ここはにぎやかではなくて、落ち着いた雰囲気です。',  // It isn't lively; it has a calm atmosphere
      'そのアプリは便利じゃなくて、使いにくい。',         // That app isn't convenient and is hard to use
    ];

    for (const sentence of testSentences) {
      const hits = await matchText(sentence, grammarCatalog.filter(g => g.id === 'n5.na-adj-de'));
      expect(hits.length).toBeGreaterThan(0);
      expect(hits.some(h => h.grammarId === 'n5.na-adj-de')).toBe(true);
    }
  });

  test('NaAdjDeshite macro matches formal connective forms in real sentences', async () => {
    // Test the n5.na-adj-de grammar with formal でして examples
    const testSentences = [
      'ここは静かでして、勉強に集中できます。',  // It is quiet here, so I can focus on studying
      'この店は有名でして、予約が必要です。',    // This shop is famous and reservations are necessary
    ];

    for (const sentence of testSentences) {
      const hits = await matchText(sentence, grammarCatalog.filter(g => g.id === 'n5.na-adj-de'));
      expect(hits.length).toBeGreaterThan(0);
      expect(hits.some(h => h.grammarId === 'n5.na-adj-de')).toBe(true);
    }
  });
});