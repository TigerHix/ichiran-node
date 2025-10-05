import { describe, test, expect, beforeAll } from 'bun:test';
import { compileGrammars, matchGrammars, segmentSentence, matchSentence } from '../../src/grammarMatcher/runtime.js';
import { compilePattern } from '../../src/grammarMatcher/compiler.js';
import { grammarCatalog } from '../../src/grammarMatcher/catalog.js';
import type { CompiledGrammar } from '../../src/grammarMatcher/runtime.js';
import type { MatchHit, MatchOutcome, Token } from '../../src/grammarMatcher/types.js';
import { setupTests } from '../test-setup.js';

setupTests();

function makeToken(text: string): Token {
  return { text } as Token;
}

describe('Grammar matcher backtracking gaps', () => {
  test('sequence reconsiders earlier alt branches', () => {
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

    const outcomes = matcher(tokens, 0);
    const match = outcomes.find(o => o.index === tokens.length);
    expect(match?.index).toBe(tokens.length);
  });

  test('repeat shrinks when trailing matcher fails', () => {
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

    const outcomes = matcher(tokens, 0);
    const match = outcomes.find(o => o.index === tokens.length);
    expect(match?.index).toBe(tokens.length);
  });

  test('repeat keeps distinct capture labels', () => {
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
    const outcomes = matcher(tokens, 0);
    const labels = outcomes.flatMap(o => o.captures.map(c => c.label));

    expect(labels).toContain('foo@0');
    expect(labels).toContain('bar|alt');
    expect(labels.length).toBe(2);
  });

  test('repeat.until stops before sentinel (greedy)', () => {
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
    const outcomes = matcher(tokens, 0);
    const match = outcomes.find(o => o.index === 3); // stop at Y
    expect(match?.index).toBe(3);
  });

  test('repeat.until allows zero repetitions when sentinel immediate', () => {
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
    const outcomes = matcher(tokens, 0);
    const match = outcomes.find(o => o.index === 1);
    expect(match?.index).toBe(1);
  });

  test('repeat keeps distinct capture spans', () => {
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
    const outcomes = matcher(tokens, 0);
    const signatures = outcomes.map(outcome => outcome.captures.map(capture => `${capture.label}:${capture.tokens.map(t => t.text).join('')}`));

    expect(signatures).toContainEqual(['segment:AB']);
    expect(signatures).toContainEqual(['segment:BC']);
    expect(signatures.length).toBe(2);
  });

  test('repeat greedy vs lazy preference propagation', () => {
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

    const greedyOutcome = greedyMatcher(tokens, 0).reduce((best, outcome) => {
      if (!best || outcome.index > best.index) return outcome;
      return best;
    }, null as MatchOutcome | null);

    const lazyOutcome = lazyMatcher(tokens, 0).reduce((best, outcome) => {
      if (!best || outcome.index < best.index) return outcome;
      return best;
    }, null as MatchOutcome | null);

    expect(greedyOutcome?.index).toBe(3);
    expect(lazyOutcome?.index).toBe(1);
  });
});