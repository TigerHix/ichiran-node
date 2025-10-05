import { describe, test, expect, beforeAll } from 'bun:test';
import { compileGrammars, matchGrammars, segmentSentence, matchSentence } from '../../src/grammarMatcher/runtime.js';
import { compilePattern } from '../../src/grammarMatcher/compiler.js';
import type { CompiledGrammar } from '../../src/grammarMatcher/runtime.js';
import type { MatchHit, MatchOutcome, Token, GrammarDefinition } from '../../src/grammarMatcher/types.js';
import { setupTests } from '../test-setup.js';

setupTests();

// Test grammar definitions
const testGrammars: GrammarDefinition[] = [
  {
    id: 'test-ga-suki',
    level: 'N5',
    description: 'X が好き pattern (X is liked)',
    priority: 10,
    pattern: {
      sequence: [
        {
          capture: 'subject',
          pattern: { token: ['isNominalHead'] }
        },
        { token: ['text:が'] },
        {
          capture: 'predicate',
          pattern: { token: ['text:好き'] }
        }
      ]
    }
  },
  {
    id: 'test-ichiban-suki',
    level: 'N5',
    description: 'X が一番好き pattern (X is liked the most)',
    priority: 20,
    pattern: {
      sequence: [
        {
          capture: 'subject',
          pattern: { token: ['isNominalHead'] }
        },
        { token: ['text:が'] },
        { token: ['text:一番'] },
        {
          capture: 'predicate',
          pattern: { token: ['text:好き'] }
        }
      ]
    }
  }
];

describe('Grammar matcher integration', () => {
  let compiled: CompiledGrammar[];

  beforeAll(() => {
    compiled = compileGrammars(testGrammars);
  });

  test('matchSentence returns same hits as manual pipeline', async () => {
    const sentence = '寿司が一番好きです。';
    const [autoHit] = await matchSentence(sentence, testGrammars);
    const tokens = await segmentSentence(sentence);
    const manual = (await matchGrammars(tokens, compiled))[0];
    expect(autoHit?.grammarId).toBe(manual?.grammarId);
    expect(autoHit?.captures.length).toBe(manual?.captures.length);
  });
});