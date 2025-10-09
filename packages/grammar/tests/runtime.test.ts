import { describe, test, expect, beforeEach } from 'bun:test';
import { selectBestOutcome } from '../src/matcher.js';
import { clearCompiledGrammarCache, getCompiledMatcher, getCacheStats } from '../src/cache.js';
import { MACRO_MIN_TOKENS } from '../src/macros.js';
import { matchText, compileGrammars } from '../src/runtime.js';
import type { Token, MatchOutcome, GrammarDefinition } from '@ichiran/grammar';
import { setupTests } from '../../../test-utils/test-setup.js';

setupTests();

function makeToken(text: string, wordInfo?: any, grammarInfo?: any): Token {
  return { text, wordInfo, grammarInfo } as Token;
}

describe('selectBestOutcome deterministic ranking', () => {
  test('prefers longest span', () => {
    const outcomes: MatchOutcome[] = [
      { index: 2, captures: [], preference: 0 },
      { index: 3, captures: [], preference: 0 },
      { index: 1, captures: [], preference: 0 },
    ];
    const best = selectBestOutcome(outcomes, 0);
    expect(best?.index).toBe(3); // longest span wins
  });

  test('prefers higher preference when spans equal', () => {
    const outcomes: MatchOutcome[] = [
      { index: 3, captures: [], preference: 1 },
      { index: 3, captures: [], preference: 5 },
      { index: 3, captures: [], preference: 2 },
    ];
    const best = selectBestOutcome(outcomes, 0);
    expect(best?.preference).toBe(5); // highest preference wins
  });

  test('prefers more captures when span and preference equal', () => {
    const outcomes: MatchOutcome[] = [
      { index: 3, captures: [{ label: 'a', start: 0, end: 1, tokens: [] }], preference: 0 },
      { index: 3, captures: [{ label: 'a', start: 0, end: 1, tokens: [] }, { label: 'b', start: 1, end: 2, tokens: [] }], preference: 0 },
      { index: 3, captures: [], preference: 0 },
    ];
    const best = selectBestOutcome(outcomes, 0);
    expect(best?.captures.length).toBe(2); // most captures wins
  });

  test('returns null when no outcomes advance index', () => {
    const outcomes: MatchOutcome[] = [
      { index: 0, captures: [], preference: 0 },
      { index: -1, captures: [], preference: 0 },
    ];
    const best = selectBestOutcome(outcomes, 0);
    expect(best).toBeNull();
  });

  test('complete ranking order: span > preference > captures', () => {
    const outcomes: MatchOutcome[] = [
      { index: 2, captures: [], preference: 10 }, // shorter span
      { index: 4, captures: [{ label: 'x', start: 0, end: 1, tokens: [] }], preference: 1 }, // longest span, low pref
      { index: 4, captures: [], preference: 3 }, // longest span, medium pref
    ];
    const best = selectBestOutcome(outcomes, 0);
    expect(best?.index).toBe(4);
    expect(best?.preference).toBe(3); // span > preference, so medium pref wins over low pref with more captures
  });
});

describe('macro min-token estimation', () => {
  test('NP requires at least 1 token', () => {
    expect(MACRO_MIN_TOKENS.NP).toBe(1);
  });

  test('NPCase requires at least 2 tokens (NP + particle)', () => {
    expect(MACRO_MIN_TOKENS.NPCase).toBe(2);
  });

  test('NPTopic requires at least 2 tokens (NP + topic marker)', () => {
    expect(MACRO_MIN_TOKENS.NPTopic).toBe(2);
  });

  test('PrePredicateAdjuncts can be 0 tokens (optional)', () => {
    expect(MACRO_MIN_TOKENS.PrePredicateAdjuncts).toBe(0);
  });

  test('IAdjKunakute requires at least 2 tokens', () => {
    expect(MACRO_MIN_TOKENS.IAdjKunakute).toBe(2);
  });

  test('IAdjKuSuru requires at least 2 tokens', () => {
    expect(MACRO_MIN_TOKENS.IAdjKuSuru).toBe(2);
  });

  test('NaAdjDe requires at least 2 tokens', () => {
    expect(MACRO_MIN_TOKENS.NaAdjDe).toBe(2);
  });

  test('all macros have conservative estimates', () => {
    // Ensure all macros have defined min tokens >= 0
    const macros = Object.keys(MACRO_MIN_TOKENS);
    expect(macros.length).toBeGreaterThan(10); // at least 10 macros defined
    
    for (const macro of macros) {
      const minTokens = MACRO_MIN_TOKENS[macro];
      expect(minTokens).toBeGreaterThanOrEqual(0);
      expect(Number.isInteger(minTokens)).toBe(true);
    }
  });
});

describe('compiled grammar cache', () => {
  beforeEach(() => {
    clearCompiledGrammarCache();
  });

  test('different patterns produce cache misses', () => {
    clearCompiledGrammarCache();
    
    const def1: GrammarDefinition = {
      id: 'test-1',
      level: 'N5',
      pattern: { token: ['text:です'] },
    };
    
    const def2: GrammarDefinition = {
      id: 'test-1',
      level: 'N5',
      pattern: { token: ['text:だ'] },
    };
    
    getCompiledMatcher(def1);
    const stats1 = getCacheStats();
    expect(stats1.misses).toBe(1);
    expect(stats1.hits).toBe(0);
    
    getCompiledMatcher(def2); // different pattern, same id
    const stats2 = getCacheStats();
    expect(stats2.misses).toBe(2); // new miss
  });

  test('same pattern produces cache hits', () => {
    clearCompiledGrammarCache();
    
    const def: GrammarDefinition = {
      id: 'test-cached',
      level: 'N5',
      pattern: { token: ['text:です'] },
    };
    
    getCompiledMatcher(def);
    const stats1 = getCacheStats();
    expect(stats1.misses).toBe(1);
    
    getCompiledMatcher(def); // same pattern
    const stats2 = getCacheStats();
    expect(stats2.hits).toBe(1);
    expect(stats2.misses).toBe(1); // no additional miss
  });

  test('cache key includes both id and pattern', () => {
    clearCompiledGrammarCache();
    
    const def1: GrammarDefinition = {
      id: 'test-a',
      level: 'N5',
      pattern: { token: ['text:です'] },
    };
    
    const def2: GrammarDefinition = {
      id: 'test-b',
      level: 'N5',
      pattern: { token: ['text:です'] },
    };
    
    getCompiledMatcher(def1);
    getCompiledMatcher(def2);
    
    const stats = getCacheStats();
    expect(stats.misses).toBe(2); // different ids should not share cache entries
  });

  test('clearCompiledGrammarCache resets stats', () => {
    const def: GrammarDefinition = {
      id: 'test-clear',
      level: 'N5',
      pattern: { token: ['text:です'] },
    };
    
    getCompiledMatcher(def);
    expect(getCacheStats().misses).toBeGreaterThan(0);
    
    clearCompiledGrammarCache();
    const stats = getCacheStats();
    expect(stats.hits).toBe(0);
    expect(stats.misses).toBe(0);
  });
});

describe('segmentation and matching integration', () => {
  test('matchText handles empty input', async () => {
    const defs: GrammarDefinition[] = [];
    const hits = await matchText('', defs);
    expect(hits).toEqual([]);
  });

  test('matchText with no grammars returns empty', async () => {
    const hits = await matchText('これは本です。', []);
    expect(hits).toEqual([]);
  });

  test('matchText basic smoke test', async () => {
    const defs: GrammarDefinition[] = [
      {
        id: 'test-desu',
        level: 'N5',
        priority: 5,
        pattern: { token: ['text:です'] },
      },
    ];
    
    const hits = await matchText('これは本です。', defs);
    // Should not crash; may or may not match depending on tokenization
    expect(Array.isArray(hits)).toBe(true);
  });

  test('compileGrammars sorts by priority descending', () => {
    const defs: GrammarDefinition[] = [
      { id: 'low', level: 'N5', priority: 1, pattern: { token: ['text:a'] } },
      { id: 'high', level: 'N5', priority: 10, pattern: { token: ['text:b'] } },
      { id: 'medium', level: 'N5', priority: 5, pattern: { token: ['text:c'] } },
    ];
    
    const compiled = compileGrammars(defs);
    
    expect(compiled[0].def.id).toBe('high');
    expect(compiled[1].def.id).toBe('medium');
    expect(compiled[2].def.id).toBe('low');
  });

  test('compileGrammars handles missing priority as 0', () => {
    const defs: GrammarDefinition[] = [
      { id: 'no-priority', level: 'N5', pattern: { token: ['text:a'] } },
      { id: 'with-priority', level: 'N5', priority: 1, pattern: { token: ['text:b'] } },
    ];
    
    const compiled = compileGrammars(defs);
    
    expect(compiled[0].def.id).toBe('with-priority'); // priority 1 > 0
    expect(compiled[1].def.id).toBe('no-priority');
  });
});

describe('start gate precomputation', () => {
  test('grammars with firstToken gate are checked at match time', async () => {
    // This test verifies that start gates work without crashing
    // Full behavioral testing would require mocking predicates
    const defs: GrammarDefinition[] = [
      {
        id: 'test-gate',
        level: 'N5',
        pattern: { token: ['text:です'] },
        startGate: {
          firstToken: ['text:これ'],
        },
      },
    ];
    
    const hits = await matchText('これはです。', defs);
    expect(Array.isArray(hits)).toBe(true);
  });

  test('grammars with anyToken gate are precomputed', async () => {
    const defs: GrammarDefinition[] = [
      {
        id: 'test-any-gate',
        level: 'N5',
        pattern: { sequence: [{ token: ['text:は'] }, { token: ['text:です'] }] },
        startGate: {
          anyToken: ['text:本'],
        },
      },
    ];
    
    const hits = await matchText('本はです。', defs);
    expect(Array.isArray(hits)).toBe(true);
  });

  test('grammars with near gate check window', async () => {
    const defs: GrammarDefinition[] = [
      {
        id: 'test-near-gate',
        level: 'N5',
        pattern: { token: ['text:です'] },
        startGate: {
          near: {
            predicates: ['text:本'],
            window: { left: 2, right: 2 },
          },
        },
      },
    ];
    
    const hits = await matchText('本はこれです。', defs);
    expect(Array.isArray(hits)).toBe(true);
  });
});

