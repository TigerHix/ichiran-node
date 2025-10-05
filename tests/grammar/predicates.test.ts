import { describe, test, expect, beforeAll } from 'bun:test';
import { setupTests } from '../test-setup.js';
import { resolvePredicate } from '../../src/grammarMatcher/predicates.js';
import type { Token, PredicateContext } from '../../src/grammarMatcher/types.js';
import type { IchiranPos } from '../../src/grammarMatcher/pos.js';
import { WordInfo } from '../../src/dict/wordInfo.js';

setupTests();

// Helper to create a mock token
function makeToken(
  text: string,
  overrides?: {
    kana?: string | string[];
    pos?: IchiranPos | IchiranPos[];
    lemma?: string;
    conj?: number[] | ':root';
    type?: string;
    components?: WordInfo[];
  }
): Token {
  // Extract the first POS if an array is provided
  const pos = overrides?.pos 
    ? (Array.isArray(overrides.pos) ? overrides.pos[0] : overrides.pos)
    : 'unknown';

  const token: Token = {
    text,
    wordInfo: overrides ? new WordInfo({
      type: 'kanji',
      text,
      kana: overrides.kana || text,
      conjugations: overrides.conj,
      components: overrides.components,
    }) : undefined,
    grammarInfo: overrides ? {
      partOfSpeech: pos,
      word: text,
      reading: '',
      meanings: [],
      conjugations: [],
      alternatives: [],
      components: [],
      isConjugation: false,
      hasConjugationVia: false,
      isConjugationVia: false,
      isAlternative: false,
      isComponent: false,
      isSuffix: false,
    } : undefined,
  };

  if (overrides?.type && token.wordInfo) {
    (token.wordInfo as any).type = overrides.type;
  }

  return token;
}

// Helper to create a predicate context
function makeContext(tokens: Token[], index: number): PredicateContext {
  return {
    tokens,
    index,
    prev: index > 0 ? tokens[index - 1] : undefined,
    next: index < tokens.length - 1 ? tokens[index + 1] : undefined,
  };
}

describe('Grammar matcher predicates', () => {
  describe('text predicate', () => {
    test('matches exact text', () => {
      const predicate = resolvePredicate('text:寿司');
      const token = makeToken('寿司');
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match different text', () => {
      const predicate = resolvePredicate('text:寿司');
      const token = makeToken('ラーメン');
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('matches with regex pattern', () => {
      const predicate = resolvePredicate('text:/寿.*/');
      const token = makeToken('寿司');
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('returns false when no value provided', () => {
      const predicate = resolvePredicate('text:');
      const token = makeToken('寿司');
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('kana predicate', () => {
    test('matches single kana', () => {
      const predicate = resolvePredicate('kana:すし');
      const token = makeToken('寿司', { kana: 'すし' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches one of multiple kana readings', () => {
      const predicate = resolvePredicate('kana:いく');
      const token = makeToken('行く', { kana: ['いく', 'ゆく'] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches with regex pattern', () => {
      const predicate = resolvePredicate('kana:/す.*/');
      const token = makeToken('寿司', { kana: 'すし' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match different kana', () => {
      const predicate = resolvePredicate('kana:らーめん');
      const token = makeToken('寿司', { kana: 'すし' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('returns false for tokens without wordInfo', () => {
      const predicate = resolvePredicate('kana:すし');
      const token = makeToken('寿司');
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('pos predicate', () => {
    test('matches single POS', () => {
      const predicate = resolvePredicate('pos:n');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches one of multiple POS', () => {
      const predicate = resolvePredicate('pos:v1');
      const token = makeToken('食べる', { pos: ['v1', 'vt'] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match different POS', () => {
      const predicate = resolvePredicate('pos:adj-i');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('returns false for tokens without grammarInfo', () => {
      const predicate = resolvePredicate('pos:n');
      const token = makeToken('猫');
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('lemma predicate', () => {
    test('matches lemma from first component', () => {
      const predicate = resolvePredicate('lemma:食べ');
      const components = [
        new WordInfo({ type: 'kanji', text: '食べ', kana: 'たべ' }),
        new WordInfo({ type: 'kanji', text: 'る', kana: 'る' })
      ];
      const token = makeToken('食べる', { components });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match different lemma', () => {
      const predicate = resolvePredicate('lemma:飲み');
      const components = [
        new WordInfo({ type: 'kanji', text: '食べ', kana: 'たべ' }),
        new WordInfo({ type: 'kanji', text: 'る', kana: 'る' })
      ];
      const token = makeToken('食べる', { components });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('returns false for tokens without components', () => {
      const predicate = resolvePredicate('lemma:食べ');
      const token = makeToken('食べる');
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('conj predicate', () => {
    test('matches :root conjugation', () => {
      const predicate = resolvePredicate('conj::root');
      const token = makeToken('食べる', { conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches numeric conjugation', () => {
      const predicate = resolvePredicate('conj:3');
      const token = makeToken('食べて', { conj: [3] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches one of multiple conjugations', () => {
      const predicate = resolvePredicate('conj:2');
      const token = makeToken('食べた', { conj: [2, 11] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match different conjugation', () => {
      const predicate = resolvePredicate('conj:5');
      const token = makeToken('食べて', { conj: [3] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('returns false for tokens without conjugations', () => {
      const predicate = resolvePredicate('conj:3');
      const token = makeToken('食べて');
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('type predicate', () => {
    test('matches word type', () => {
      const predicate = resolvePredicate('type:kanji');
      const token = makeToken('食べる', { type: 'kanji' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match different type', () => {
      const predicate = resolvePredicate('type:kana');
      const token = makeToken('食べる', { type: 'kanji' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isVerb predicate', () => {
    test('matches v1 verb', () => {
      const predicate = resolvePredicate('isVerb');
      const token = makeToken('食べる', { pos: 'v1' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches v5 verb', () => {
      const predicate = resolvePredicate('isVerb');
      const token = makeToken('書く', { pos: 'v5k' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches transitive verb', () => {
      const predicate = resolvePredicate('isVerb');
      const token = makeToken('食べる', { pos: 'vt' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match noun', () => {
      const predicate = resolvePredicate('isVerb');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isAdjective predicate', () => {
    test('matches i-adjective', () => {
      const predicate = resolvePredicate('isAdjective');
      const token = makeToken('高い', { pos: 'adj-i' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches na-adjective', () => {
      const predicate = resolvePredicate('isAdjective');
      const token = makeToken('静か', { pos: 'adj-na' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match noun', () => {
      const predicate = resolvePredicate('isAdjective');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isAdverb predicate', () => {
    test('matches adverb', () => {
      const predicate = resolvePredicate('isAdverb');
      const token = makeToken('とても', { pos: 'adv' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches adverb-to', () => {
      const predicate = resolvePredicate('isAdverb');
      const token = makeToken('ゆっくり', { pos: 'adv-to' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match adjective', () => {
      const predicate = resolvePredicate('isAdverb');
      const token = makeToken('高い', { pos: 'adj-i' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isAuxiliary predicate', () => {
    test('matches auxiliary verb', () => {
      const predicate = resolvePredicate('isAuxiliary');
      const token = makeToken('れる', { pos: 'aux-v' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches copula', () => {
      const predicate = resolvePredicate('isAuxiliary');
      const token = makeToken('だ', { pos: 'cop-da' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match verb', () => {
      const predicate = resolvePredicate('isAuxiliary');
      const token = makeToken('食べる', { pos: 'v1' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isIAdjective predicate', () => {
    test('matches adj-i', () => {
      const predicate = resolvePredicate('isIAdjective');
      const token = makeToken('高い', { pos: 'adj-i' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches adj-ix', () => {
      const predicate = resolvePredicate('isIAdjective');
      const token = makeToken('良い', { pos: 'adj-ix' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match na-adjective', () => {
      const predicate = resolvePredicate('isIAdjective');
      const token = makeToken('静か', { pos: 'adj-na' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isNaAdjective predicate', () => {
    test('matches na-adjective', () => {
      const predicate = resolvePredicate('isNaAdjective');
      const token = makeToken('静か', { pos: 'adj-na' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective', () => {
      const predicate = resolvePredicate('isNaAdjective');
      const token = makeToken('高い', { pos: 'adj-i' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isNominalHead predicate', () => {
    test('matches noun', async () => {
      const predicate = resolvePredicate('isNominalHead');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches pronoun', async () => {
      const predicate = resolvePredicate('isNominalHead');
      const token = makeToken('彼', { pos: 'pn' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match prenoun adjective', async () => {
      const predicate = resolvePredicate('isNominalHead');
      const token = makeToken('この', { pos: 'adj-pn' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('matches adj-no', async () => {
      const predicate = resolvePredicate('isNominalHead');
      const token = makeToken('たくさん', { pos: 'adj-no' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match verb', async () => {
      const predicate = resolvePredicate('isNominalHead');
      const token = makeToken('食べる', { pos: 'v1' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isNounPhrase predicate', () => {
    test('matches noun', async () => {
      const predicate = resolvePredicate('isNounPhrase');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches pronoun', async () => {
      const predicate = resolvePredicate('isNounPhrase');
      const token = makeToken('彼', { pos: 'pn' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches pronominal の before は', async () => {
      const predicate = resolvePredicate('isNounPhrase');
      const noToken = makeToken('の', { pos: 'prt' });
      const waToken = makeToken('は', { pos: 'prt' });
      const ctx = makeContext([noToken, waToken], 0);
      expect(await predicate(noToken, ctx)).toBe(true);
    });

    test('matches pronominal の before か', async () => {
      const predicate = resolvePredicate('isNounPhrase');
      const noToken = makeToken('の', { pos: 'prt' });
      const kaToken = makeToken('か', { pos: 'prt' });
      const ctx = makeContext([noToken, kaToken], 0);
      expect(await predicate(noToken, ctx)).toBe(true);
    });

    test('matches pronominal の before より', async () => {
      const predicate = resolvePredicate('isNounPhrase');
      const noToken = makeToken('の', { pos: 'prt' });
      const yoriToken = makeToken('より', { pos: 'prt' });
      const ctx = makeContext([noToken, yoriToken], 0);
      expect(await predicate(noToken, ctx)).toBe(true);
    });

    test('does not match connective の before noun', async () => {
      const predicate = resolvePredicate('isNounPhrase');
      const noToken = makeToken('の', { pos: 'prt' });
      const nounToken = makeToken('代表', { pos: 'n' });
      const ctx = makeContext([noToken, nounToken], 0);
      expect(await predicate(noToken, ctx)).toBe(false);
    });

    test('does not match prenoun adjective', async () => {
      const predicate = resolvePredicate('isNounPhrase');
      const token = makeToken('この', { pos: 'adj-pn' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isParticle predicate', () => {
    test('matches particle', () => {
      const predicate = resolvePredicate('isParticle');
      const token = makeToken('が', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match noun', () => {
      const predicate = resolvePredicate('isParticle');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isTopicParticle predicate', () => {
    test('matches は as topic particle', () => {
      const predicate = resolvePredicate('isTopicParticle');
      const token = makeToken('は', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match other particles', () => {
      const predicate = resolvePredicate('isTopicParticle');
      const token = makeToken('が', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match は without particle POS', () => {
      const predicate = resolvePredicate('isTopicParticle');
      const token = makeToken('は', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isQuestionParticle predicate', () => {
    test('matches か as question particle', () => {
      const predicate = resolvePredicate('isQuestionParticle');
      const token = makeToken('か', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match other particles', () => {
      const predicate = resolvePredicate('isQuestionParticle');
      const token = makeToken('は', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match か without particle POS', () => {
      const predicate = resolvePredicate('isQuestionParticle');
      const token = makeToken('か', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isCounter predicate', () => {
    test('matches counter', () => {
      const predicate = resolvePredicate('isCounter');
      const token = makeToken('つ', { pos: 'ctr' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match noun', () => {
      const predicate = resolvePredicate('isCounter');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isOrdinal predicate', () => {
    test('matches ordinal with 番目', () => {
      const predicate = resolvePredicate('isOrdinal');
      const token = makeToken('1番目', { pos: 'num' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches ordinal with 第', () => {
      const predicate = resolvePredicate('isOrdinal');
      const token = makeToken('第一', { pos: 'num' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match regular number', () => {
      const predicate = resolvePredicate('isOrdinal');
      const token = makeToken('一', { pos: 'num' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isAdverbModifier predicate', () => {
    test('matches adverb', () => {
      const predicate = resolvePredicate('isAdverbModifier');
      const token = makeToken('とても', { pos: 'adv' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches i-adjective in ku-form', () => {
      const predicate = resolvePredicate('isAdverbModifier');
      const token = makeToken('早く', { pos: 'adj-i', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective in root form', () => {
      const predicate = resolvePredicate('isAdverbModifier');
      const token = makeToken('早い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match noun', () => {
      const predicate = resolvePredicate('isAdverbModifier');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isPredicateHead predicate', () => {
    test('matches verb in root form', () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('食べる', { pos: 'v1', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches adjective in root form', () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('高い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches copula in root form', () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('だ', { pos: 'cop-da', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match verb in conjugated form', () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('食べて', { pos: 'v1', conj: [3] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match noun', () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isIAdjKuForm predicate', () => {
    test('matches i-adjective in ku-form', () => {
      const predicate = resolvePredicate('isIAdjKuForm');
      const token = makeToken('早く', { pos: 'adj-i', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective in root form', () => {
      const predicate = resolvePredicate('isIAdjKuForm');
      const token = makeToken('早い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match i-adjective in different conjugation', () => {
      const predicate = resolvePredicate('isIAdjKuForm');
      const token = makeToken('早くて', { pos: 'adj-i', conj: [3] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match na-adjective', () => {
      const predicate = resolvePredicate('isIAdjKuForm');
      const token = makeToken('静か', { pos: 'adj-na', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isIAdjKunaiForm predicate', () => {
    test('matches i-adjective in kunai-form', () => {
      const predicate = resolvePredicate('isIAdjKunaiForm');
      const token = makeToken('高く', { pos: 'adj-i', conj: [52] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective in root form', () => {
      const predicate = resolvePredicate('isIAdjKunaiForm');
      const token = makeToken('高い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match i-adjective in different conjugation', () => {
      const predicate = resolvePredicate('isIAdjKunaiForm');
      const token = makeToken('高く', { pos: 'adj-i', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isIAdjKuteForm predicate', () => {
    test('matches i-adjective in kute-form', () => {
      const predicate = resolvePredicate('isIAdjKuteForm');
      const token = makeToken('早くて', { pos: 'adj-i', conj: [3] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective in root form', () => {
      const predicate = resolvePredicate('isIAdjKuteForm');
      const token = makeToken('早い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match i-adjective in different conjugation', () => {
      const predicate = resolvePredicate('isIAdjKuteForm');
      const token = makeToken('早く', { pos: 'adj-i', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });


  describe('isNaAdjDeForm predicate', () => {
    test('matches na-adjective in de-form', () => {
      const predicate = resolvePredicate('isNaAdjDeForm');
      const token = makeToken('静かで', { pos: 'adj-na', conj: [3] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match na-adjective in root form', () => {
      const predicate = resolvePredicate('isNaAdjDeForm');
      const token = makeToken('静か', { pos: 'adj-na', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match na-adjective in different conjugation', () => {
      const predicate = resolvePredicate('isNaAdjDeForm');
      const token = makeToken('静かに', { pos: 'adj-na', conj: [50] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });

    test('does not match i-adjective', () => {
      const predicate = resolvePredicate('isNaAdjDeForm');
      const token = makeToken('高くて', { pos: 'adj-i', conj: [3] });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isSuruVerb predicate', () => {
    test('matches suru verb (vs)', () => {
      const predicate = resolvePredicate('isSuruVerb');
      const token = makeToken('勉強', { pos: 'vs' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches suru verb (vs-i)', () => {
      const predicate = resolvePredicate('isSuruVerb');
      const token = makeToken('する', { pos: 'vs-i' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches suru verb (vs-s)', () => {
      const predicate = resolvePredicate('isSuruVerb');
      const token = makeToken('愛する', { pos: 'vs-s' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match regular verb', () => {
      const predicate = resolvePredicate('isSuruVerb');
      const token = makeToken('食べる', { pos: 'v1' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });

  describe('isNaruVerb predicate', () => {
    test('matches なる by text', () => {
      const predicate = resolvePredicate('isNaruVerb');
      const token = makeToken('なる', { kana: 'なる' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('matches なる by kana', () => {
      const predicate = resolvePredicate('isNaruVerb');
      const token = makeToken('成る', { kana: 'なる' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(true);
    });

    test('does not match different verb', () => {
      const predicate = resolvePredicate('isNaruVerb');
      const token = makeToken('食べる', { kana: 'たべる' });
      const ctx = makeContext([token], 0);
      expect(predicate(token, ctx)).toBe(false);
    });
  });
});