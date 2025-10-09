import { describe, test, expect, beforeAll } from 'bun:test';
import { setupTests } from '../../../test-utils/test-setup.js';
import { resolvePredicate } from '../src/predicates.js';
import type { Token, PredicateContext } from '@ichiran/grammar';
import type { IchiranPos } from '../src/pos.js';
import { WordInfo } from '@ichiran/core';

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
  // Ensure POS is always an array
  const pos = overrides?.pos 
    ? (Array.isArray(overrides.pos) ? overrides.pos : [overrides.pos])
    : ['unknown' as IchiranPos];

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
    test('matches exact text', async () => {
      const predicate = resolvePredicate('text:寿司');
      const token = makeToken('寿司');
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match different text', async () => {
      const predicate = resolvePredicate('text:寿司');
      const token = makeToken('ラーメン');
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('matches with regex pattern', async () => {
      const predicate = resolvePredicate('text:/寿.*/');
      const token = makeToken('寿司');
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('returns false when no value provided', async () => {
      const predicate = resolvePredicate('text:');
      const token = makeToken('寿司');
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('kana predicate', () => {
    test('matches single kana', async () => {
      const predicate = resolvePredicate('kana:すし');
      const token = makeToken('寿司', { kana: 'すし' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches one of multiple kana readings', async () => {
      const predicate = resolvePredicate('kana:いく');
      const token = makeToken('行く', { kana: ['いく', 'ゆく'] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches with regex pattern', async () => {
      const predicate = resolvePredicate('kana:/す.*/');
      const token = makeToken('寿司', { kana: 'すし' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match different kana', async () => {
      const predicate = resolvePredicate('kana:らーめん');
      const token = makeToken('寿司', { kana: 'すし' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('returns false for tokens without wordInfo', async () => {
      const predicate = resolvePredicate('kana:すし');
      const token = makeToken('寿司');
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('pos predicate', () => {
    test('matches single POS', async () => {
      const predicate = resolvePredicate('pos:n');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches one of multiple POS', async () => {
      const predicate = resolvePredicate('pos:v1');
      const token = makeToken('食べる', { pos: ['v1', 'vt'] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match different POS', async () => {
      const predicate = resolvePredicate('pos:adj-i');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('returns false for tokens without grammarInfo', async () => {
      const predicate = resolvePredicate('pos:n');
      const token = makeToken('猫');
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('lemma predicate', () => {
    test('matches lemma from wordInfo components', async () => {
      const predicate = resolvePredicate('lemma:食べ');
      const components = [
        new WordInfo({ type: 'kanji', text: '食べ', kana: 'たべ' }),
        new WordInfo({ type: 'kanji', text: 'る', kana: 'る' })
      ];
      // Create token with components but empty grammarInfo.word so it falls through to wordInfo.components[0].text
      const token: Token = {
        text: '食べる',
        wordInfo: new WordInfo({ type: 'kanji', text: '食べる', kana: 'たべる', components }),
        grammarInfo: {
          partOfSpeech: ['v1' as any],
          word: '', // Empty so it falls through to components
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
        },
      };
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match different lemma', async () => {
      const predicate = resolvePredicate('lemma:飲み');
      const components = [
        new WordInfo({ type: 'kanji', text: '食べ', kana: 'たべ' }),
        new WordInfo({ type: 'kanji', text: 'る', kana: 'る' })
      ];
      const token = makeToken('食べる', { components });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('returns false for tokens without components', async () => {
      const predicate = resolvePredicate('lemma:食べ');
      const token = makeToken('食べる');
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('conj predicate', () => {
    test('matches :root conjugation', async () => {
      const predicate = resolvePredicate('conj::root');
      const token = makeToken('食べる', { conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches numeric conjugation', async () => {
      const predicate = resolvePredicate('conj:3');
      const token = makeToken('食べて', { conj: [3] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches one of multiple conjugations', async () => {
      const predicate = resolvePredicate('conj:2');
      const token = makeToken('食べた', { conj: [2, 11] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match different conjugation', async () => {
      const predicate = resolvePredicate('conj:5');
      const token = makeToken('食べて', { conj: [3] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('returns false for tokens without conjugations', async () => {
      const predicate = resolvePredicate('conj:3');
      const token = makeToken('食べて');
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('type predicate', () => {
    test('matches word type', async () => {
      const predicate = resolvePredicate('type:kanji');
      const token = makeToken('食べる', { type: 'kanji' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match different type', async () => {
      const predicate = resolvePredicate('type:kana');
      const token = makeToken('食べる', { type: 'kanji' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('hasVerbalConjugation predicate', () => {
    test('matches v1 verb', async () => {
      const predicate = resolvePredicate('hasVerbalConjugation');
      const token = makeToken('食べる', { pos: 'v1' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches v5 verb', async () => {
      const predicate = resolvePredicate('hasVerbalConjugation');
      const token = makeToken('書く', { pos: 'v5k' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches transitive verb', async () => {
      const predicate = resolvePredicate('hasVerbalConjugation');
      const token = makeToken('食べる', { pos: 'vt' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match noun', async () => {
      const predicate = resolvePredicate('hasVerbalConjugation');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isAdjective predicate', () => {
    test('matches i-adjective', async () => {
      const predicate = resolvePredicate('isAdjective');
      const token = makeToken('高い', { pos: 'adj-i' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches na-adjective', async () => {
      const predicate = resolvePredicate('isAdjective');
      const token = makeToken('静か', { pos: 'adj-na' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match noun', async () => {
      const predicate = resolvePredicate('isAdjective');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isAdverb predicate', () => {
    test('matches adverb', async () => {
      const predicate = resolvePredicate('isAdverb');
      const token = makeToken('とても', { pos: 'adv' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches adverb-to', async () => {
      const predicate = resolvePredicate('isAdverb');
      const token = makeToken('ゆっくり', { pos: 'adv-to' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match adjective', async () => {
      const predicate = resolvePredicate('isAdverb');
      const token = makeToken('高い', { pos: 'adj-i' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isAuxiliary predicate', () => {
    test('matches auxiliary verb', async () => {
      const predicate = resolvePredicate('isAuxiliary');
      const token = makeToken('れる', { pos: 'aux-v' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches copula', async () => {
      const predicate = resolvePredicate('isAuxiliary');
      const token = makeToken('だ', { pos: 'cop-da' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match verb', async () => {
      const predicate = resolvePredicate('isAuxiliary');
      const token = makeToken('食べる', { pos: 'v1' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isIAdjective predicate', () => {
    test('matches adj-i', async () => {
      const predicate = resolvePredicate('isIAdjective');
      const token = makeToken('高い', { pos: 'adj-i' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches adj-ix', async () => {
      const predicate = resolvePredicate('isIAdjective');
      const token = makeToken('良い', { pos: 'adj-ix' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match na-adjective', async () => {
      const predicate = resolvePredicate('isIAdjective');
      const token = makeToken('静か', { pos: 'adj-na' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isNaAdjective predicate', () => {
    test('matches na-adjective', async () => {
      const predicate = resolvePredicate('isNaAdjective');
      const token = makeToken('静か', { pos: 'adj-na' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective', async () => {
      const predicate = resolvePredicate('isNaAdjective');
      const token = makeToken('高い', { pos: 'adj-i' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
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
    test('matches particle', async () => {
      const predicate = resolvePredicate('isParticle');
      const token = makeToken('が', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match noun', async () => {
      const predicate = resolvePredicate('isParticle');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isTopicMarker predicate', () => {
    test('matches は as topic particle', async () => {
      const predicate = resolvePredicate('isTopicMarker');
      const token = makeToken('は', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match other particles', async () => {
      const predicate = resolvePredicate('isTopicMarker');
      const token = makeToken('が', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('does not match は without particle POS', async () => {
      const predicate = resolvePredicate('isTopicMarker');
      const token = makeToken('は', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isQuestionParticle predicate', () => {
    test('matches か as question particle', async () => {
      const predicate = resolvePredicate('isQuestionParticle');
      const token = makeToken('か', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match other particles', async () => {
      const predicate = resolvePredicate('isQuestionParticle');
      const token = makeToken('は', { pos: 'prt' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('does not match か without particle POS', async () => {
      const predicate = resolvePredicate('isQuestionParticle');
      const token = makeToken('か', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isCounter predicate', () => {
    test('matches counter', async () => {
      const predicate = resolvePredicate('isCounter');
      const token = makeToken('つ', { pos: 'ctr' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match noun', async () => {
      const predicate = resolvePredicate('isCounter');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isOrdinal predicate', () => {
    test('matches ordinal with 番目', async () => {
      const predicate = resolvePredicate('isOrdinal');
      const token = makeToken('1番目', { pos: 'num' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches ordinal with 第', async () => {
      const predicate = resolvePredicate('isOrdinal');
      const token = makeToken('第一', { pos: 'num' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match regular number', async () => {
      const predicate = resolvePredicate('isOrdinal');
      const token = makeToken('一', { pos: 'num' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isAdverbModifier predicate', () => {
    test('matches adverb', async () => {
      const predicate = resolvePredicate('isAdverbModifier');
      const token = makeToken('とても', { pos: 'adv' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches i-adjective in ku-form', async () => {
      const predicate = resolvePredicate('isAdverbModifier');
      const token = makeToken('早く', { pos: 'adj-i', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective in root form', async () => {
      const predicate = resolvePredicate('isAdverbModifier');
      const token = makeToken('早い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('does not match noun', async () => {
      const predicate = resolvePredicate('isAdverbModifier');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isPredicateHead predicate', () => {
    test('matches verb in root form', async () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('食べる', { pos: 'v1', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches adjective in root form', async () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('高い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches copula in root form', async () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('だ', { pos: 'cop-da', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match verb in conjugated form', async () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('食べて', { pos: 'v1', conj: [3] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('does not match noun', async () => {
      const predicate = resolvePredicate('isPredicateHead');
      const token = makeToken('猫', { pos: 'n' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isIAdjKuForm predicate', () => {
    test('matches i-adjective in ku-form', async () => {
      const predicate = resolvePredicate('isIAdjKuForm');
      const token = makeToken('早く', { pos: 'adj-i', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective in root form', async () => {
      const predicate = resolvePredicate('isIAdjKuForm');
      const token = makeToken('早い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('does not match i-adjective in different conjugation', async () => {
      const predicate = resolvePredicate('isIAdjKuForm');
      const token = makeToken('早くて', { pos: 'adj-i', conj: [3] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('does not match na-adjective', async () => {
      const predicate = resolvePredicate('isIAdjKuForm');
      const token = makeToken('静か', { pos: 'adj-na', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isIAdjKunaiForm predicate', () => {
    test('matches i-adjective in kunai-form', async () => {
      const predicate = resolvePredicate('isIAdjKunaiForm');
      const token: Token = {
        text: '高くない',
        wordInfo: new WordInfo({ type: 'kanji', text: '高くない', kana: 'たかくない' }),
        grammarInfo: {
          partOfSpeech: ['adj-i' as any],
          word: '高くない',
          reading: '',
          meanings: [],
          conjugations: [{ neg: true, partOfSpeech: ['adj-i' as any] } as any],
          alternatives: [],
          components: [],
          isConjugation: false,
          hasConjugationVia: false,
          isConjugationVia: false,
          isAlternative: false,
          isComponent: false,
          isSuffix: false,
        },
      };
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective in root form', async () => {
      const predicate = resolvePredicate('isIAdjKunaiForm');
      const token = makeToken('高い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('does not match i-adjective in different conjugation', async () => {
      const predicate = resolvePredicate('isIAdjKunaiForm');
      const token = makeToken('高く', { pos: 'adj-i', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isIAdjKuteForm predicate', () => {
    test('matches i-adjective in kute-form', async () => {
      const predicate = resolvePredicate('isIAdjKuteForm');
      const token: Token = {
        text: '早くて',
        wordInfo: new WordInfo({ type: 'kanji', text: '早くて', kana: 'はやくて' }),
        grammarInfo: {
          partOfSpeech: ['adj-i' as any],
          word: '早くて',
          reading: '',
          meanings: [],
          conjugations: [{ conjugationTypes: ['Conjunctive (~te)'], partOfSpeech: ['adj-i' as any] } as any],
          alternatives: [],
          components: [],
          isConjugation: false,
          hasConjugationVia: false,
          isConjugationVia: false,
          isAlternative: false,
          isComponent: false,
          isSuffix: false,
        },
      };
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match i-adjective in root form', async () => {
      const predicate = resolvePredicate('isIAdjKuteForm');
      const token = makeToken('早い', { pos: 'adj-i', conj: ':root' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });

    test('does not match i-adjective in different conjugation', async () => {
      const predicate = resolvePredicate('isIAdjKuteForm');
      const token = makeToken('早く', { pos: 'adj-i', conj: [13] });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });


  // Note: isNaAdjDeForm predicate does not exist - the NaAdjDe macro handles this pattern
  // Tests for isCopulaDe would go here if needed

  describe('isSuruVerb predicate', () => {
    test('matches suru verb (vs)', async () => {
      const predicate = resolvePredicate('isSuruVerb');
      const token = makeToken('勉強', { pos: 'vs' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches suru verb (vs-i)', async () => {
      const predicate = resolvePredicate('isSuruVerb');
      const token = makeToken('する', { pos: 'vs-i' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches suru verb (vs-s)', async () => {
      const predicate = resolvePredicate('isSuruVerb');
      const token = makeToken('愛する', { pos: 'vs-s' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match regular verb', async () => {
      const predicate = resolvePredicate('isSuruVerb');
      const token = makeToken('食べる', { pos: 'v1' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });

  describe('isNaruVerb predicate', () => {
    test('matches なる by text', async () => {
      const predicate = resolvePredicate('isNaruVerb');
      const token = makeToken('なる', { kana: 'なる', pos: 'v5r' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('matches なる by kana', async () => {
      const predicate = resolvePredicate('isNaruVerb');
      const token = makeToken('成る', { kana: 'なる', pos: 'v5r' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(true);
    });

    test('does not match different verb', async () => {
      const predicate = resolvePredicate('isNaruVerb');
      const token = makeToken('食べる', { kana: 'たべる', pos: 'v1' });
      const ctx = makeContext([token], 0);
      expect(await predicate(token, ctx)).toBe(false);
    });
  });
});