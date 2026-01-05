import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ために (for/because of - verb form)
 *
 * Verb/Adj/Noun + ために/ため = due to / because of
 * Indicates cause or reason
 *
 * Examples:
 * - 勝つために (in order to win)
 * - 飲んだために (due to drinking)
 * - 寒いために (due to cold)
 * - 大雨のため (due to heavy rain)
 *
 * GiNZA parses ために in various ways depending on what comes before:
 * - After noun+の: ため (NOUN) + に (ADP/AUX, lemma=で/に)
 * - After verb/adj: Often as copula form with lemma=だ
 *
 * This rule handles ために/ため after verbs, adjectives, and nouns
 */
export default bunproLinguisticRule('ために', (r) => {
  r.either(
    // Pattern 1: ため (NOUN) + に (ADP) with dep=case
    (b) => {
      const tame = b.noun({ lemma: 'ため' }, 'tame');
      const ni = b.tok({ text: 'に', dep: 'case' }, 'ni');
      b.headChild(tame, ni, 'case');
      b.captureSpan('ために', tame, ni);
    },

    // Pattern 2: ため (NOUN) + に (ADP/AUX) with dep=obl
    (b) => {
      const tame = b.noun({ lemma: 'ため' }, 'tame');
      const ni = b.tok({ text: 'に', dep: 'obl' }, 'ni');
      b.headChild(tame, ni, 'obl');
      b.captureSpan('ために', tame, ni);
    },

    // Pattern 3: ため (any POS) + に in order
    (b) => {
      const tame = b.tok({ text: 'ため' }, 'tame');
      const ni = b.tok({ text: 'に' }, 'ni');
      b.inOrder(tame, ni, 1);
      b.captureSpan('ために', tame, ni);
    },

    // Pattern 4: ため (NOUN) alone
    (b) => {
      const tame = b.noun({ lemma: 'ため' }, 'tame');
      b.captureSpan('ため', tame, tame);
    },

    // Pattern 5: ため (any POS) alone
    (b) => {
      const tame = b.tok({ text: 'ため' }, 'tame');
      b.captureSpan('ため', tame, tame);
    }
  );
});
