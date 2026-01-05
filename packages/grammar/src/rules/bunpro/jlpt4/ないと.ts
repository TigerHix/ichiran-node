import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ないと (conditional: "if not/do not")
 *
 * Matches verb[negative] + と as a conditional marker meaning "if not" or "must do".
 * Expresses that something must be done, often with the nuance that
 * if it's not done, something bad will happen.
 *
 * Examples:
 * - 勉強しないと、試験に合格できない。 (If you don't study, you can't pass the exam.)
 * - 早く行かないと、間に合いません。 (If you don't hurry, you won't make it.)
 * - もっと練習しないと、上手になりません。 (If you don't practice more, you won't improve.)
 * - 薬を飲まないと、治りません。 (If you don't take medicine, you won't get better.)
 *
 * Key discriminators from other と usages:
 * - Conditional と: pos=SCONJ, dep=mark (marks subordinate clauses)
 * - Quotation と: pos=ADP, dep=case (marks quotes/complements)
 * - Case marker と: pos=ADP, dep=case (marks nouns with nmod/obl deps)
 *
 * Structure:
 * - Verb in any form + ない (negative auxiliary) + と (conditional particle)
 *
 * The ない auxiliary can be:
 * - Dictionary form: ない (e.g., 行かないと)
 * - Contraction: ねば (e.g., 行かねばと - rare but possible)
 */
export default bunproLinguisticRule('ないと', (r) => {
  r.either(
    // Pattern 1: Standard form with ない + と (SCONJ, dep=mark)
    // e.g., 勉強しないと, 行かないと, 食べないと
    // Uses dep=mark to distinguish from quotation と (ADP, dep=case)
    (b) => {
      const nai = b.tok(
        {
          lemma: 'ない',
        },
        'nai'
      );

      const to = b.tok(
        {
          text: 'と',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'to'
      );

      b.inOrder(nai, to, 1);
      b.captureSpan('ないと', nai, to);
    },

    // Pattern 2: Standard form with ない + と (SCONJ, dep=case)
    // e.g., 勉強しないと, 行かないと (te-form verbs use dep=case)
    (b) => {
      const nai = b.tok(
        {
          lemma: 'ない',
        },
        'nai'
      );

      const to = b.tok(
        {
          text: 'と',
          pos: 'SCONJ',
          dep: 'case',
        },
        'to'
      );

      b.inOrder(nai, to, 1);
      b.captureSpan('ないと', nai, to);
    },

    // Pattern 3: Colloquial form with ん + と (SCONJ)
    // e.g., 勉強せんと, 行かんと (casual contraction)
    // GiNZA parses ん with lemma=ない
    (b) => {
      const n = b.tok(
        {
          text: 'ん',
          lemma: 'ない',
        },
        'n'
      );

      const to = b.tok(
        {
          text: 'と',
          pos: 'SCONJ',
        },
        'to'
      );

      b.inOrder(n, to, 1);
      b.captureSpan('ないと', n, to);
    },

    // Pattern 4: Sentence-final form with ない + と (ADP)
    // e.g., 「合格したらまず両親や先生につたえないと。」
    // Some sentence-final conditionals are parsed with と as ADP, dep=case
    // Note: This may also match some quotation patterns, but those are structurally
    // identical to conditionals (verb + nai + to + speech_verb), so we accept this limitation
    (b) => {
      const nai = b.tok(
        {
          lemma: 'ない',
        },
        'nai'
      );

      const to = b.tok(
        {
          text: 'と',
          pos: 'ADP',
          dep: 'case',
        },
        'to'
      );

      b.inOrder(nai, to, 1);
      b.captureSpan('ないと', nai, to);
    }
  );
});
