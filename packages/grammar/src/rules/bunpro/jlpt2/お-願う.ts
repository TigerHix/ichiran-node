import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('お-願う', (r) => {
  // Humble request form: お/ご + verb-stem/noun + 願う/願います/願えます
  // Examples:
  //   お確かめ願います (Please verify)
  //   お待ち願います (Please wait)
  //   ご確認願います (Please confirm)
  //   サイン願います (Please sign - no prefix for western words)
  //
  // Also handles hiragana forms:
  //   おまちねがいます (Please wait - hiragana)
  //   ごきょうりょくねがいます (Please cooperate - hiragana)

  r.either(
    // Pattern 1: お/ご prefix + verb-stem + 願います (polite, kanji)
    (b) => {
      const o = b.tok({
        textOneOf: ['お', 'ご'],
        pos: 'NOUN',
        dep: 'compound'
      }, 'o');

      const stem = b.tok({
        posOneOf: ['VERB', 'NOUN']
      }, 'stem');

      const negai = b.verb({
        lemma: '願う'
      }, 'negai');

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般'
      }, 'masu');

      b.inOrder(o, stem, 2);
      b.auxOf(negai, masu);
      b.captureSpan('お-願う', o, masu);
    },

    // Pattern 2: お/ご prefix + verb-stem + 願えます (polite potential)
    (b) => {
      const o = b.tok({
        textOneOf: ['お', 'ご'],
        pos: 'NOUN',
        dep: 'compound'
      }, 'o');

      const stem = b.tok({
        posOneOf: ['VERB', 'NOUN']
      }, 'stem');

      const negai = b.verb({
        lemma: '願う'
      }, 'negai');

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般'
      }, 'masu');

      const e = b.aux({
        lemma: '得る',
        inflectionForm: '連用形-一般'
      }, 'e');

      b.inOrder(o, stem, 2);
      b.auxOf(negai, masu);
      b.auxOf(masu, e);
      b.captureSpan('お-願う', o, e);
    },

    // Pattern 3: お/ご prefix + verb-stem + ねがいます (hiragana polite)
    // Handles cases where "ねがい" and "ます" are separate tokens
    (b) => {
      const o = b.tok({
        textOneOf: ['お', 'ご'],
        pos: 'NOUN',
        dep: 'compound'
      }, 'o');

      const stem = b.tok({
        posOneOf: ['VERB', 'NOUN']
      }, 'stem');

      const negai = b.verb({
        text: 'ねがい',
        lemma: '願う'
      }, 'negai');

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般'
      }, 'masu');

      b.inOrder(o, stem, 2);
      b.auxOf(negai, masu);
      b.captureSpan('お-願う', o, masu);
    },

    // Pattern 4: お/ご prefix + verb-stem + ねがいます (hiragana, single token)
    // Handles cases where "ねがいます" is a single verb token
    (b) => {
      const o = b.tok({
        textOneOf: ['お', 'ご'],
        pos: 'NOUN',
        dep: 'compound'
      }, 'o');

      const stem = b.tok({
        posOneOf: ['VERB', 'NOUN']
      }, 'stem');

      const negaimasu = b.verb({
        text: 'ねがいます',
        lemma: '願う'
      }, 'negaimasu');

      b.inOrder(o, stem, 2);
      b.inOrder(stem, negaimasu, 5);
      b.captureSpan('お-願う', o, negaimasu);
    },

    // Pattern 5: お/ご prefix + verb-stem + ねがえます (hiragana potential, single token)
    (b) => {
      const o = b.tok({
        textOneOf: ['お', 'ご'],
        pos: 'NOUN',
        dep: 'compound'
      }, 'o');

      const stem = b.tok({
        posOneOf: ['VERB', 'NOUN']
      }, 'stem');

      const negaemasu = b.verb({
        textRe: /^ねがえ/,
        lemma: '願う'
      }, 'negaemasu');

      b.inOrder(o, stem, 2);
      b.inOrder(stem, negaemasu, 5);
      b.captureSpan('お-願う', o, negaemasu);
    },

    // Pattern 6: noun/verb + 願います (no prefix - western words like サイン)
    // Must NOT match patterns like "幸せを願う" (object + を + 願う)
    (b) => {
      const stem = b.noun({}, 'stem');

      const negai = b.verb({
        lemma: '願う'
      }, 'negai');

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般'
      }, 'masu');

      // Don't match object-marker patterns (を + 願う)
      b.not((nb) => {
        const wo = nb.particle('を');
        nb.inOrder(stem, wo, 1);
        nb.inOrder(wo, negai, 2);
      });

      b.auxOf(negai, masu);
      b.captureSpan('お-願う', stem, masu);
    },

    // Pattern 7: noun/verb + ねがいます (hiragana, no prefix)
    (b) => {
      const stem = b.noun({}, 'stem');

      const negaimasu = b.verb({
        text: 'ねがいます',
        lemma: '願う'
      }, 'negaimasu');

      // Don't match object-marker patterns
      b.not((nb) => {
        const wo = nb.particle('を');
        nb.inOrder(stem, wo, 1);
        nb.inOrder(wo, negaimasu, 2);
      });

      b.inOrder(stem, negaimasu, 3);
      b.captureSpan('お-願う', stem, negaimasu);
    }
  );
});
