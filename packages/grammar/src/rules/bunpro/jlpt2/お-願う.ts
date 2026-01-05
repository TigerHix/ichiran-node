import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: お～願う (o-negau) - Humble request with honorific prefix
 *
 * A humble honorific construction used to make polite requests. The speaker
 * petitions someone to do something in a formal, dignified manner. Similar to
 * お～ください but even more formal and polite, typically used in business
 * contexts, public announcements, or formal writing.
 *
 * Structure:
 * - お + Verb[stem] + 願う (for Japanese-origin verbs)
 * - ご + Noun(Sino-Japanese) + 願う (for Chinese-origin nouns)
 * - Noun(loanword) + 願う (for Western-origin words, no prefix)
 *
 * Examples:
 * - お待ち願いまます (Please wait)
 * - ご確認願います (Please check/confirm)
 * - サイン願います (Please sign)
 * - お静かに願います (Please be quiet)
 *
 * Key discriminators:
 * - Honorific prefix お or ご (NOUN, dep=compound)
 * - Followed by verb stem or noun
 * - Ends with 願う/願います (lemma=願う)
 * - More formal than お～ください or てください
 * - Different from お願いします (polite "please" greeting)
 * - Different from literal "wish for [noun]" patterns (noun + を + 願う)
 *
 * GiNZA parse structure:
 * - お/ご (NOUN, dep=compound)
 * - verb stem or noun (VERB/NOUN with dep=compound/obl)
 * - 願う (VERB) or 願います (VERB+ AUX with lemma=ます)
 * - Literal wishes have: NOUN (dep=obj) + を (dep=case) + 願う
 */
export default linguisticRule('お-願う', (r) => {
  r.either(
    // Pattern 1: お/ご + verb-stem/noun + 願います/ねがいます (polite form - most common)
    (b1) => {
      const o = b1.tok({ textOneOf: ['お', 'ご'], pos: 'NOUN', dep: 'compound' }, 'o');
      const stem = b1.tok({ posOneOf: ['VERB', 'NOUN'] }, 'stem');
      const negai = b1.verb({ lemmaOneOf: ['願う', 'ねがう'] }, 'negai');
      const masu = b1.aux({ lemma: 'ます' }, 'masu');

      b1.inOrder(o, stem, 1);
      b1.inOrder(stem, negai, 3);
      b1.auxOf(negai, masu);

      // Exclude: お願いします (different grammar - "please" greeting)
      // This has し (lemma=する) instead of just 願い (lemma=願う)
      b1.not((nr) => {
        const shi = nr.aux({ lemma: 'する' });
        nr.inOrder(negai, shi, 1);
      });

      b1.captureSpan('お-願う', o, masu);
    },

    // Pattern 1b: お/ご + stem (dep=advmod) + 願います (for adverbial stems like へんじ)
    (b1b) => {
      const o = b1b.tok({ textOneOf: ['お', 'ご'], pos: 'NOUN', dep: 'compound' }, 'o');
      const stem = b1b.tok({ posOneOf: ['VERB', 'NOUN', 'ADV'], dep: 'advmod' }, 'stem');
      const negai = b1b.verb({ lemmaOneOf: ['願う', 'ねがう'] }, 'negai');
      const masu = b1b.aux({ lemma: 'ます' }, 'masu');

      b1b.inOrder(o, stem, 1);
      b1b.inOrder(stem, negai, 3);
      b1b.auxOf(negai, masu);

      // Exclude: お願いします
      b1b.not((nr) => {
        const shi = nr.aux({ lemma: 'する' });
        nr.inOrder(negai, shi, 1);
      });

      b1b.captureSpan('お-願う', o, masu);
    },

    // Pattern 2: お/ご + adj-stem + に + 願います (adverbial form like 静かに)
    (b2) => {
      const o = b2.tok({ textOneOf: ['お', 'ご'], pos: 'NOUN', dep: 'compound' }, 'o');
      const adjStem = b2.tok({ posOneOf: ['ADJ', 'NOUN', 'VERB'] }, 'adjStem');
      const ni = b2.aux({ text: 'に', lemma: 'だ' }, 'ni');
      const negai = b2.verb({ lemmaOneOf: ['願う', 'ねがう'] }, 'negai');
      const masu = b2.aux({ lemma: 'ます' }, 'masu');

      b2.inOrder(o, adjStem, 1);
      b2.inOrder(adjStem, ni, 1);
      b2.inOrder(ni, negai, 3);
      b2.auxOf(negai, masu);

      b2.captureSpan('お-願う', o, masu);
    },

    // Pattern 3: noun/verb + 願います (loanwords like サイン, no prefix)
    // Key: stem has dep=obl (oblique), NOT dep=obj (direct object)
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'VERB'], dep: 'obl' }, 'noun');
      const negai = b3.verb({ lemmaOneOf: ['願う', 'ねがう'] }, 'negai');
      const masu = b3.aux({ lemma: 'ます' }, 'masu');

      b3.inOrder(noun, negai, 2);
      b3.auxOf(negai, masu);

      b3.captureSpan('お-願う', noun, masu);
    },

    // Pattern 4: noun/verb + 願う (loanwords without polite form)
    // Key: stem has dep=obl, NOT dep=obj (to exclude literal "wish for X")
    // Also exclude when followed by に particle (directional, like 神に願う)
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'VERB'], dep: 'obl' }, 'noun');
      const negau = b4.verb({ lemmaOneOf: ['願う', 'ねがう'] }, 'negau');

      b4.inOrder(noun, negau, 2);

      // Exclude: potential forms (願える/ねがえる)
      b4.not((nr) => {
        const potential = nr.aux({ lemmaOneOf: ['れる', 'られる'] });
        nr.inOrder(negau, potential, 2);
      });

      // Exclude: noun + に + 願う (directional "pray to X", not humble request)
      b4.not((nr) => {
        const ni = nr.particle('に');
        nr.inOrder(noun, ni, 1);
      });

      b4.captureSpan('お-願う', noun, negau);
    },

    // Pattern 5: お/ご + verb-stem/noun + 願う (basic form without polite)
    // Key: stem has dep=compound (attached to honorific prefix)
    (b5) => {
      const o = b5.tok({ textOneOf: ['お', 'ご'], pos: 'NOUN', dep: 'compound' }, 'o');
      const stem = b5.tok({ posOneOf: ['VERB', 'NOUN'], dep: 'compound' }, 'stem');
      const negau = b5.verb({ lemmaOneOf: ['願う', 'ねがう'] }, 'negau');

      b5.inOrder(o, stem, 1);
      b5.inOrder(stem, negau, 3);

      // Exclude potential forms
      b5.not((nr) => {
        const potential = nr.aux({ lemmaOneOf: ['れる', 'られる'] });
        nr.inOrder(negau, potential, 2);
      });

      b5.captureSpan('お-願う', o, negau);
    },

    // Pattern 6: Potential form - お/ご + stem + 願えます/ねがえます (can you please?)
    (b6) => {
      const o = b6.tok({ textOneOf: ['お', 'ご'], pos: 'NOUN', dep: 'compound' }, 'o');
      const stem = b6.tok({ posOneOf: ['VERB', 'NOUN'] }, 'stem');
      const negaeru = b6.verb({ lemmaOneOf: ['願う', 'ねがう', 'ねがえる'] }, 'negaeru');
      const masu = b6.aux({ lemma: 'ます' }, 'masu');

      b6.inOrder(o, stem, 1);
      b6.inOrder(stem, negaeru, 3);
      b6.auxOf(negaeru, masu);

      b6.captureSpan('お-願う', o, masu);
    }
  );
});
