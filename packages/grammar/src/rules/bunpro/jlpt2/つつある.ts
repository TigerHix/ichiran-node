import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: つつある (tsutsu aru) - "in the process of, gradually ~ing"
 *
 * A formal grammar pattern expressing that something is in the process of changing
 * or undergoing a gradual transition. Similar to ている but with more formal tone
 * and emphasis on ongoing change/gradual progression.
 *
 * Structure:
 * - Verb stem (masu form minus ます) + つつ + ある/あります
 * - Can be past tense: つつあった
 *
 * Examples:
 * - 物価が上がりつつある (Prices are gradually rising)
 * - 病気が治りつつあります (My illness is in the process of healing)
 * - 契約しつつあったのに決裂した (Despite having been in the process of signing...)
 * - 野生動物の数が減りつつある (The number of wild animals is decreasing)
 *
 * Key discriminators:
 * - Formal/literary register (vs ている which is neutral)
 * - Emphasizes gradual ongoing change (vs ている which can be simple progressive)
 * - Often used with verbs showing change (増える, 減る, 上がる, 治る, etc.)
 * - つつ is SCONJ or AUX with lemma='つつ'
 * - ある is VERB with lemma='ある' (or AUX in some parses)
 *
 * GiNZA parse structure:
 * - Verb stem (連用形) with inflectionForm=連用形-一般
 * - つつ as SCONJ/AUX with pos='SCONJ' or pos='AUX', lemma='つつ'
 * - ある as VERB/AUX with lemma='ある'
 *
 * Different from:
 * - ている (te-iru) - neutral progressive, less formal
 * - ていく (te-iku) - "start to" or "continue to"
 * - てくる (te-kuru) - "become" or "change over time"
 * - つづける (tsuzukeru) - "continue to" (suru-verb)
 * - つつも (tsutsu mo) - "even while" (concessive)
 */
export default linguisticRule('つつある', (r) => {
  r.either(
    // Pattern 1: Standard form - Verb stem + つつ + ある
    // e.g., 上がりつつある, 減りつつある, 治りつつある
    // GiNZA: verb(連用形) + つつ(SCONJ/AUX) + ある(VERB)
    (b1) => {
      const verb = b1.verb({ inflectionForm: '連用形-一般' }, 'verb');
      const tsutsu = b1.tok({
        text: 'つつ',
        lemma: 'つつ',
        posOneOf: ['SCONJ', 'AUX', 'PART']
      }, 'tsutsu');
      const aru = b1.verb({ lemma: 'ある' }, 'aru');

      b1.inOrder(verb, tsutsu, 1);
      b1.inOrder(tsutsu, aru, 1);
      b1.captureSpan('つつある', verb, aru);
    },

    // Pattern 2: Polite form - Verb stem + つつ + あります
    // e.g., 治りつつあります, 落ち着きつつあります
    // GiNZA: verb(連用形) + つつ(SCONJ/AUX) + あります(AUX/VERB)
    (b2) => {
      const verb = b2.verb({ inflectionForm: '連用形-一般' }, 'verb');
      const tsutsu = b2.tok({
        text: 'つつ',
        lemma: 'つつ',
        posOneOf: ['SCONJ', 'AUX', 'PART']
      }, 'tsutsu');
      const arimasu = b2.tok({
        text: 'あります',
        lemma: 'ある',
        posOneOf: ['AUX', 'VERB']
      }, 'arimasu');

      b2.inOrder(verb, tsutsu, 1);
      b2.inOrder(tsutsu, arimasu, 1);
      b2.captureSpan('つつある', verb, arimasu);
    },

    // Pattern 3: Past tense - Verb stem + つつ + あった
    // e.g., しつつあった, 上がりつつあった
    // GiNZA: verb(連用形) + つつ(SCONJ/AUX) + あった(AUX/VERB)
    (b3) => {
      const verb = b3.verb({ inflectionForm: '連用形-一般' }, 'verb');
      const tsutsu = b3.tok({
        text: 'つつ',
        lemma: 'つつ',
        posOneOf: ['SCONJ', 'AUX', 'PART']
      }, 'tsutsu');
      const atta = b3.tok({
        text: 'あった',
        lemma: 'ある',
        posOneOf: ['AUX', 'VERB']
      }, 'atta');

      b3.inOrder(verb, tsutsu, 1);
      b3.inOrder(tsutsu, atta, 1);
      b3.captureSpan('つつある', verb, atta);
    },

    // Pattern 4: More flexible verb matching (handles various inflection forms)
    // Some verbs may not have the inflectionForm consistently marked
    (b4) => {
      const verb = b4.verb({}, 'verb');
      const tsutsu = b4.tok({
        text: 'つつ',
        lemma: 'つつ',
        posOneOf: ['SCONJ', 'AUX', 'PART']
      }, 'tsutsu');
      const aru = b4.tok({ lemma: 'ある' }, 'aru');

      b4.inOrder(verb, tsutsu, 2);
      b4.inOrder(tsutsu, aru, 2);
      b4.captureSpan('つつある', verb, aru);
    },

    // Pattern 5: Combined token variant (sometimes parsed as one token)
    // Handle cases where つつある might be tokenized differently
    (b5) => {
      const verb = b5.verb({ inflectionForm: '連用形-一般' }, 'verb');
      const tsutsuAru = b5.tok({
        text: 'つつある',
        posOneOf: ['AUX', 'SCONJ', 'VERB']
      }, 'tsutsuAru');

      b5.inOrder(verb, tsutsuAru, 1);
      b5.captureSpan('つつある', verb, tsutsuAru);
    },

    // Pattern 6: Very flexible matching - any verb + つつ + ある-variant
    // Catch-all for various GiNZA parsing patterns
    (b6) => {
      const verb = b6.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const tsutsu = b6.tok({
        text: 'つつ',
        posOneOf: ['SCONJ', 'AUX', 'PART']
      }, 'tsutsu');
      const aru = b6.tok({ lemma: 'ある' }, 'aru');

      b6.inOrder(verb, tsutsu, 3);
      b6.inOrder(tsutsu, aru, 3);
      b6.captureSpan('つつある', verb, aru);
    }
  );
});
