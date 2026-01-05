import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: んだけど・んですが (but/although with explanatory tone)
 *
 * Matches explanatory ん/の + conjunction (だけど/ですが) to express contrast
 * with an explanatory or softening tone.
 *
 * Structures:
 * - Verb/i-adj + ん/の + だ/です + が/けど (explanatory but)
 * - Na-adj/noun + な + ん/の + だ/です + が/けど
 *
 * Examples:
 * - 行きたいんだけど (I want to go, but...)
 * - 買おうと思っているのですが (I'm thinking of buying, but...)
 * - 顔はいいのだけど (The face is good, but...)
 * - 窓が開いているからちょっと寒いんですが (Since the window is open it's a bit cold, but...)
 *
 * Key discriminators:
 * - ん/の is the explanatory nominalizer (SCONJ, dep=mark or AUX, dep=aux)
 * - だ/です is the copula (AUX, lemma=だ)
 * - が/けど is the conjunction particle (SCONJ or PART)
 *
 * GiNZA parse structure:
 * - POSITIVE: 行きたいんだけど
 *   - 行く(VERB) + たい(AUX) + ん(SCONJ, dep=mark) + だ(AUX, lemma=だ) + けど(PART)
 * - POSITIVE: 顔はいいのだけど
 *   - 顔(NOUN) + は(PART) + いい(ADJ) + の(SCONJ, dep=mark) + だ(AUX) + けど(PART)
 * - POSITIVE: 買おうと思っているのですが
 *   - 買う(VERB) + おう(AUX) + と思っている + の(SCONJ, dep=mark) + です(AUX) + が(SCONJ)
 */
export default bunproLinguisticRule('んだけど-んですが', (r) => {
  r.either(
    // Pattern 1: ん + だけど (casual form with ん)
    (b) => {
      const n = b.tok({ text: 'ん', posOneOf: ['SCONJ', 'AUX'] }, 'n');
      const da = b.aux({ lemma: 'だ' }, 'da');
      const kedo = b.particle('けど', 'kedo');

      b.inOrder(n, da, 1);
      b.inOrder(da, kedo, 1);
      b.captureSpan('んだけど', n, kedo);
    },

    // Pattern 2: の + だけど (slightly more formal with の)
    (b) => {
      const no = b.tok({ text: 'の', posOneOf: ['SCONJ', 'AUX'] }, 'no');
      const da = b.aux({ lemma: 'だ' }, 'da');
      const kedo = b.particle('けど', 'kedo');

      b.inOrder(no, da, 1);
      b.inOrder(da, kedo, 1);
      b.captureSpan('のだけど', no, kedo);
    },

    // Pattern 3: ん + ですが (polite form with ん)
    (b) => {
      const n = b.tok({ text: 'ん', posOneOf: ['SCONJ', 'AUX'] }, 'n');
      const desu = b.tok({ text: 'です' }, 'desu');
      const ga = b.particle('が', 'ga', { pos: 'SCONJ' });

      b.inOrder(n, desu, 1);
      b.inOrder(desu, ga, 1);
      b.captureSpan('んですが', n, ga);
    },

    // Pattern 4: の + ですが (polite form with の)
    (b) => {
      const no = b.tok({ text: 'の', posOneOf: ['SCONJ', 'AUX'] }, 'no');
      const desu = b.tok({ text: 'です' }, 'desu');
      const ga = b.particle('が', 'ga', { pos: 'SCONJ' });

      b.inOrder(no, desu, 1);
      b.inOrder(desu, ga, 1);
      b.captureSpan('のですが', no, ga);
    },

    // Pattern 5: ん + だが (plain form with だが)
    (b) => {
      const n = b.tok({ text: 'ん', posOneOf: ['SCONJ', 'AUX'] }, 'n');
      const da = b.tok({ text: 'だ' }, 'da');
      const ga = b.particle('が', 'ga', { pos: 'SCONJ' });

      b.inOrder(n, da, 1);
      b.inOrder(da, ga, 1);
      b.captureSpan('んだが', n, ga);
    },

    // Pattern 6: の + だが (plain form with のだが)
    (b) => {
      const no = b.tok({ text: 'の', posOneOf: ['SCONJ', 'AUX'] }, 'no');
      const da = b.tok({ text: 'だ' }, 'da');
      const ga = b.particle('が', 'ga', { pos: 'SCONJ' });

      b.inOrder(no, da, 1);
      b.inOrder(da, ga, 1);
      b.captureSpan('のだが', no, ga);
    }
  );
});
