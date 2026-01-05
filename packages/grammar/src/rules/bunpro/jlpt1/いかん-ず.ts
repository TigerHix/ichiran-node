import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: いかん〜ず (ikan~zu) - Regardless of, Irrespective of
 *
 * A formal written expression meaning "regardless of (A), (B)" or
 * "irrespective of (A), (B)". Combines the noun いかん (meaning "how" or
 * "in what way") with a classical negative auxiliary verb ず.
 *
 * The verbs used with this construction are restricted to three patterns:
 * - にかかわらず (without relation to)
 * - によらず (without dependence on)
 * - をとわず/を問わず (without questioning)
 *
 * Structure:
 * - Noun + (の) + いかん + に + かかわらず/よらず
 * - Noun + (の) + いかん + を + とわず/問わず
 *
 * The particle の is optional between the noun and いかん.
 *
 * Examples:
 * - 結果のいかんにかかわらず、努力をした生徒を褒める
 *   (Regardless of the result, praise students who made effort)
 * - 理由のいかんによらず、キャンセルできます
 *   (Regardless of the reason, you can cancel)
 * - 天候のいかんを問わず、開催されます
 *   (Irrespective of the weather, it will be held)
 *
 * Key discriminators:
 * - いかん is a NOUN meaning "how/manner"
 * - Followed by particle に or を
 * - Ends with かかわらず/よらず/とわず/問わず (verb forms)
 * - Formal written register
 */
export default linguisticRule('いかん-ず', (r) => {
  r.either(
    // Pattern 1: Noun + の + いかん + に + かかわら + ず (AUX)
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const no = b1.particle('の', 'no');
      const ikan = b1.noun({ lemma: 'いかん' }, 'ikan');
      const ni = b1.particle('に', 'ni');
      const kakawara = b1.tok({ lemmaOneOf: ['かかわる', '関わる'] }, 'kakawara');
      const zu = b1.aux({ lemma: 'ず' }, 'zu');

      b1.inOrder(noun, no, 1).inOrder(no, ikan, 1);
      b1.inOrder(ikan, ni, 1).inOrder(ni, kakawara, 1);
      b1.auxOf(kakawara, zu);
      b1.captureSpan('いかん-ず', noun, zu);
    },

    // Pattern 1b: Noun + の + いかん + に + かかわら + ず (SCONJ, fixed dep)
    (b1b) => {
      const noun = b1b.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const no = b1b.particle('の', 'no');
      const ikan = b1b.noun({ lemma: 'いかん' }, 'ikan');
      const ni = b1b.particle('に', 'ni');
      const kakawara = b1b.tok({ lemmaOneOf: ['かかわる', '関わる'], pos: 'SCONJ' }, 'kakawara');
      const zu = b1b.tok({ lemma: 'ず', pos: 'SCONJ' }, 'zu');

      b1b.inOrder(noun, no, 1).inOrder(no, ikan, 1);
      b1b.inOrder(ikan, ni, 1).inOrder(ni, kakawara, 1);
      b1b.inOrder(kakawara, zu, 1);
      b1b.captureSpan('いかん-ず', noun, zu);
    },

    // Pattern 2: Noun + の + いかん + に + よら + ず
    (b2) => {
      const noun = b2.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const no = b2.particle('の', 'no');
      const ikan = b2.noun({ lemma: 'いかん' }, 'ikan');
      const ni = b2.particle('に', 'ni');
      const yora = b2.verb({ lemma: 'よる' }, 'yora');
      const zu = b2.aux({ lemma: 'ず' }, 'zu');

      b2.inOrder(noun, no, 1).inOrder(no, ikan, 1);
      b2.inOrder(ikan, ni, 1).inOrder(ni, yora, 1);
      b2.auxOf(yora, zu);
      b2.captureSpan('いかん-ず', noun, zu);
    },

    // Pattern 3: Noun + の + いかん + を + とわ + ず
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const no = b3.particle('の', 'no');
      const ikan = b3.noun({ lemma: 'いかん' }, 'ikan');
      const wo = b3.particle('を', 'wo');
      const towa = b3.verb({ lemma: 'とう' }, 'towa');
      const zu = b3.aux({ lemma: 'ず' }, 'zu');

      b3.inOrder(noun, no, 1).inOrder(no, ikan, 1);
      b3.inOrder(ikan, wo, 1).inOrder(wo, towa, 1);
      b3.auxOf(towa, zu);
      b3.captureSpan('いかん-ず', noun, zu);
    },

    // Pattern 4: Noun + の + いかん + を + 問わ + ず
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const no = b4.particle('の', 'no');
      const ikan = b4.noun({ lemma: 'いかん' }, 'ikan');
      const wo = b4.particle('を', 'wo');
      const towa = b4.verb({ lemma: '問う' }, 'towa');
      const zu = b4.aux({ lemma: 'ず' }, 'zu');

      b4.inOrder(noun, no, 1).inOrder(no, ikan, 1);
      b4.inOrder(ikan, wo, 1).inOrder(wo, towa, 1);
      b4.auxOf(towa, zu);
      b4.captureSpan('いかん-ず', noun, zu);
    },

    // Pattern 5: Noun + いかん + に + かかわら + ず (without の, AUX)
    (b5) => {
      const noun = b5.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const ikan = b5.noun({ lemma: 'いかん' }, 'ikan');
      const ni = b5.particle('に', 'ni');
      const kakawara = b5.tok({ lemmaOneOf: ['かかわる', '関わる'] }, 'kakawara');
      const zu = b5.aux({ lemma: 'ず' }, 'zu');

      b5.inOrder(noun, ikan, 1).inOrder(ikan, ni, 1).inOrder(ni, kakawara, 1);
      b5.auxOf(kakawara, zu);
      b5.captureSpan('いかん-ず', noun, zu);
    },

    // Pattern 5b: Noun + いかん + に + かかわら + ず (without の, SCONJ, fixed dep)
    (b5b) => {
      const noun = b5b.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const ikan = b5b.noun({ lemma: 'いかん' }, 'ikan');
      const ni = b5b.particle('に', 'ni');
      const kakawara = b5b.tok({ lemmaOneOf: ['かかわる', '関わる'], pos: 'SCONJ' }, 'kakawara');
      const zu = b5b.tok({ lemma: 'ず', pos: 'SCONJ' }, 'zu');

      b5b.inOrder(noun, ikan, 1).inOrder(ikan, ni, 1).inOrder(ni, kakawara, 1);
      b5b.inOrder(kakawara, zu, 1);
      b5b.captureSpan('いかん-ず', noun, zu);
    },

    // Pattern 6: Noun + いかん + に + よら + ず (without の)
    (b6) => {
      const noun = b6.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const ikan = b6.noun({ lemma: 'いかん' }, 'ikan');
      const ni = b6.particle('に', 'ni');
      const yora = b6.verb({ lemma: 'よる' }, 'yora');
      const zu = b6.aux({ lemma: 'ず' }, 'zu');

      b6.inOrder(noun, ikan, 1).inOrder(ikan, ni, 1).inOrder(ni, yora, 1);
      b6.auxOf(yora, zu);
      b6.captureSpan('いかん-ず', noun, zu);
    },

    // Pattern 7: Noun + いかん + を + とわ + ず (without の)
    (b7) => {
      const noun = b7.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const ikan = b7.noun({ lemma: 'いかん' }, 'ikan');
      const wo = b7.particle('を', 'wo');
      const towa = b7.verb({ lemma: 'とう' }, 'towa');
      const zu = b7.aux({ lemma: 'ず' }, 'zu');

      b7.inOrder(noun, ikan, 1).inOrder(ikan, wo, 1).inOrder(wo, towa, 1);
      b7.auxOf(towa, zu);
      b7.captureSpan('いかん-ず', noun, zu);
    },

    // Pattern 8: Noun + いかん + を + 問わ + ず (without の)
    (b8) => {
      const noun = b8.tok({ posOneOf: ['NOUN', 'ADV', 'PROPN', 'PRON'] }, 'noun');
      const ikan = b8.noun({ lemma: 'いかん' }, 'ikan');
      const wo = b8.particle('を', 'wo');
      const towa = b8.verb({ lemma: '問う' }, 'towa');
      const zu = b8.aux({ lemma: 'ず' }, 'zu');

      b8.inOrder(noun, ikan, 1).inOrder(ikan, wo, 1).inOrder(wo, towa, 1);
      b8.auxOf(towa, zu);
      b8.captureSpan('いかん-ず', noun, zu);
    }
  );
});
