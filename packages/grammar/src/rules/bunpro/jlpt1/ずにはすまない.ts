import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: ずにはすまない (zu ni wa sumanai) - Cannot avoid doing; must do
 *
 * A formal/literary expression indicating that one cannot get away with not doing (A).
 * It means "will not come to an end with (A) not being done" or "cannot avoid doing (A)".
 * This is an old-fashioned expression - consider using ざるをえない or ないわけにはいかない instead.
 *
 * Structures:
 * - Verb［stem removing ない］+ ずにはすまない
 * - Verb［stem removing ない］+ ずではすまない
 * - Verb［ない form］+ ではすまない
 * - Exception: する → せずにはすまない
 *
 * Examples:
 * - 謝らずにはすまないだろう。
 *   (I won't get away without apologizing.)
 * - 会社の先輩からの電話は出ずにはすまない。
 *   (I have no choice but to answer phone calls from my senpai.)
 * - 税金は払わないではすまない。
 *   (You can't get away without paying taxes.)
 * - 大人なのだから、しらずではすまないだろう。
 *   (Since you are an adult, you won't get by without knowing it.)
 *
 * Key discriminators:
 * - ず is classical negative auxiliary (old form of ない)
 * - には/では is particle combination
 * - すまない is すむ (verb: to finish/end) + ない (auxiliary)
 * - Must capture from verb stem to すまない
 * - Three variants: ずには, ずでは, では
 *
 * GiNZA parsing notes:
 * - ず is parsed as AUX with lemma=ぬ (classical negative)
 * - すまない is parsed as: すむ (VERB, lemma=すむ) + ない (AUX, lemma=ない)
 */
export default linguisticRule('ずにはすまない', (r) => {
  r.either(
    // Variant 1: Verb stem + ずには + すまない
    // 謝らずにはすまない, 行かずにはすまない, etc.
    (b) => {
      // ず is the classical negative form (attached to verb stem)
      const zu = b.tok({ text: 'ず', pos: 'AUX' }, 'zu');

      // Particle combination には
      const ni = b.tok({ text: 'に', pos: 'ADP' }, 'ni');
      const wa = b.tok({ text: 'は', pos: 'ADP' }, 'wa');

      // すまない is すむ (verb: to finish) + ない (auxiliary)
      const sumu = b.verb({ lemma: 'すむ' }, 'sumu');
      const nai = b.aux({ lemma: 'ない' }, 'nai');

      // Order constraints: ず → に → は → すむ → ない (consecutive)
      b.inOrder(zu, ni, 1).inOrder(ni, wa, 1).inOrder(wa, sumu, 1).inOrder(sumu, nai, 1);

      // Capture from verb stem (which precedes ず) to ない
      // Since ず is attached to verb stem, we capture from ず's head to ない
      b.captureSpan('ずにはすまない', zu, nai);
    },

    // Variant 2: Verb stem + ずでは + すまない
    // しらずではすまない, etc.
    (b) => {
      // ず is the classical negative form (attached to verb stem)
      const zu = b.tok({ text: 'ず', pos: 'AUX' }, 'zu');

      // Particle combination では
      const de = b.tok({ text: 'で', posOneOf: ['AUX', 'ADP'] }, 'de');
      const wa = b.tok({ text: 'は', pos: 'ADP' }, 'wa');

      // すまない is すむ (verb: to finish) + ない (auxiliary)
      const sumu = b.verb({ lemma: 'すむ' }, 'sumu');
      const nai = b.aux({ lemma: 'ない' }, 'nai');

      // Order constraints: ず → で → は → すむ → ない (consecutive)
      b.inOrder(zu, de, 1).inOrder(de, wa, 1).inOrder(wa, sumu, 1).inOrder(sumu, nai, 1);

      // Capture from verb stem (which precedes ず) to ない
      b.captureSpan('ずにはすまない', zu, nai);
    },

    // Variant 3: Verb［ない form］+ では + すまない
    // 謝らないではすまない, 行かないではすまない, etc.
    (b) => {
      // Find the nai auxiliary (negative form of verb)
      const verbNai = b.aux({ lemma: 'ない' }, 'verbNai');

      // Particle combination では
      const de = b.tok({ text: 'で', posOneOf: ['AUX', 'ADP'] }, 'de');
      const wa = b.tok({ text: 'は', pos: 'ADP' }, 'wa');

      // すまない is すむ (verb: to finish) + ない (auxiliary)
      const sumu = b.verb({ lemma: 'すむ' }, 'sumu');
      const sumanaiNai = b.aux({ lemma: 'ない' }, 'sumanaiNai');

      // Order constraints: verb-ない → で → は → すむ → ない (consecutive)
      b.inOrder(verbNai, de, 1).inOrder(de, wa, 1).inOrder(wa, sumu, 1).inOrder(sumu, sumanaiNai, 1);

      // Capture from verb stem (which precedes verb-ない) to sumanai-ない
      b.captureSpan('ずにはすまない', verbNai, sumanaiNai);
    }
  );
});
