import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ということは (to iu koto wa) - "that means, that is to say"
 *
 * An expression used to clarify or restate what has been said before. It marks a
 * quoted phrase as a topic for further explanation or draws a logical conclusion.
 * Translates as "that means", "that is to say", or "so what you're saying is".
 *
 * Structure:
 * - Phrase/Sentence + という + こと + は
 *
 * The pattern consists of:
 * - と (to) - quote particle marking the preceding phrase
 * - いう (iu) - verb "to say" (often in citation form)
 * - こと (koto) - noun "thing/matter/abstract concept"
 * - は (wa) - topic marker
 *
 * Examples:
 * - 日本語を上手に話せるということは、日本に長い間住んでいたということですか。
 *   (You can speak Japanese well. Does that mean that you have lived in Japan for a long time?)
 * - 彼女から返事が来ないということは、今は忙しいということだろう。
 *   (The fact that she hasn't responded to me means that she is probably busy at the moment.)
 * - もう仕事終わったの？ということは、今日こそは一緒に晩御飯が食べれるということだね！
 *   (You are already done with work!? That means that we can finally eat dinner together today!)
 * - 末っ子ということは、本当は甘えん坊なんじゃない？
 *   (Being the youngest child means you're really spoiled, isn't that right?)
 * - 今日は祝日です。ということは、あのスーパーは閉まっていますか？
 *   (Today is a national holiday. Does that mean that supermarket is closed?)
 *
 * Key discriminators:
 * - Used to interpret, clarify, or draw conclusions from previous statements
 * - Can appear mid-sentence (continuing thought) or at sentence start (new thought)
 * - Often followed by explanatory clause or question
 * - Different from simple ということ (JLPT4) which just nominalizes
 * - Different from というのは (JLPT3) which introduces definitions
 *
 * GiNZA parse structure:
 * - 活かせる(VERB) + という(ADP/FIXED) + こと(NOUN) + は(PART/ADP)
 * - 仕事終わったの？ + という(ADP/FIXED) + こと(NOUN) + は(PART/ADP)
 *
 * Different from:
 * - ということ (JLPT4) - just nominalization "the fact that" (topic marker は is key)
 * - というのは (JLPT3) - introduces definitions or reasons ("what is called X is Y")
 * - ことだ (JLPT3) - giving advice "you should"
 * - 単なる こと (mere thing/matter) - no quotation/intent
 */
export default bunproLinguisticRule('ということは', (r) => {
  // Quote particle と (marks the quoted phrase)
  const to = r.particle('と', 'to');

  // という is often tokenized as a fixed adposition or as separate tokens
  r.either(
    // Pattern 1: と + いう + こと + は (separate tokens)
    (b1) => {
      const iu = b1.verb({ lemma: 'いう' }, 'iu');
      const koto = b1.noun({ lemma: 'こと' }, 'koto');
      const wa = b1.particle('は', 'wa');

      b1.inOrder(to, iu, 5);
      b1.inOrder(iu, koto, 2);
      b1.inOrder(koto, wa, 1);

      b1.captureSpan('ということは', to, wa);
    },

    // Pattern 2: という (fixed phrase) + こと + は
    (b2) => {
      const toiu = b2.tok({ text: 'という', posOneOf: ['ADP', 'SYM'] }, 'toiu');
      const koto = b2.noun({ lemma: 'こと' }, 'koto');
      const wa = b2.particle('は', 'wa');

      b2.inOrder(toiu, koto, 1);
      b2.inOrder(koto, wa, 1);

      b2.captureSpan('ということは', toiu, wa);
    }
  );
});
