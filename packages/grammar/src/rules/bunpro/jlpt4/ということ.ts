import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ということ - the thing is / it means that / nominalizer
 *
 * Matches phrase + という + こと
 *
 * This pattern nominalizes an entire phrase (turns it into a noun phrase).
 * Used to:
 * - Turn a phrase into a noun (nominalization)
 * - Clarify or restate what was said ("you mean that...")
 * - Ask about the meaning of something ("does that mean...?")
 *
 * Structure:
 * - Phrase/Sentence + と + いう + こと (formal/casual)
 * - Phrase/Sentence + って + こと (casual/spoken)
 *
 * Examples:
 * - 日本に行くということ (the fact that (he) is going to Japan)
 * - これが正しいということですか？ (Does that mean this is correct?)
 * - 病院にもう行かなくていいの？治ったということ？ (You don't have to go to the hospital anymore? Does that mean you're healed?)
 * - これは人工ってこと？ (You mean that this is artificial?)
 *
 * This is different from:
 * - ということだ (JLPT3) - hearsay/reporting "I hear that..."
 * - こと (JLPT4) - simple nominalizer without という
 * - という (JLPT3) - "called X" when followed by a noun
 * - というのは (JLPT3) - topic marker + nominalizer
 *
 * GiNZA parse structure:
 * - ということ: と(particle) + いう(verb) + こと(noun)
 * - ってこと: って(particle) + こと(noun)
 *   Note: GiNZA parses って as a particle with lemma=と
 *
 * The key constraint: という must be immediately followed by こと (not は or other particles)
 */
export default linguisticRule('ということ', (r) => {
  r.either(
    // Pattern 1: ということ (formal/casual)
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const koto = b.noun({ lemma: 'こと' }, 'koto');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, koto, 1);

      b.captureSpan('ということ', to, koto);
    },
    // Pattern 2: ってこと (casual/spoken)
    // GiNZA parses って as a token, often as particle or contraction
    (b) => {
      const tte = b.tok({ text: 'って' }, 'tte');
      const koto = b.noun({ lemma: 'こと' }, 'koto');

      b.inOrder(tte, koto, 1);

      b.captureSpan('ということ', tte, koto);
    }
  );
});
