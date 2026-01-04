import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: といえば - speaking of / now that you mention
 *
 * Matches noun + と (quotational) + いう (verb) + conditional ending
 *
 * This pattern is used when a topic reminds the speaker of something related,
 * effectively shifting to a related topic. It's a topic-changer that references
 * something previously mentioned or known to both parties.
 *
 * Structure:
 * - Noun + といえば - "speaking of N..."
 * - Noun + というと - "if it's N..." / "when it comes to N..."
 * - Noun + といったら - "when it's N..." / "speaking of N..."
 *
 * Examples:
 * - コーヒーといえば、新しい喫茶店が開店した。
 *   (Speaking of coffee, a new coffee shop just opened.)
 * - 夏といえばかき氷だよね。
 *   (Speaking of summer, it's shaved ice, right?)
 * - アニメといえばドラグーンボールの新しいシーズンが放送されているね。
 *   (Speaking of anime, a new season of Dragon Ball has aired.)
 *
 * This is different from:
 * - そういえば (JLPT2) - "come to think of it" / "now that you mention it"
 * - という - "called" / "known as" (defining/naming)
 * - と言っても (JLPT3) - "even if I say" / "although I say"
 *
 * GiNZA parse structure (IMPORTANT):
 * - といえば: と(particle) + いえ(verb, lemma=いう, text=いえ) + ば(particle)
 *   Note: GiNZA tokenizes "いえば" as "いえ" (verb stem) + "ば" (conditional particle)
 * - というと: と(particle) + いう(verb) + と(particle)
 * - といったら: と(particle) + いったら(verb, lemma=いう) or いう(verb) + たら(aux)
 *
 * The key insight: For "といえば", GiNZA parses "いえ" as a verb form with lemma="いう",
 * not the full "いう". This is the 仮定形 (conditional form) of the verb いう.
 */
export default linguisticRule('といえば', (r) => {
  r.either(
    // Pattern 1: といえば (speaking of / if we speak of)
    // GiNZA parses this as: と + いえ(verb, lemma=いう) + ば
    // The verb "いえ" is the conditional stem form of いう
    (b) => {
      const to = b.particle('と', 'to');
      const ie = b.verb({
        lemma: 'いう',
        textOneOf: ['いえ', 'いい', 'えば'],  // GiNZA may use different tokenizations
      }, 'verbConditional');
      const ba = b.tok({
        text: 'ば',
      }, 'conditional');

      b.inOrder(to, ie, 1);
      b.inOrder(ie, ba, 1);
      b.captureSpan('といえば', to, ba);
    },

    // Pattern 2: というと (if it's / when it comes to)
    // GiNZA parses this as: と + いう + と
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう', text: 'いう' }, 'iu');
      const to2 = b.particle('と', 'to2');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, to2, 1);
      b.captureSpan('といえば', to, to2);
    },

    // Pattern 3: といったら (when it's / speaking of)
    // GiNZA may parse this as: と + いったら or と + いう + たら
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const tara = b.tok({
        textOneOf: ['ったら', 'たら'],
      }, 'conditional');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, tara, 1);
      b.captureSpan('といえば', to, tara);
    }
  );
});
