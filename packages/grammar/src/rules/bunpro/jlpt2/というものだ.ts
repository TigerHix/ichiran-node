import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: というものだ (to iu mono da) - "It is that..., it's a matter of..."
 *
 * A strong expression used to draw conclusions about or criticize the nature of
 * something. Translates as "my impression of (A) is that", "(A) is just a thing that",
 * or "that is what (A) is all about".
 *
 * The pattern combines:
 * - と (to): quoting particle
 * - いう (iu): verb "say" (often dep=fixed in という compounds)
 * - もの (mono): noun "thing"
 * - だ (da): copula "is"
 *
 * Structure:
 * - Phrase/Clause + と + いう + ものだ (casual)
 * - Phrase/Clause + と + いう + ものです (polite)
 * - Phrase/Clause + って + ものだ (colloquial, contraction of という)
 *
 * Examples:
 * - その言い分はわがままというものだ。
 *   (My impression is that your complaint is selfish.)
 * - 嘘はつかない。それがジャーナリストというものだ。
 *   (Not telling lies. That is my impression of what a journalist is.)
 * - すぐには忘れられないのが失恋というものだ。
 *   (Being unable to instantly forget is what a broken heart is.)
 * - 何があってもお互い助け合うのが友達というものだ。
 *   (Helping each other no matter what is just what friends do.)
 * - それがマナーというものだ。
 *   (That is what manners are all about.)
 *
 * Key discriminators:
 * - Must have と + いう + もの + だ/です (or colloquial って + もの + だ)
 * - The いう in という has dep=fixed (fixed expression)
 * - Often follows nominalized clauses (の, が) or nouns
 * - Expresses speaker's strong impression or definition
 * - Different from simple という (quoting) without ものだ
 * - Different from ものだ alone (general truths)
 * - Different from というわけだ (conclusion/reasoning)
 *
 * GiNZA parse structure:
 * - Noun/Clause + と(ADP,case) + いう(VERB,fixed) + もの(NOUN) + だ(AUX)
 * - Noun/Clause + と(ADP,case) + いう(VERB,fixed) + もの(NOUN) + です(AUX)
 * - Colloquial: Noun/Clause + って(PART) + もの(NOUN) + だ(AUX)
 *
 * Note: GiNZA parses という as TWO tokens: と (ADP) + いう (VERB, dep=fixed)
 * Note: One test case uses ってものだ (colloquial variant where という → って)
 */
export default bunproLinguisticRule('というものだ', (r) => {
  r.either(
    // Branch 1: と + いう + ものだ (casual, most common pattern)
    // GiNZA parses という as two tokens: と(ADP) + いう(VERB,dep=fixed)
    // e.g., わがままというものだ
    (b1) => {
      const to = b1.particle('と', 'to');

      const iu = b1.verb({
        lemma: 'いう',
        dep: 'fixed',
      }, 'iu');

      b1.inOrder(to, iu, 1);

      const mono = b1.noun({
        text: 'もの',
      }, 'mono');

      b1.inOrder(iu, mono, 2);

      const da = b1.aux({
        text: 'だ',
        lemma: 'だ',
      }, 'da');

      b1.auxOf(mono, da);

      b1.captureSpan('というものだ', to, da);
    },

    // Branch 2: と + いう + ものです (polite)
    // e.g., わがままというものです
    (b2) => {
      const to = b2.particle('と', 'to');

      const iu = b2.verb({
        lemma: 'いう',
        dep: 'fixed',
      }, 'iu');

      b2.inOrder(to, iu, 1);

      const mono = b2.noun({
        text: 'もの',
      }, 'mono');

      b2.inOrder(iu, mono, 2);

      const desu = b2.aux({
        text: 'です',
        lemma: 'です',
      }, 'desu');

      b2.auxOf(mono, desu);

      b2.captureSpan('というものだ', to, desu);
    },

    // Branch 3: って + ものだ (colloquial)
    // In casual speech, という contracts to って
    // e.g., 男ってものだ
    (b3) => {
      const tte = b3.particle('って', 'tte');

      const mono = b3.noun({
        text: 'もの',
      }, 'mono');

      b3.inOrder(tte, mono, 2);

      const da = b3.aux({
        text: 'だ',
        lemma: 'だ',
      }, 'da');

      b3.auxOf(mono, da);

      b3.captureSpan('というものだ', tte, da);
    },

    // Branch 4: More flexible pattern for という + もの + copula
    // Handles edge cases where dep might not be exactly 'fixed'
    (b4) => {
      const to = b4.particle('と', 'to');

      const iu = b4.verb({
        lemma: 'いう',
      }, 'iu');

      b4.inOrder(to, iu, 2);

      const mono = b4.tok({
        text: 'もの',
      }, 'mono');

      b4.inOrder(iu, mono, 3);

      const copula = b4.aux({
        textOneOf: ['だ', 'です'],
        lemmaOneOf: ['だ', 'です'],
      }, 'copula');

      b4.auxOf(mono, copula);

      b4.captureSpan('というものだ', to, copula);
    },

    // Branch 5: Most permissive - any pattern with と/って before いう/もの + copula
    // For complex edge cases
    (b5) => {
      const to = b5.tok({
        textOneOf: ['と', 'って'],
      }, 'to');

      const iu = b5.tok({
        lemma: 'いう',
      }, 'iu');

      b5.inOrder(to, iu, 3);

      const mono = b5.tok({
        text: 'もの',
      }, 'mono');

      b5.inOrder(iu, mono, 3);

      const copula = b5.tok({
        textOneOf: ['だ', 'です'],
      }, 'copula');

      b5.inOrder(mono, copula, 2);

      b5.captureSpan('というものだ', to, copula);
    }
  );
});
