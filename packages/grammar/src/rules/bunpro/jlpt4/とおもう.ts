import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: とおもう - I think that...
 *
 * Matches quotation + 思う (to think) expressing thoughts or opinions.
 *
 * This pattern is used to express one's thoughts, opinions, or conjecture.
 * The particle と marks the content of what is being thought (like quotation marks).
 *
 * Forms:
 * - とおもう (casual, present)
 * - とおもった (casual, past)
 * - とおもいます (polite, present)
 * - とおもいました (polite, past)
 * - とおもっている (continuous/state "is thinking")
 * - とおっています (polite continuous)
 *
 * Examples:
 * - 明日は雨が降ると思う (I think it will rain tomorrow)
 * - これはいいと思います (I think this is good)
 * - 彼は賢いと思っている (He thinks he is smart)
 *
 * This is different from:
 * - Direct quotation と (marked by brackets 「...」と)
 * - Conditional と (when/if)
 * - Accompaniment と (with)
 *
 * GiNZA parse structure:
 * - と (particle, dep=case) marks the quoted content
 * - 思う/おもう (verb) is the thinking verb - GiNZA may use kanji or hiragana
 * - Various auxiliaries for tense/politeness:
 *   - た (past tense auxiliary)
 *   - ます (polite auxiliary)
 *   - いる (te-form auxiliary for continuous/state)
 */
export default linguisticRule('とおもう', (r) => {
  r.either(
    // Branch 1: Standard quotation と (pos=ADP, dep=case) - preferred pattern
    (b) => {
      const to = b.particle('と', 'to', { pos: 'ADP', dep: 'case' });
      b.either(
        // Pattern 1a: とおもう (casual, present)
        (b) => {
          const omou = b.verb({ textOneOf: ['思う', 'おもう'] }, 'omou');
          b.inOrder(to, omou, 3);
          b.captureSpan('とおもう', to, omou);
        },
        // Pattern 1b: とおもった (casual, past)
        (b) => {
          const omou = b.verb({ textOneOf: ['思っ', 'おもっ'] }, 'omou');
          const ta = b.aux({ lemma: 'た' }, 'ta');
          b.inOrder(to, omou, 3);
          b.auxOf(omou, ta);
          b.captureSpan('とおもう', to, ta);
        },
        // Pattern 1c: とおもいます (polite, present)
        (b) => {
          const omoi = b.tok({ textOneOf: ['思', 'おも', '思い', 'おもい'] }, 'omoi');
          const masu = b.aux({ lemma: 'ます' }, 'masu');
          b.inOrder(to, omoi, 2);
          b.inOrder(omoi, masu, 1);
          b.captureSpan('とおもう', to, masu);
        },
        // Pattern 1d: とおもいました (polite, past)
        (b) => {
          const omomashi = b.tok({ textOneOf: ['思い', 'おもい', '思', 'おも'] }, 'omomashi');
          const mashita = b.aux({ lemma: 'ました' }, 'mashita');
          b.inOrder(to, omomashi, 2);
          b.inOrder(omomashi, mashita, 1);
          b.captureSpan('とおもう', to, mashita);
        },
        // Pattern 1e: とおもっている (continuous/state)
        (b) => {
          const omou = b.verb({ textOneOf: ['思っ', 'おもっ'] }, 'omou');
          const te = b.aux({ text: 'て' }, 'te');
          const iru = b.aux({ lemma: 'いる' }, 'iru');
          b.inOrder(to, omou, 3);
          b.auxOf(omou, te);
          b.auxOf(te, iru);
          b.captureSpan('とおもう', to, iru);
        },
        // Pattern 1f: とおっています (polite continuous)
        (b) => {
          const omou = b.verb({ textOneOf: ['思っ', 'おもっ'] }, 'omou');
          const te = b.aux({ text: 'て' }, 'te');
          const imasu = b.aux({ lemma: 'います' }, 'imasu');
          b.inOrder(to, omou, 3);
          b.auxOf(omou, te);
          b.auxOf(te, imasu);
          b.captureSpan('とおもう', to, imasu);
        }
      );
    },

    // Branch 2: Flexible と (for edge cases where GiNZA parses differently)
    (b) => {
      const to = b.particle('と', 'to'); // No pos/dep constraints
      b.either(
        // Pattern 2a: とおもう (casual, present)
        (b) => {
          const omou = b.tok({ textOneOf: ['思う', 'おもう'] }, 'omou');
          b.inOrder(to, omou, 3);
          b.captureSpan('とおもう', to, omou);
        },
        // Pattern 2b: とおもった (casual, past)
        (b) => {
          const omou = b.verb({ textOneOf: ['思っ', 'おもっ'] }, 'omou');
          const ta = b.aux({ lemma: 'た' }, 'ta');
          b.inOrder(to, omou, 3);
          b.auxOf(omou, ta);
          b.captureSpan('とおもう', to, ta);
        },
        // Pattern 2c: とおもいます (polite, present)
        (b) => {
          const omoi = b.tok({ textOneOf: ['思', 'おも', '思い', 'おもい'] }, 'omoi');
          const masu = b.aux({ lemma: 'ます' }, 'masu');
          b.inOrder(to, omoi, 2);
          b.inOrder(omoi, masu, 1);
          b.captureSpan('とおもう', to, masu);
        },
        // Pattern 2d: とおもいました (polite, past)
        (b) => {
          const omomashi = b.tok({ textOneOf: ['思い', 'おもい', '思', 'おも'] }, 'omomashi');
          const mashita = b.aux({ lemma: 'ました' }, 'mashita');
          b.inOrder(to, omomashi, 2);
          b.inOrder(omomashi, mashita, 1);
          b.captureSpan('とおもう', to, mashita);
        },
        // Pattern 2e: とおもっている (continuous/state)
        (b) => {
          const omou = b.verb({ textOneOf: ['思っ', 'おもっ'] }, 'omou');
          const te = b.aux({ text: 'て' }, 'te');
          const iru = b.aux({ lemma: 'いる' }, 'iru');
          b.inOrder(to, omou, 3);
          b.auxOf(omou, te);
          b.auxOf(te, iru);
          b.captureSpan('とおもう', to, iru);
        },
        // Pattern 2f: とおっています (polite continuous)
        (b) => {
          const omou = b.verb({ textOneOf: ['思っ', 'おもっ'] }, 'omou');
          const te = b.aux({ text: 'て' }, 'te');
          const imasu = b.aux({ lemma: 'います' }, 'imasu');
          b.inOrder(to, omou, 3);
          b.auxOf(omou, te);
          b.auxOf(te, imasu);
          b.captureSpan('とおもう', to, imasu);
        }
      );
    }
  );
});
