import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: としても (toshitemo) - Even if (we assume), Assuming, Also as
 *
 * A phrase used when assuming something is true for the sake of argument,
 * or emphasizing someone's position/role. Two main usages:
 *
 * 1. "Even if we assume / Even assuming" - hypothetical concession
 *    Structure: [Noun/Verb/Adj] + としても
 *
 * 2. "Also as / Even as" - emphatic role/position
 *    Structure: Noun + としても
 *
 * Examples (hypothetical):
 * - たとえできるとしても、やらないだろう。
 *   (Even if I could do it, I probably wouldn't.)
 * - あなたが言っている事が正しいとしても、私は賛成できません。
 *   (Even assuming what you're saying is correct, I can't agree.)
 * - 大人向きの本だとしても売れます。
 *   (It will sell even as a book for adults.)
 *
 * Examples (role/position):
 * - 私としても困ります。
 *   (It will even inconvenience me / Even for me, it's troubling.)
 * - 会社としてもこういうことはしたくない。
 *   (Even as a company, we don't want to do this.)
 * - オルガン奏者としても有名です。
 *   (He's also famous as an organ player.)
 *
 * Key discriminators:
 * - と is a particle (ADP)
 * - して is te-form of する, but GiNZA tokenizes as "し" (lemma=する, pos=SCONJ)
 * - も is an emphatic particle (ADP)
 * - The phrase expresses hypothetical concession or emphatic role
 *
 * GiNZA parse structure:
 * - "して" is tokenized as TWO tokens: "し" (lemma=する, pos=SCONJ) + "て" (lemma=て, pos=SCONJ)
 * - Various dependency relations (compound, fixed, mark, obl)
 *
 * Different from:
 * - として (toshite) - "as, in the capacity of" (non-emphatic)
 * - にしても (nishitemo) - "even if" (different particle, more subjective)
 * - としたら (toshitara) - "if we were to assume" (conditional, not concessive)
 */
export default linguisticRule('としても', (r) => {
  r.either(
    // Pattern 1: Noun + としても with compound dependency
    // Most common pattern for role/position usage
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const to = b1.particle('と', 'to');
      const shi = b1.tok({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi');
      const te = b1.particle('て', 'te');
      const mo = b1.particle('も', 'mo');

      b1.inOrder(noun, to, 1);
      b1.inOrder(to, shi, 1);
      b1.inOrder(shi, te, 1);
      b1.inOrder(te, mo, 1);
      b1.headChild(noun, to, 'compound');
      b1.headChild(noun, shi, 'compound');
      b1.headChild(noun, te, 'compound');
      b1.headChild(noun, mo, 'compound');

      b1.captureSpan('としても', noun, mo);
    },

    // Pattern 2: Noun + としても with fixed dependency
    // Alternative parsing pattern
    (b2) => {
      const noun = b2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const to = b2.particle('と', 'to');
      const shi = b2.tok({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi');
      const te = b2.particle('て', 'te');
      const mo = b2.particle('も', 'mo');

      b2.inOrder(noun, to, 1);
      b2.inOrder(to, shi, 1);
      b2.inOrder(shi, te, 1);
      b2.inOrder(te, mo, 1);
      b2.headChild(noun, to, 'fixed');
      b2.headChild(noun, shi, 'fixed');
      b2.headChild(noun, te, 'fixed');
      b2.headChild(noun, mo, 'fixed');

      b2.captureSpan('としても', noun, mo);
    },

    // Pattern 3: Verb/Adj + としても with mark dependency
    // Common in hypothetical usage
    (b3) => {
      const pred = b3.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'pred');
      const to = b3.particle('と', 'to');
      const shi = b3.tok({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi');
      const te = b3.particle('て', 'te');
      const mo = b3.particle('も', 'mo');

      b3.inOrder(pred, to, 1);
      b3.inOrder(to, shi, 1);
      b3.inOrder(shi, te, 1);
      b3.inOrder(te, mo, 1);
      b3.headChild(pred, to, 'mark');
      b3.headChild(pred, shi, 'mark');

      b3.captureSpan('としても', pred, mo);
    },

    // Pattern 4: Noun + としても with case dependency
    // Case marker pattern
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const to = b4.particle('と', 'to');
      const shi = b4.tok({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi');
      const te = b4.particle('て', 'te');
      const mo = b4.particle('も', 'mo');

      b4.inOrder(noun, to, 1);
      b4.inOrder(to, shi, 1);
      b4.inOrder(shi, te, 1);
      b4.inOrder(te, mo, 1);
      b4.headChild(noun, to, 'case');

      b4.captureSpan('としても', noun, mo);
    },

    // Pattern 5: Any token + としても (catch-all with loose constraints)
    // For unexpected GiNZA parsings
    (b5) => {
      const base = b5.tok({}, 'base');
      const to = b5.particle('と', 'to');
      const shi = b5.tok({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi');
      const te = b5.particle('て', 'te');
      const mo = b5.particle('も', 'mo');

      b5.inOrder(base, to, 2);
      b5.inOrder(to, shi, 1);
      b5.inOrder(shi, te, 1);
      b5.inOrder(te, mo, 1);

      b5.captureSpan('としても', base, mo);
    }
  );
});
