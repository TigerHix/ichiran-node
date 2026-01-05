import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('こそ', (r) => {
  // こそ is an emphatic particle that replaces を/が/は
  // Pattern: Noun/Pronoun/Adv + こそ
  // The こそ particle acts as a case marker (dep=case) emphasizing the preceding element
  // Sometimes it has dep=fixed when attached to fixed expressions like それで

  r.either(
    // Pattern 1: Noun + こそ with dep=case (e.g., 愛こそすべてだ)
    (b) => {
      const koso = b.particle('こそ', 'koso', { dep: 'case' });
      const noun = b.noun({}, 'noun');
      b.inOrder(noun, koso, 1);
      b.captureSpan('こそ', noun, koso);
    },
    // Pattern 2: Pronoun + こそ with dep=case (e.g., あなたこそ、私こそ、こちらこそ)
    (b) => {
      const koso = b.particle('こそ', 'koso', { dep: 'case' });
      const pronoun = b.tok({ pos: 'PRON' }, 'pronoun');
      b.inOrder(pronoun, koso, 1);
      b.captureSpan('こそ', pronoun, koso);
    },
    // Pattern 3: Adverb + こそ with dep=case (e.g., 今度こそ、そんな時こそ)
    (b) => {
      const koso = b.particle('こそ', 'koso', { dep: 'case' });
      const adv = b.adv({}, 'adv');
      b.inOrder(adv, koso, 1);
      b.captureSpan('こそ', adv, koso);
    },
    // Pattern 4: Fixed/Conjunction expressions + こそ with dep=fixed (e.g., それでこそ)
    (b) => {
      const koso = b.particle('こそ', 'koso', { dep: 'fixed' });
      const fixed = b.tok({ depOneOf: ['fixed', 'cc'] }, 'fixed');
      b.inOrder(fixed, koso, 1);
      b.captureSpan('こそ', fixed, koso);
    }
  );
});
