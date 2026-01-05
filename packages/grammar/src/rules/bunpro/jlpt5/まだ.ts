import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('まだ', (r) => {
  // まだ (mada): still, not yet
  // Adverb highlighting something expected to be over but is not
  // Patterns: まだ + Verb［ている］, まだ + Noun + が + いる/ある

  const mada = r.adv({
    text: 'まだ',
    lemma: 'まだ',
  }, 'mada');

  r.capture(mada);
});
