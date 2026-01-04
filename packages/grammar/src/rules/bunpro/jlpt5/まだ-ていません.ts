import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('まだ-ていません', (r) => {
  // まだ～ていません (mada te imasen): still haven't done (something)
  // Expresses that (A) remains in a state of not being done
  // Pattern: まだ + Verb［て］+ いません/いない

  // The rule captures from まだ through the negative form
  // We look for まだ + te-form + negative auxiliary

  const mada = r.adv({ text: 'まだ' }, 'mada');
  const te = r.tok({ textOneOf: ['て', 'で'] }, 'te');
  const negative = r.aux({
    // Match various negative forms: いません, いません, いない, etc.
    lemmaOneOf: ['ない', 'ぬ', 'ます'],
  }, 'negative');

  r.inOrder(mada, te, 10);
  r.inOrder(te, negative, 3);
  r.captureSpan('まだ-ていません', mada, negative);
});
