import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('とうとう', (r) => {
  // とうとう is an adverb meaning "finally, at last, after all"
  // Used when something happens after a long journey, effort, or wait
  const toutou = r.adv({ text: 'とうとう' }, 'toutou');
  r.capture(toutou);
});
