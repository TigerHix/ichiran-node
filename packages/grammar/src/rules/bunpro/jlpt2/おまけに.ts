import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('おまけに', (r) => {
  // おまけに is a conjunctive adverb meaning "on top of that", "in addition",
  // or "to make matters worse". It's a fixed expression.
  const omakeni = r.adv({ text: 'おまけに' }, 'omakeni');
  r.capture(omakeni);
});
