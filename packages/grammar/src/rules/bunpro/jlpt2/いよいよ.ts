import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('いよいよ', (r) => {
  // いよいよ is an adverb meaning "at last, finally, more and more, increasingly"
  // Casual way of saying "finally" - indicates reaching an important stage after effort
  // Can also mean "more and more" when describing increasing intensity
  const iyoiyo = r.adv({ text: 'いよいよ' }, 'iyoiyo');
  r.capture(iyoiyo);
});
