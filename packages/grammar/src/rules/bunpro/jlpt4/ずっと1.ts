import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ずっと1', (r) => {
  // ずっと is an adverb meaning "continuously, all throughout, entire time, ever (since), the whole time, all the way"
  const zutto = r.adv({ text: 'ずっと' }, 'zutto');
  r.capture(zutto);
});
