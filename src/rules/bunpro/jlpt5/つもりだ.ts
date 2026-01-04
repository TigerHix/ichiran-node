import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('つもりだ', (r) => {
  const tsumori = r.tok({ lemma: 'つもり', pos: 'NOUN' }, 'tsumori');
  // Intent/plan reading is usually realized as a nominal head modified by a verb phrase:
  //   起きる(acl) -> つもり
  const v = r.verb({}, 'v');
  r.headChild(tsumori, v, 'acl');
  r.captureAs('verb', v);
  r.capture(tsumori);
});

