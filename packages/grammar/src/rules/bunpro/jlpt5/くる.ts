import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('くる', (r) => {
  const kuru = r.verb({ lemma: 'くる', conjugationClass: 'カ行変格' }, 'kuru');
  r.capture(kuru);
});
