import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('v-まで', (r) => {
  // Match the particle まで when it attaches to a verb
  // まで here means "until" or "by the time"
  // Key discriminator: dep=mark (not dep=case like noun-まで)
  const made = r.particle('まで', 'made', { dep: 'mark' });
  const verb = r.verb({}, 'verb');

  // まで marks the verb as an extent/limit
  r.headChild(verb, made, 'mark');

  r.captureSpan('v-まで', verb, made);
});
