import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('しかも', (r) => {
  const shikamo = r.tok({
    textOneOf: ['しかも', '然も', '而も', '併も'],
    posOneOf: ['CCONJ', 'ADV', 'SCONJ'],
  }, 'shikamo');

  r.capture(shikamo);
});
