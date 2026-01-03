import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('か', (r) => {
  r.either(
    (b) => {
      const ka = b.particle('か', 'ka');
      b.capture(ka);
    },
    (b) => {
      const ka = b.tok({ text: 'か', pos: 'ADP', dep: 'case' }, 'ka');
      b.capture(ka);
    }
  );
});
