import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('すると', (r) => {
  // すると (conjunction: "and then", "so", "upon doing")
  // A sentence starter meaning "once (A) is done, (B)" or "upon (A), (B)"
  // Often considered abbreviation of そうすると

  // Match by exact text
  const suruto = r.tok({ text: 'すると' }, 'suruto');
  r.capture(suruto);
});
