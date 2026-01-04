import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('いい', (r) => {
  // Match いい as an adjective meaning "good" (irregular form of よい)
  const ii = r.adj({
    lemmaOneOf: ['いい', 'よい'],
    tag: '形容詞-非自立可能',
  }, 'ii');
  r.capture(ii);
});
