import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('おそらく', (r) => {
  // おそらく is a formal adverb meaning "probably, likely, perhaps"
  // Often used with conjectural expressions like だろう, でしょう, かもしれない
  // Can also carry the nuance of "I fear that" in formal contexts
  const osoraku = r.adv({ text: 'おそらく' }, 'osoraku');
  r.capture(osoraku);
});
