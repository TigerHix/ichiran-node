import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './したがって.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Similar conjunctions that are more casual
  '彼は長男です。だから、次期社長はおそらく彼でしょう。',
  '彼は長男です。それで、次期社長はおそらく彼でしょう。',
  '彼は長男です。なので、次期社長はおそらく彼でしょう。',
  '彼は長男です。ですから、次期社長はおそらく彼でしょう。',
  '彼は長男です。よって、次期社長はおそらく彼でしょう。',

  // Different discourse markers
  '彼は長男です。そこで、次期社長はおそらく彼でしょう。',
  '彼は長男です。その結果、次期社長はおそらく彼でしょう。',

  // したがって as part of a verb phrase (したがう + て-form)
  '規則にしたがって行動します。',
  '先生の指示にしたがってください。',
  '彼は従って行きました。', // followed (verb) not conjunction

  // Similar sounding words
  'しっかりとしてください。',
  'したくないのですが、',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
