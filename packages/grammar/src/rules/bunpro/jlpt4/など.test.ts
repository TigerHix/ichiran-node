import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './など.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: Similar patterns that should NOT match
const negatives = [
  // とか (more casual listing particle)
  'リンゴとかバナナとかを買いました。',
  '映画とか見るのが好きです。',
  // や (listing particle without など)
  'リンゴやバナナを買いました。',
  'イタリアやアメリカに行きたい。',
  // だれか (someone) - not など
  'だれか来ましたか。',
  // なにか (something) - not など
  'なかば食べたいです。',
  // なんて (more casual/dismissive variant)
  '彼なんて知りません。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
