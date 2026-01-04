import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './から言うと.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative test cases: sentences with から or 言う that should NOT match
const negatives = [
  // Simple から as source/from marker
  '東京から大阪まで新幹線で行きます。',
  '彼から電話がありました。',
  // 言う as a regular verb meaning "to say"
  '彼はそう言った。',
  '私は言うことを聞いてください。',
  // から...て form as "from...and then"
  '家から出て、会社に行きました。',
  // という conditional pattern (different grammar)
  '彼が来るということは、予定が変わったのだろう。',
  'これは何だと言いますか。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
