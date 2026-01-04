import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ようになる.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases - sentences that should NOT match
const negatives = [
  // Plain なる without ように (simple "becomes", not "comes to be able to")
  '春になる。',
  '彼は先生になった。',
  '暗くなる。',
  // ように (purpose/aim, not change of state)
  '忘れないようにメモした。',
  '間に合うように急いだ。',
  // ようにする (making effort to ensure something)
  '毎日運動するようにしている。',
  '健康に気をつけるようにしてください。',
  // ～のように (manner/similarity = "like X")
  '鳥のように空を飛びたい。',
  '彼のように優しくなってほしい。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
