import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ないことはない.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // こと + は + ない (JLPT3 - different grammar: "no need to" or "never happens")
  // This is single negative, not the double negative pattern of ないことはない
  '心配することはない。',
  'そんな急ぐことはありません。',
  '彼と話すことはない。',

  // たことがない (JLPT5 - past experience: "have never done")
  '私は寿司を食べたことがない。',
  '京都に行ったことがありますか。',

  // 〜ないことには (JLPT2 - prerequisite: "unless...")
  '実際に見てみないことには、わからない。',
  '努力しないことには、成功できない。',

  // 〜ないわけではない (JLPT2 - "not necessarily")
  'できないわけではないが、時間がかかる。',
  '行きたくないわけではない。',

  // 〜ないでもない (JLPT2 - "somewhat / kinda")
  '美味しくないでもない。',
  '行けなくもない。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
