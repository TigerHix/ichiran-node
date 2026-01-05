import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たまでだ.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Verb + まで but not followed by copula (different grammar - ～るまでだ "will just do X")
  '誰も教えてくれないなら、自分で調べるまでだ。',
  // Verb in dictionary form + までだ (different grammar)
  'やるまでだ、やるしかない。',
  // まで as temporal limit (different meaning)
  '明日までにこの仕事を終わらせたい。',
  '店は夜10時まで開いている。',
  // まで followed by particle + copula (different structure)
  'ここまでは来たが、先には進めない。',
  // ～てまで (extent of action)
  'そこまでしてやる必要はない。',
  // Verb past form but different meaning (not "merely did")
  'これだけ言ったまではやめられない。',
  // Noun + まで + copula (different structure)
  'これまでは順調だった。',
  // ～るまでの (future action, not past)
  '成功するまでの道のりは長い。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
