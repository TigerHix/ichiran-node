import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ひつようがある.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // がひつよう (nouns need to be necessary, not verbs)
  '免許が必要だ。',
  '時間が必要です。',
  'お金が必要だった。',
  // 必要 without が (not this pattern)
  '勉強が必要です。',
  '練習が必要だ。',
  // が + ある (subject marker + exist, not 必要がある pattern)
  '私がある。',
  '問題がある。',
  '彼がある。',
  // は + 必要 (topic marker, not subject marker)
  '勉強は必要がある。',
  '練習は必要がある。',
  // に + 必要 (different particle)
  '成功に必要がある。',
  // 必要 as standalone verb/noun
  '必要なもの。',
  '必要とする。',
  '必要があること。', // Different grammar structure
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
