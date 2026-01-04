import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './風.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 風 as "wind" (natural phenomenon, not style suffix)
  // Note: These should not match because they don't use 風 as a suffix
  '風が吹いています。',
  '今日は風が強いです。',
  '風の音が聞こえる。',

  // 風 meaning "appearance/manner" but not as the suffix
  // (if there are any such cases)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
