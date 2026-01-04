import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がみられる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 見える (spontaneous "visible" - different verb)
  '富士山が見える。',
  '星が見える。',
  // 見る without potential auxiliary - regular verb form
  '彼を見る。',
  'それを見てください。',
  // Other verbs with られる (not 見る)
  '彼が来られる。',
  '子供が泣かれる。',
  // 聞こえる vs 聞ける (different potential verbs)
  '音が聞こえる。',
  '彼が聞ける。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
