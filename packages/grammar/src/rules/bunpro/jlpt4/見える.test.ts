import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './見える.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 見られる (potential form of 見る - different grammar point)
  '彼は映画を見られる。',
  '富士山が見られます。',
  // 見る without potential/spontaneous form - regular verb
  '彼を見る。',
  'それを見てください。',
  '映画を見ません。',
  // 聞こえる (audible - different spontaneous verb)
  '音が聞こえる。',
  '声が聞こえない。',
  // Other potential verbs
  '彼が来られる。',
  '英語が話せる。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
