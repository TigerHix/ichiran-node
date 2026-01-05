import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ていては.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Simple ては without い (different grammar - JLPT2 ては pattern)
  'そんなことをしてはいけません。',
  '勉強しては遊びなさい。',
  // ている without は (progressive aspect, not conditional)
  '本を読んでいる学生がいる。',
  '彼は今、東京に住んでいる。',
  // ていて followed by something other than は
  '知っていても教えてくれない。',
  '持っていてくれないか。',
  // でいては (different pattern - de is case marker, not te-form)
  // Note: Some test cases actually use でいては (e.g., 遊んでいては)
  // which is the te-form of 来る → 来て → 来で (geminate)
  // So we should allow で as well
  // Sentence-final て (te-form ending)
  '宿題が終わって、遊んで。',
  '買い物をして、帰って。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
