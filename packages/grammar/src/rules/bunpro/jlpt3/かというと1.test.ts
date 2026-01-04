import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かというと1.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences with similar particles that should NOT match
const negatives = [
  // Simple というと without question particle (different grammar - topic introduction)
  '日本というと、桜を思い出します。',
  '京都というと、古い寺がたくさんあります。',
  // かというか - "or rather" expression (different grammar point)
  '彼が来なかったかというか、遅れただけです。',
  // というと without question context
  '彼が行ったというと、本当ですか。',
  // Plain か + いう (question + "say" without conditional)
  '何か言いましたか。いうと、あの...',
  // かというのは with nominalizer (different structure)
  '彼が来なかったかというのは、病気だからです。',
  // Simple quotation という (just "called" or "said")
  'これは何という花ですか。',
  '田中という人から電話がありました。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
