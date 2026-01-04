import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけしか.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // だけだけ (dake dake) - not the double particle construction
  // This would be ungrammatical or have different meaning

  // しかだけ (shika dake) - wrong order
  // The grammar requires だけしか, not しかだけ

  // だけ followed by positive verb (not negative)
  // だけ requires positive, だけしか requires negative
  'お金だけあります。', // Has only money (positive)
  '時間だけあります。', // Has only time (positive)

  // しか followed by positive verb (also wrong - しか requires negative)
  // 'これしかあります。', // This is ungrammatical

  // だけ alone (without しか) - different grammar point
  'りんごだけ食べた。', // Ate only apples (だけ without しか)
  '今日だけ来てください。', // Please come only today (だけ without しか)

  // しか alone (without だけ) - different grammar point (JLPT4)
  'これしかありません。', // Only have this (しか without だけ)
  '一人しかいません。', // Only one person (しか without だけ)

  // だけで (dake de) - "just by doing" (different grammar)
  '見るだけで分かる。', // Can understand just by looking

  // ばかり (bakari) - "nothing but/excessive" (different grammar)
  '漫画ばかり読む。', // Do nothing but read manga
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
