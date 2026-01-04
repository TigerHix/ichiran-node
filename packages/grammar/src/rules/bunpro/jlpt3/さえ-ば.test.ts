import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さえ-ば.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences that should NOT match さえ-ば
const negatives = [
  // さえ alone (without ば conditional) - different grammar point
  '時間さえある。',
  'お金さえあるから、大丈夫です。',
  'それさえ知らなかった。',
  // Regular conditional (ば) without さえ
  '勉強すれば、試験に合格できます。',
  '行けば、会えます。',
  // でさえ (even with) - different grammar point
  '子供でさえ知っている。',
  '親友でさえ言わなかった。',
  // さえ + たら (rarer conditional form) - handled separately if needed
  'ギターさえ弾けたら誰でもオッケーです。',
  '英語さえ話せたら、どこでも働けます。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
