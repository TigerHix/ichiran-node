import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おおよそ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the おおよそ grammar rule
const negatives = [
  // およ (oyo) - different word/fragment
  // Not a real word in isolation, unlikely to occur

  // およそ as "as a rule/generally" (slightly different meaning)
  // Example: 凡そ高価な作品 (Expensive works as a rule)
  // This is still およそ but with a different nuance
  // However, GiNZA likely parses it the same way, so this might match
  // That's acceptable as it's the same grammar point

  // Similar sounding but different words
  // およぐ (oyogu) - to swim
  '彼はプールでおよいでいる。',
  '子供はおよぐことが好きだ。',

  // およぼす (oyobosu) - to bring about, cause
  '影響をおよぼす。',

  // お (o) - different words starting with お
  'お名前は何ですか。',
  'お元気ですか。',

  // そ (so) - different words ending in そ
  'それください。',
  'その本を読みたい。',

  // だいたい (daitai) - similar meaning but different word (more casual)
  'だいたい１０人来た。',
  'だいたい分かったけど、詳しく説明して。',

  // ほぼ (hobo) - "almost/nearly", not "approximately"
  'ほぼ完成した。',
  'ほぼすべての人が賛成した。',

  // だいたい as "in the first place" (different meaning)
  'だいたい何で遅刻したの？',
  'だいたいそんなこと言うなよ。',

  // Exact numbers without approximation
  // (These test that we don't overcapture on unrelated ADV + NUM patterns)
  '正確に１００人です。',
  '正確に３０分かかりました。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
