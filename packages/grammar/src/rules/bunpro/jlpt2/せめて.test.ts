import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './せめて.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the せめて grammar rule
const negatives = [
  // Different grammar: せめて (as verb stem)
  // When せめて appears as part of 責める (to blame/criticize) conjugation
  // Note: せめて is almost always the adverb meaning "at least"

  // Similar-sounding but unrelated patterns
  // 攻めて (semete - attack) - same sound, different kanji
  '敵の城を攻めて取る。',  // "Attack and take the enemy castle" (攻めて, not せめて)
  '彼の意地を攻めてみた。',  // "I tried attacking his stubbornness" (攻めて, not せめて)

  // 勧めて (susumete - recommend) - different verb
  'この映画を勧めてみた。',  // "I recommended this movie" (勧めて, not せめて)

  // 責めて (semete - to blame/press) - the etymological origin
  // While related, the verb form typically uses kanji
  '彼の失敗を責めてはいけない。',  // "You shouldn't blame him for his failure" (責めて)

  // Other ~て forms that sound similar
  '支えて (sasaete - support)',
  '挑んで (idonde - challenge)',
  '占めて (shimete - occupy)',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
