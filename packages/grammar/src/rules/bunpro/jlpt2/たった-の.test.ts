import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たった-の.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the たった grammar rule
const negatives = [
  // 立つ (tatsu) - different verb (to stand)
  '彼は立った。',
  '電車に立っている人が多い。',

  // 尋ねる (tazuneru) - different verb (to ask/inquire)
  '道を尋ねる。',
  '彼女に名前を尋ねた。',

  // ただ (tada) - similar but less emphatic
  'ただの人間です。',
  'ただ遊びに行っただけです。',

  // わずか (wazuka) - more formal "slightly/barely"
  'わずかに聞こえる。',
  'わずかの差で勝った。',

  // Similar sounding but unrelated words
  // たっぷり (tappuri) - "plentifully" (opposite meaning!)
  '時間たっぷりあります。',
  'クリームをたっぷり塗る。',

  // たとえば (tatoeba) - "for example"
  'たとえば、彼の場合はそうだ。',

  // たち (tachi) - plural marker for people
  '私たちで行きます。',
  '子供たちが遊んでいる。',

  // Sentences with たった but as non-adverb usage (very rare)
  // Example: "たった" as a sound effect or name (unlikely but possible)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
