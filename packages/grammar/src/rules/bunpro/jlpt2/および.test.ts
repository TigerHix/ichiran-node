import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './および.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the および grammar rule
const negatives = [
  // と (to) - casual "and" for everyday conversation
  'りんごとバナナを買いました。',
  '私は田中さんと行きました。',
  '本とペンが必要です。',

  // や (ya) - partial list "and things like"
  'りんごやバナナを買いました。',
  '本やペンが必要です。',

  // そして (soshite) - connects clauses/sentences, not just nouns
  '彼は家に帰りました。そして、寝ました。',
  '勉強した。そして、試験に合格した。',

  // それから (sorekara) - "and then" (temporal sequence)
  '朝ごはんを食べた。それから、学校に行った。',

  // かつ (katsu) - "and" emphasizing simultaneity
  '彼は政治家であり、かつ著作家でもある。',
  '美しく、かつ賢い女性。',

  // ならびに (narabini) - even more formal, typically legal/academic
  // This uses a different conjunction, not および
  '氏名ならびに住所をご記入ください。',

  // および as verb stem (from 及ぶ - to reach/extend)
  // When functioning as verb conjugation, it has different dependency structure
  '影響は全国に及びました。',
  '被害は数千人に及びます。',
  '彼の功績は多岐に及ぶ。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
