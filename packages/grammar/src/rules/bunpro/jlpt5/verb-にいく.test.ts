import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verb-にいく.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Verb + に for other purposes (direction, not purpose)
  '日本に行きたい。', // want to go to Japan (directional に)
  '駅に着いた。', // arrived at station (directional に)
  '友達に会った。', // met friend (indirect object に)
  // Motion verb without purpose
  '公園に行った。', // went to park (directional に, no purpose verb)
  '家に帰る。', // return home (directional に)
  // て-form + いく (sequential action, not purpose)
  '食べて行く。', // eat and go (different grammar)
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Verb stems that GiNZA parses incorrectly as other POS
//
// GiNZA parses some verb stems incorrectly:
//   わたし (from 渡す) → pos=PRON (pronoun), not verb stem ✗ INCORRECT
//   のみ (from 飲む) → pos=ADP (particle), not verb stem ✗ INCORRECT
//
// These are homographs with different parts of speech:
//   わたし = 渡す (verb "to hand over") stem form
//   わたし = 私 (pronoun "I/me")
//   のみ = 飲む (verb "to drink") stem form
//   のみ = のみ (particle "only/just")
//
// When parsed as PRON or ADP (non-verb), we can't match using verb patterns.
// To match these cases, we would need to match by surface text alone, which would:
//   1. Overcapture: match any "わたし" or "のみ" regardless of context
//   2. Match non-verb-にいく constructions (e.g., pronoun + に + verb)
//
// CONCLUSION: GiNZA limitation for these homographs.
//
// ANALYSIS: Verbal nouns that look like verb stems
//
// 釣り (fishing) can be either:
//   - Noun: 釣り (fishing as an activity)
//   - Verb stem: 釣る (to fish) + る removed
//
// GiNZA parses 釣り as tag=名詞-普通名詞-一般 (regular noun), not as a verb form.
// Matching all nouns with this tag would overcapture regular nouns + に + いく patterns.
//
// CONCLUSION: GiNZA limitation for distinguishing verbal nouns from verb stems.
const skipPositives = [
  '釣りにいく。',
  'お金をわたしにいった。',
  '今夜、のみにいきます。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
