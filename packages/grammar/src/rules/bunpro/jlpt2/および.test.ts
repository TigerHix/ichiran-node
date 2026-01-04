import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './および.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the および grammar rule
const negatives = [
  // 及ぶ (oyobu) - verb "to reach, extend to"
  // および is derived from this verb, but the verb itself is different grammar
  '私の悩みは彼に及ぶ。',
  '影響が世界中に及んだ。',
  '被害甚大に及ぶ。',

  // と (to) - casual "and" for noun listing
  // および is the formal version of と
  'りんごとバナナを買った。',
  '東京と大阪へ行きたい。',

  // や (ya) - "and" partial listing (implies "and so on")
  'りんゴやバナナを買った。',
  '本や雑誌を読む。',

  // そして (soshite) - conjunction connecting clauses/sentences
  '彼は来た。そして、彼女も来た。',
  '雨が降った。そして、風も吹いた。',

  // ならびに (narabini) - similar formal conjunction, but different word
  // This is a related but different formal conjunction
  '名前ならびに住所を記入してください。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
