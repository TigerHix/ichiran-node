import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './が気になる.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the が気になる grammar rule
const negatives = [
  // 気にする (ki ni suru) - active worrying (volitional)
  // "to worry about (actively/intentionally)"
  '彼のことを気にするな。',
  'そんなことを気にしないで。',
  '細かいことを気にしすぎる。',
  '彼女は私の服装を気にしている。',
  '失敗を気にする必要がない。',
  '人の目を気にするな。',

  // 関心がある (kanshin ga aru) - "to have interest in" (stronger/more formal)
  // "が気になる" alternative mentioned in Bunpro feedback
  '彼の話に関心がある。',
  'この問題に関心を持っています。',
  '政治に関心がない。',

  // Just "気になる" without the subject marker (different grammatical structure)
  // When used as "it's concerning/worrying" without explicit topic
  'ちょっと気になるんだけど。',

  // Noun + 気 constructions with different meanings
  // 気がする (ki ga suru) - "to have a feeling/feeling like"
  '何か変な気がする。',
  '雨が降る気がする。',
  '彼は来る気がしない。',

  // 気がつく (ki ga tsuku) - "to notice/realize"
  '私は間違いに気がついた。',
  '彼は私に気がつかなかった。',
  '遅刻したことに気がついた。',

  // 気に入る (ki ni iru) - "to be fond of/like"
  'その服が気に入った。',
  '彼はその考えに気に入らない。',
  '私の新しい車が気に入っています。',

  // Noun + が + なる patterns without "気に"
  // 春になる (haru ni naru) - "to become spring"
  'もうすぐ春になる。',
  // 夜になる (yoru ni naru) - "to become night/night falls"
  'もうすぐ夜になる。',
  // 彼は先生になった (kare wa sensei ni natta) - "he became a teacher"
  '彼は立派な医者になった。',

  // Similar sounding but different grammar patterns
  // になる (ni naru) - "to become" (without ki)
  '彼は大人になる。',

  // Cases where "が" is not a subject marker but part of a conjunctive form
  // ～てが (～te ga) - conjunction (rare)
  // These should not match as "が気になる"
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
