import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ないではいられない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ないではいられない grammar rule
const negatives = [
  // ずにはいられない (zu ni wa irarenai) - same meaning but more formal/literary
  // Uses classical negative stem (ず) instead of (ない)
  '彼の一言には驚かずにはいられない。',
  '笑わずにはいられない。',
  '涙を流さずにはいられない。',

  // てはいられない (te wa irarenai) - "can't afford to, unable to remain doing"
  // Different meaning - expresses inability to continue a state, not uncontrollable urge
  '待ってはいられない。',
  'こんなに遅くまで寝てはいられない。',
  '黙ってはいられない。',

  // Simple negation with いられない (irarenai) - "can't stay in a state"
  // This is potential negative of いる (to be/stay), not the grammar pattern
  'ここにはいられない。',
  'もう待ってはいられない。', // This could match te-wa-irarenai rule
  'こんな状態にはいられない。',

  // ざるを得ない (zaru o enai) - "have no choice but to" (more objective)
  // Uses classical negative form + を得
  '同意せざるを得ない。',
  '謝罪せざるを得ない。',
  '従わざるを得ない。',

  // ないわけにはいかない (nai wake ni wa ikanai) - "must not fail to"
  // Different grammatical structure
  '行かないわけにはいかない。',
  '勉強しないわけにはいかない。',
  '見ないわけにはいかない。',

  // てたまらない (te tamaranai) - "can't help but feel" (overwhelming feeling)
  // Uses te-form + たまらない
  '暑くてたまらない。',
  '会いたくてたまらない。',
  'うれしくてたまらない。',

  // てしょうがない (te shouganai) - "can't help but feel" (emotional)
  // Uses te-form + しょうがない
  '心配でしようがない。',
  '会いたくてしょうがない。',
  '寂しくてしょうがない。',

  // Simple negative verb forms (without いられない)
  '食べない。',
  '行かない。',
  'しない。',

  // Locative では (de wa) - "at/in" (location, not conjunction)
  // Here では marks location, not part of the grammar pattern
  '東京では雪が降っている。',
  'この部屋では静かにしてください。',

  // Conjunction では with different meaning
  'これでは困る。', // "With this (situation), it's troubling"
  'そんなでは失礼です。', // "Being like that is rude"
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
