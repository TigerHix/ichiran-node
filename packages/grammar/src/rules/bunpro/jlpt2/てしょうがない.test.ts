import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てしょうがない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the てしょうがない grammar rule
const negatives = [
  // てたまらない (tetamaranai) - "can't stand, unbearable"
  // Different grammar: expresses degree/intensity, not inevitability
  '会いたくてたまらない。',
  '暑くてたまらない。',
  'うるさくてたまらない。',

  // てならない (tenaranai) - "can't help but, very"
  // Different grammar: more formal/written style
  '気になってならない。',
  '心配でならない。',
  '会いたくてならない。',

  // てはかなわない (tehakanawanai) - "unbearable, can't deal with"
  // Different grammar: focus on external conditions being intolerable
  'こんなに暑くてはかなわない。',
  '毎日残業ではかなわない。',

  // ざるを得ない (zaruoenai) - "have no choice but, cannot help but"
  // Different grammar: expresses compulsion due to external circumstances
  '行かざるを得ない。',
  '謝罪せざるを得ない。',

  // ずにはいられない (zuwairenai) - "cannot help but do, cannot resist doing"
  // Different grammar: focus on being compelled to act
  '彼を応援せずにはいられない。',
  '泣かずにはいられない。',

  // Simple て-form + ない (te-form + not) - different grammar
  '彼を知っていない。',
  '行っていない。',
  '読んでいない。',

  // て + が (te + ga) - conjunction + topic particle
  // Different grammar: just conjunction + topic, not our pattern
  '行って、私は帰ります。',
  '暑くて、私は疲れた。',

  // 仕方ない (shikatana i) - "no way, helpless" (without て)
  // Different grammar: standalone adjective phrase
  '仕方ない。',
  'もう仕方ない。',

  // 仕方なく (shikatanaku) - "reluctantly, unavoidably"
  // Different grammar: adverbial form
  '仕方なく承知した。',
  '仕方なく帰った。',

  // 〜てから (tekara) - "after doing"
  // Different grammar: temporal conjunction
  '帰ってから電話します。',
  '食べてから出かけます。',

  // Noun + 仕方がない (different pattern)
  'この仕方がない。',
  'その方法は仕方がない。',

  // 〜げ (suffix) - appearance/looking
  // Similar sound but different grammar
  '彼は悲しげだ。',
  '彼女は楽しげに笑う。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
