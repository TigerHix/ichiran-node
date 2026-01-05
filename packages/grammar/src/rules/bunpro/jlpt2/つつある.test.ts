import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './つつある.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the つつある grammar rule
const negatives = [
  // ている (te-iru) - neutral progressive form, different grammar
  '彼はりんごを食べている。',
  '雨が降っている。',
  '勉強している。',
  '彼女は眠っている。',

  // ていく (te-iku) - "start to" or "continue to", different pattern
  'この本は難しくなっていく。',
  '少しずつ良くなっていく。',
  '彼らは遠ざかっていく。',

  // てくる (te-kuru) - "become" or "change over time", different pattern
  '最近暑くなってきた。',
  '彼が走ってきた。',
  '雨が降ってきた。',

  // つづける (tsuzukeru) - "continue to", different grammar
  '勉強しつづける。',
  '走りつづける。',
  '生きつづける。',

  // つつも (tsutsu mo) - "even while" / concessive, different meaning
  '知りつつも、無視した。',
  '緊張しつつも、楽しめました。',
  '思いつつも、言えなかった。',

  // Simple verb conjugations not related to つつある
  '行きます。',
  '食べました。',
  '勉強しません。',

  // Verb + ある without つつ (different grammar - e.g., てある for state)
  '黒板に字が書いてある。',
  '準備してある。',
  '窓が開けてある。',

  // Verb stem followed by unrelated auxiliary
  '見る。',
  '食べる。',
  '来る。',

  // ながら (nagara) - "while doing", different structure
  '歩きながら話す。',
  '考えながら書く。',
  '音楽を聴きながら走る。',

  // て-form for various other grammar points
  '行っても大丈夫だ。',
  '読んでもわからない。',
  '食べてください。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
