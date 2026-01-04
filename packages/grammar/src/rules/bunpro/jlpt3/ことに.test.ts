import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことに.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: similar patterns that should NOT match
const negatives = [
  // ことになる (JLPT3 - "it is decided that", different grammar)
  '来月日本に行くことになった。',
  '会議は明日に行うことになった。',

  // ことにする (JLPT3 - "decide to", different grammar)
  '毎日運動することにしました。',
  '健康のためにジョギングすることにする。',

  // ことがある (JLPT3 - "sometimes", different particle)
  'この馬は人を蹴ることがある。',
  'たまに楽しいことがある。',

  // ことから (JLPT3 - "from the fact that", different particle)
  'コーヒーが冷たいことから、淹れられたのは前だと分かる。',
  '以上のことから、この結論に至りました。',

  // Simple こと + に without emotional predicate (plain adverbial use)
  '時間どおりに集合することにします。',

  // ことだ (JLPT2 - advice "should", different grammar)
  '健康のためには運動することだ。',

  // ことか (JLPT3 - emphasis, different particle)
  'どれくらい待ったことか。',
  'どれだけ嬉しかったことか。',

  // Noun + ことに (when こと is a regular noun, not dep=obl)
  '父のことはよく知っている。',
  'このことについて話したい。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
