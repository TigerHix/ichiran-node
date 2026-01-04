import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './しかしながら.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the しかしながら grammar rule
const negatives = [
  // しかし alone (without ながら) - "however" (less formal)
  '彼は来ませんでした。しかし、電話がありました。',
  '雨が降っています。しかし、傘を持っていません。',
  '日本語は難しい。しかし、面白いです。',

  // しかし + ながら as separate components (verb form pattern)
  // 食べながら, 働きながら, etc.
  '音楽を聴きながら勉強します。',
  '歩きながら話しました。',
  'テレビを見ながらご飯を食べる。',

  // ながら + Verb stem (while doing X, do Y)
  '彼は泣きながら話した。',
  '母は歌いながら料理をしている。',

  // ながら as "although" with verb stem
  '残念ながら、参加できません。',
  '不思議ながら、本当の話です。',

  // Similar conjunctions that are different grammar
  'それでも、頑張ります。',
  'それなのに、来なかった。',
  'それにしても、高いですね。',

  // 乍ら (kanji variant) used in verb stem pattern
  '涼しくならずとも、乍ら暑い。'
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
