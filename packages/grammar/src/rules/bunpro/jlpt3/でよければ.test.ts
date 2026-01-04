import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './でよければ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negatives: sentences that should NOT match でよければ (if acceptable)
const negatives = [
  // Instrumental で (means/means) + other verbs - not "if acceptable"
  '日本語で手紙を書いた。',
  '鉛筆で絵を描く。',
  '電車で行く。',

  // Locative で (location) - not "if acceptable"
  '図書館で勉強する。',
  '公園で遊ぶ。',

  // ではない (copula negation) - different grammar
  'これは本ではない。',
  '彼は学生ではない。',

  // できれば (if possible) - different grammar, similar meaning
  'できれば来てください。',

  // でもいい (even if/it's okay to) - different grammar
  '飲んでもいい。',

  // Simple conditional ば forms without で
  '行ければ行く。',
  'できれはいい。',

  // Other particles + conditionals
  'ければ行く。',
  'とよければどうぞ。',

  // Noun + だ + よければ (copula + conditional) - different pattern
  'これだよければいい',

  // Adjective + で (te-form) - different grammar
  'この部屋はきれいで広い。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
