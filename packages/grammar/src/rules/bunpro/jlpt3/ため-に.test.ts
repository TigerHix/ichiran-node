import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ため-に.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Instrumental に (means "using/by means of", not "for the sake of")
  '鉛筆で書く。',
  '日本語で話してください。',
  // Locative に (means "at/in", not "for the sake of")
  '公園に行きます。',
  '家に帰る。',
  // Directional に (means "to", not "for the sake of")
  '東京に行きます。',
  '学校に来てください。',
  // Purpose を (direct object, not "for")
  '本を読む。',
  'ご飯を食べる。',
  // Similar grammar with different meaning
  'ようになる', // becomes/comes to
  'ためにする', // different construction
  // ため as standalone noun (not the grammar pattern)
  'これはためにならない。',
  'ためになる本です。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
