import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './のように-のような.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // ようだ as conjecture (seems/appears) without noun+の
  '彼は日本人のようだ。',
  '雨が降るようだ。',
  // ように meaning "in order to" (purpose)
  '見えるように座って。',
  '分かるように説明して。',
  // みたい (casual version without の)
  '彼みたいな人。',
  'スープみたいなカレー。',
  // そうに/そうな (seems/looks like - different grammar)
  '彼は幸せそうだ。',
  'おいしそうなケーキ。',
  // ふうに/ふうな (in a way/manner)
  'こんなふうに書く。',
  'そんなふうな話。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
