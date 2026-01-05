import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かたわら.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Different grammar - 一方で (on the other hand) not かたわら
  '一方で都市化が進んでいる。',
  // Different grammar - ながら (simultaneous) not かたわら
  '音楽を聴きながら勉強しています。',
  'ご飯を食べながらテレビを見る。',
  // Different grammar - ついでに (while at it) not かたわら
  '買い物のついでに本を買った。',
  // Different grammar - うちに (while) not かたわら
  '日本にいるうちに富士山に登りたい。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
