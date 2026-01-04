import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かた.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // かた/方 meaning "direction" or "side" (not way of doing)
  '右の方に歩いて。',
  '左の方を見て。',
  '彼の方に行った。',
  'この方向は南だ。',
  // 方 as a person (less common meaning)
  '読者の方へ。',
  '参加者の方にお礼を言う。',
  // Verb not in stem form (dictionary form)
  '食べるかたを教えて。',
  '使うかたが分からない。',
  // 難しい (muzukashii) - adjective ending in かたい but not a verb stem
  'これは難しい。',
];

// GiNZA parsing limitation:
// 変わったさきかた - GiNZA parses "さき" as proper name (tag=名詞-固有名詞-人名-名)
// instead of verb stem (lemma=さく, inflectionForm=連用形-一般)
// This is a GiNZA error - it misidentifies the verb as a person's name.
// Matching all "名詞-固有名詞-人名-名" + かた would overcapture on actual names.
const skipPositives = [
  '変わったさきかたをする花がある。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
