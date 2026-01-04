import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たものだ.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // たことがある (JLPT5) - "have done before" (different grammar, no nostalgia)
  '日本に行ったことがある。',
  '寿司を食べたことがあります。',
  '富士山に登ったことがある。',

  // たことだ (fact/states that) - objective, matter-of-fact
  '失敗したことを忘れないで。',
  '失敗したことだが、頑張った。',

  // ものだ without past tense (supposed to / natural) - different grammar
  '水は低いところに流れるものだ。',
  '親の心子知らずとはこのことだ。',

  // Simple もの (noun thing) + だ without past tense
  'これは私のものだ。',
  'あれは誰のものですか。',

  // た form ending without ものだ
  '昨日、友達に会った。',
  '子供の頃、よく海に行った。',

  // たばかり (just did) - different grammar
  '日本に来たばかりだ。',
  '買ったばかりの本をなくした。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
