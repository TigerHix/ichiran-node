import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ために.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Instrumental に (means "to/towards", not "ため")
  '東京に行く。',
  '学校に来る。',
  '友達に会う。',
  // Time expressions
  '三時に会いましょう。', // time marker に
  '朝に早く起きる。', // time marker に
  // Direction/destination
  '北に向かう。', // direction に
  '駅に出る。', // direction に
  // Indirect object
  '彼に本をあげる。', // indirect object に
  '先生に質問する。', // indirect object に

  // NOTE: The following sentences would ideally be negatives (purpose/benefit meaning vs cause),
  // but ために has semantic overlap between "due to/cause" and "for/benefit".
  // These are excluded from negatives to avoid false negatives:
  // - 'これは私のための本です。' (book for me - could be cause or purpose)
  // - '君のためにする。' (do for you - could be cause or purpose)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
