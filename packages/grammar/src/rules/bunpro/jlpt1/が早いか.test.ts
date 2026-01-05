import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './が早いか.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Similar "as soon as" expressions that are NOT が早いか
  // か-ないかのうちに - "whether or not X happens" (repeated verb)
  '家の戸を開けるか開けないかのうちに、犬が飛び出してきた。',
  '試験の答案を書き終えるか終えないかのうちに、火災警報が鳴った。',

  // なり - "as soon as" (follows verb directly)
  'ベルが鳴るなり、学生たちが教室を出た。',
  '彼は家に帰るなり、寝てしまった。',

  // や否や - "no sooner than" (formal/literary)
  '雨が降り始めるや否や、傘をさす人々が現れた。',
  '彼が舞台に現れるや否や、観客から拍手が沸き起こった。',

  // たとたん(に) - "the moment" (requires past tense verb)
  'ドアを開けたとたんに、猫が飛び出してきた。',
  '家に帰ったとたんに、電話が鳴りました。',

  // かと思うと - "as soon as X, then Y" (different structure)
  '雨が降るかと思うと、すぐ止んだ。',
  '彼は部屋に入ってくるかと思うと、すぐ出ていった。',

  // そばから - "as soon as" (with repetition/futile effort)
  '子供が片付けるそばから、部屋がまた散らかってしまう。',
  '覚えるそばから忘れてしまう。',

  // が + 早い but different context (not the fixed expression)
  '彼が早く来るかどうかわからない。', // Question, not the pattern
  '雨が早く止むことを祈っている。', // 早く as adverb, not 早いか

  // Similar surface patterns with different meanings
  '試合が早いですね。', // 早い as predicate
  '彼が早いかというと、そうでもない。', // Different meaning
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
