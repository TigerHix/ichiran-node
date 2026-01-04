import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './か-ないかのうちに.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Similar "as soon as" expressions that are NOT か-ないかのうちに
  // たとたんに - "the moment X happened" (requires past tense verb)
  'ドアを開けたとたんに、猫が飛び出してきた。',
  '家に帰ったとたんに、電話が鳴りました。',

  // かと思うと - "as soon as X, then Y" (different structure)
  '雨が降るかと思うと、すぐ止んだ。',
  '彼は部屋に入ってくるかと思うと、すぐ出て行った。',

  // なり - "as soon as" (follows verb directly)
  'ベルが鳴るなり、学生たちが教室を出た。',
  '彼は家に帰るなり、寝てしまった。',

  // がはやいか - "the moment X" (more emphatic)
  '先生が教室に入るがはやいか、学生たちは席に着いた。',
  '試合が終わるがはやいか、観客がスタジアムを去り始めた。',

  // やいなや - "no sooner than" (formal/literary)
  '雨が降り始めるやいなや、傘をさす人々が現れた。',
  '彼が舞台に現れるやいなや、観客から拍手が沸き起こった。',

  // そばから - "as soon as" (with sense of repetition/futile effort)
  '子供が片付けるそばから、部屋がまた散らかってしまう。',
  '覚えるそばから忘れてしまう。',

  // Sentences with か in different contexts
  '来るか来ないか分からない。', // Simple question, not the pattern
  '行くかどうか迷っている。', // Question with どうか

  // かのうちに without negative form
  '雨が降るかのうちに、傘を持って出かけた。', // Missing negative form
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
