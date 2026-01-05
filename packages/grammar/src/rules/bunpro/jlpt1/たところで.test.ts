import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たところで.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Similar patterns that should NOT match
  // たとたんに (as soon as - different grammar)
  '家に帰ったとたんに電話が鳴った。',
  'ドアを開けたとたんに猫が逃げ出した。',

  // たばかり (just did - different grammar)
  '日本に来たばかりで、まだ友達がいない。',
  'この服を買ったばかりです。',

  // たあとで (after doing - different grammar)
  '晩ご飯を食べた後で、散歩に行きました。',
  '仕事が終わった後で、飲みに行きませんか。',

  // た以来 (since doing - different grammar)
  '彼とは会って以来、連絡を取っていない。',
  '結婚して以来、故郷には帰っていない。',

  // Simple instrumental で (not たところで)
  '電車で行ったほうが早いです。',
  '日本語で話してください。',

  // て-form + ところで (different meaning)
  '今勉強しているところで、あとで電話します。',

  // Simple た-form + で (instrumental usage)
  '鉛筆で書いた。',
  '筆で描いた。',

  // Noun + ところで (different grammar)
  '大事なところで間違えた。',
  'ここは良いところで写真を撮ろう。',

  // た + ところ as noun phrase (not たところで)
  '食べたところはとてもおいしかった。',
  '行ったところは静かだった。',

  // た + ものの (although - different grammar)
  '頑張ったものの、結果は良くなかった。',
  '謝ったものの、許してもらえなかった。',

  // た + が (but - different grammar)
  '買ったが、使わなかった。',
  '行ったが、会えなかった。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
