import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './-よう--う-まい-が-と.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Simple volitional without the negative volitional counterpart
  '行こうが行かないが。',
  '読もうと思います。',
  'しようがしまいが、やりたい。',
  // が particle as subject marker, not conjunction
  '雨が降る。',
  '私が行きます。',
  // Different grammar patterns with similar elements
  '行こうと思ったが、行けなかった。',
  '読みたいが、時間がない。',
  // まい without volitional
  '行くまいと思う。',
  'するまいと決めた。',
];

// Sentences from test data that are incomplete (only show volitional part, not full pattern)
// These are cloze sentences where the student fills in the blank, so the full
// volitional + まい pattern isn't shown in the sentence.
const skipPositives = [
  // Only shows volitional without the まい counterpart
  '日本語能力試験のために沢山の参考書をかおうと、それを実際に使いこなさないなら意味が無い。',
  '人が何といおうが、ずっとあなたを愛しています！',
  '何年がかかろうと、ドラゴンボールを見つけるつもりだ！',
  '誰に反対されようが、僕は同性婚を支持する気持ちを変えないつもりだ。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives, skipPositives });
});
