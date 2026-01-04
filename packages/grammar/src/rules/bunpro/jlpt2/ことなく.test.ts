import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことなく.js';
import { BUNPRO_JLPT2 } from './index.js';

const positives = [
  // From test data - standard examples
  '犯人が捕まることなく１０年が経つ。',
  '遅刻することなく、職場に着いた。',
  '彼は社長に何も言うことなく会社を辞めた。',
  '怪我人を出すことなく、人質を全員救出する事ができたそうです。',
  '真実を知ることなく終わる。',
  '遅れることなく到着した。',
  '何ごとも理解されることなく終わる。',
  '彼の長所を知ることなく、別れの時がきてしまった。',
  'この洗剤を使えば、セーターが縮むことなく洗えるからおすすめですよ。',
  '貴乃花は一門に属することなく引退してしまった。',
  'あの選手は、準備体操をすることなく試合に出るらしい。',
  '挨拶をしたのに、友人は立ち止まることなく行ってしまった。',
  'ネットショップなら、直接話すことなく注文できて楽だ。',
  '耕すことなく種を植えても、うまくいくはずがない。',
  '対立することなく話し合いを終えることができました。',
  'うちの子には、散らかすことなく遊ぶという特技があります。',

  // Additional examples
  '彼は休むことなく働き続けた。',
  '誰にも知らせることなく出発した。',
];

const negatives = [
  // ないで (informal alternative)
  '彼は休まずに働き続けた。',
  '誰にも知らせずに出発した。',

  // ずに (informal alternative)
  '休まずに働く。',
  '言わずに帰る。',

  // こと used in other contexts
  'それは良いことだ。',
  '彼のことが好きだ。',

  // なく used in other contexts
  'お金がなくて困っている。',
  '雨が降らなくていいですね。',

  // Separate clauses with こと and なく
  '良いことを知らない。彼は知らない。',

  // ことなく followed by sentence-ending particle (not valid)
  // Actually this might be valid in some contexts, but let's ensure we don't over-match
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { positives, negatives });
});
