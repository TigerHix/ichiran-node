import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './こととて.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // こと alone (without とて) - "thing, matter"
  '大切なことを忘れた。',
  'それはいいことだ。',
  '彼のことが心配だ。',

  // だから (without こと) - "therefore, so"
  '疲れたから、寝ます。',
  '彼は来ないだろうから、始めよう。',

  // ことだから (given that, more predictive) - different grammar
  '田中さんのことだから、今日も遅れてくるだろう。',
  '真面目な田中さんのことだから、約束は守るだろう。',

  // ことだし (and so, more conversational) - different grammar
  '雨も降っていることだし、今日は家にいよう。',
  '近いんだし、時々遊びに来てください。',

  // として (toshite) - "as, in the capacity of"
  '彼は友達として大事な存在だ。',
  '社長として発表します。',

  // でもあって (demo atte) - different structure
  '彼は医者でもあって、作家でもある。',

  // Noun + で (instrumental/particle) + こと
  '鉛筆で書くこと。',
  '日本語で話すこと。',

  // Similar sounding but unrelated patterns
  // こととして (kotoshite) - "as a matter, as a thing"
  'これは例外として扱う。',
  '個人的なこととして秘密にする。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
