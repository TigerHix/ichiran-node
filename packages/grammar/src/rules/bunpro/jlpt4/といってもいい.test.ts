import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './といってもいい.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: similar patterns that should NOT match
const negatives = [
  // Simple quotation と (just marking quoted content, not the full pattern)
  // These use と but don't have the full いい + いう + ても structure
  '彼は行くと言った。',
  '明日は雨だと言われている。',
  'こんにちはと言いました。',

  // という (called/named - different grammar)
  'これは何という花ですか。',
  '田中という人から電話がありました。',
  '太郎という名前の犬です。',

  // ということだ (it means that / I hear that - different structure)
  '先生によると、この病気は薬では治せないということだ。',
  '来月日本に行くことになったということです。',

  // といえる (can be said - stronger assertion, different grammar)
  '彼は天才だといえる。',
  'これは最高の結果だといえます。',

  // といえば (speaking of / if I say - conditional topic marker)
  '日本といえば、桜を思い出します。',
  '彼といえば、最近連絡がない。',

  // といって (saying that + continue - different structure)
  '彼は疲れたといって、座り込んだ。',
  '高いといって、買わなかった。',

  // て (te-form) + も + いい (permissive - different context)
  'ここで座ってもいいですか。',
  '入ってもいいと言われた。',

  // て-form + いい (it would be good if - different meaning)
  'もっと安くていい。',
  '明日の天気は晴れていいですね。',

  // Simple conditional と (when/if - not quotation)
  '春になると花が咲く。',
  '家に帰ると母が料理をしている。',

  // と (with/accompaniment particle)
  '友達と映画を見に行った。',
  '彼と話したい。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
