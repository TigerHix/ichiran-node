import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './としても.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the としても grammar rule
const negatives = [
  // として (toshite) - "as, in the capacity of" (non-emphatic)
  '彼は医者として働いています。',
  '友達としてアドバイスします。',
  '趣味として音楽を楽しんでいます。',
  'これは文化遺産として重要だ。',

  // として + affirmative statement (not concessive)
  // These use として in its basic "as" meaning, not the emphatic "even as"
  '先生として指導します。',
  '代表として出席します。',

  // にしても (nishitemo) - "even if" (different particle, more subjective judgment)
  '彼が来るにしても、遅れるだろう。',
  '雨にしても行きます。',
  '安いにしても品質が良い。',
  '忙しいにしても電話くらいできたでしょ。',

  // としたら (toshitara) - "if we were to assume" (conditional, not concessive)
  '行くとしたらいつがいいですか。',
  'できるとしたらお金がかかります。',
  '買うとしたらどれがいいですか。',

  // とする (tosuru) - "assume, suppose" (without も)
  'AはBだとする。',
  'それが真実だとする。',
  '仮に正しいとする。',

  // して (shite) alone - te-form of する
  '勉強してください。',
  '掃除して、寝ました。',
  'ご飯を作って、食べました。',

  // も alone - emphatic particle
  '私も行きます。',
  'これも好きです。',
  '彼も来るでしょう。',

  // Similar sounding but unrelated patterns
  // として (toshite) followed by different particles
  'としては勉強が必要だ。',
  'としてもらった。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
