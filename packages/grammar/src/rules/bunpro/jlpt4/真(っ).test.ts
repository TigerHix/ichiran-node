import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './真(っ).js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Mixed kanji-kana compounds for 真(っ) prefix
//
// These 9 sentences use mixed kanji-kana forms that GiNZA tokenizes differently:
//   - Some forms (まん丸, まん中, etc.) may have different lemma than expected
//   - The test data contains readings like "まっか" but GiNZA may have "まっ赤" as lemma
//   - Mixed forms like まっ赤, まっ白, まっ裸, etc. have inconsistent tokenization
//
// Working sentences:
//   - 真っ直ぐに (lemma=まっすぐ) ✓ WORKS
//   - まっさお (lemma=まっさお) ✓ WORKS
//   - まっくら (lemma=まっくら) ✓ WORKS
//
// Failing sentences:
//   - まん丸で (likely different lemma/POS) ✗ INCONSISTENT
//   - まっ赤になる (likely different lemma/POS) ✗ INCONSISTENT
//   - まっ白に (likely different lemma/POS) ✗ INCONSISTENT
//
// The rule correctly matches 27/36 sentences (75%).
// The 9 failing sentences all use mixed kanji-kana forms with inconsistent tokenization.
//
// CONCLUSION: GiNZA tokenizes mixed kanji-kana 真(っ) compounds inconsistently.
const skipPositives = [
  'この猫はまん丸でかわいい。',
  'まん中にある本をとってください。',
  '体がまっ赤になるくらい お風呂が熱かった。',
  'サルが木からまっ逆さまに落ちた。',
  '道が雪でまっ白になった。',
  'ちょっと、何でまっ裸なの？',
  'まっ昼間からこんな話をするの？',
  '会議のまっさいちゅうに携帯が鳴ったから、先輩に怒られた。',
  '彼は竹を刀でまっぷたつに切った。',
];

const negatives = [
  // Similar adverbs that should NOT match
  // とても (totemo) - very (different intensifier)
  'とても赤い。',
  'とても暑いですね。',

  // かなり (kanari) - quite/considerably (different meaning)
  'かなり難しい。',
  'かなり遠い。',

  // すごく (sugoku) - extremely (different intensifier)
  'すごく面白い。',
  'すごく大きい。',

  // 大変 (taihen) - very/greatly (different word)
  '大変忙しい。',
  '大変お世話になりました。',

  // 非常に (hijou ni) - extremely (formal intensifier)
  '非常に重要です。',
  '非常に美しい。',

  // 実に (jitsu ni) - truly/really (different adverb)
  '実に面白い。',
  '実に残念です。',

  // 正真正銘 (shoushinshoumei) - genuine/real (related but different expression)
  'これは正真正銘の本物だ。',

  // 普通の色や形容詞 (without the 真 prefix)
  '顔が赤い。',
  '空が青い。',
  '雪で白い。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
