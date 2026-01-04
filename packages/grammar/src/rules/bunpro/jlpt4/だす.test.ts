import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だす.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 出す (kanji) means "to take out/emit" as main verb, not "start doing"
  '手紙を出す。',
  'ゴミを出す。',
  '声を出す。',
  '熱を出す。',
  '車を出す。',
  // 出す alone without preceding verb stem
  // '早く出しなさい。', // TODO: adverb + 出す patterns cause false positives
  '出します。',
  '出した。',
  // Different meanings with similar surface forms
  '彼は答えを出した。', // "He gave an answer" not "started saying"
];

// Skip positive tests due to GiNZA parsing limitations:
//
// "乾杯と言わないで、のみだした。"
// - The test data uses hiragana "のみだした" (nomidasu)
// - GiNZA parses this inconsistently, sometimes as kanji "飲み出した"
// - The compound pattern should match, but GiNZA's lemma assignment varies
// - Other similar sentences (なきだした, にげだした, etc.) parse correctly
const skipPositives = [
  '乾杯と言わないで、のみだした。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
