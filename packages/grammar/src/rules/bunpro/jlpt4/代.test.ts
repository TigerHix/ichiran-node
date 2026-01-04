import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './代.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 代替 - substitute/replacement (different word)
  'この薬は水の代わりに使えます。',
  '彼は社長の代わりをします。',
  // 時代 - era/period (different grammar structure)
  'この時代は重要です。',
  '江戸時代は文化が栄えた。',
  // 现代 - modern times (different word)
  '现代の社会は複雑だ。',
  // Other uses of 代 not as suffix
  '代金を払ってください。',
  '次の代のために頑張る。',
  // Numbers without 代 suffix
  '40歳の人はここに来てください。',
  '1980年の音楽が好きです。',
  // かた - person (polite form)
  '60歳の方はこちらに並んでください。',
];

// GiNZA parsing limitation:
//
// Sentence with special Unicode minus sign (U+2212) instead of regular hyphen:
//   "アメリカ映画の誕生から−１９８０ねんだいまで"
//
// The special minus sign (−) causes GiNZA to tokenize the sequence differently
// compared to regular hyphen (-). Other sentences with the same pattern
// ("ねんだい") parse correctly and match:
//   "１９８０ねんだいころから..." ✓ WORKS
//   "昭和５０ねんだいの終わり..." ✓ WORKS
//
// CONCLUSION: Special Unicode character causes inconsistent tokenization. GiNZA limitation.
const skipPositives = [
  '映画博物館のサイトのカテゴリ：「アメリカ映画の誕生から−１９８０ねんだいまで」',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
