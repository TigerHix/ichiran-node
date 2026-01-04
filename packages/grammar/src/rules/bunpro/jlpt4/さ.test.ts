import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さ.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // こと nominalization (general concept, not degree)
  '美しいことは大事です。',
  '高いことは悪いことではない。',
  // Adjective not converted to noun (using dictionary form)
  'この山は高いです。',
  '彼は元気です。',
  'この部屋は綺麗だ。',
  // さ as part of a different word (not adjective suffix)
  // (Need to ensure we don't false match on さ in other contexts)
];

// GiNZA parsing limitation:
//
// Some common さ-nominalized words are recognized by GiNZA as dictionary entries
// and parsed as single NOUN tokens (not split into adjective stem + さ suffix).
//
// Examples:
//   長さ (length) - single token: text="長さ" pos=INTJ tag=名詞-普通名詞-一般
//   おいしさ (deliciousness) - single token: text="おいしさ" pos=ADJ tag=名詞-普通名詞-一般
//
// In contrast, less common さ-words ARE split and can be matched:
//   大切さ - split: 大切 (ADJ) + さ (PART, tag=接尾辞-名詞的-一般)
//   甘さ - split: 甘 (ADJ) + さ (PART, tag=接尾辞-名詞的-一般)
//   美しさ - split: 美し (ADJ) + さ (PART, tag=接尾辞-名詞的-一般)
//
// The discriminator `tag=接尾辞-名詞的-一般` identifies さ as a suffix.
// But when GiNZA parses a word as a single token, it loses the suffix tag.
// Single-token nouns like 長さ have tag=名詞-普通名詞-一般 (not 接尾辞-名詞的-一般).
//
// Matching all nouns ending in さ would overcapture:
//   ❌ 暇 (hima - free time, unrelated word)
//   ❌ 喫茶店 (kissaten - coffee shop, unrelated word)
//   ❌ Various proper nouns and common words ending in さ
//
// CONCLUSION: No reliable discriminator for single-token cases. GiNZA limitation.
const skipPositives = [
  '私が道の長さを測ります。',
  '昨日食べたラーメンのおいしさを友達に教えたい。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
