import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './お-願う.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative tests: sentences that should NOT match
const negatives = [
  // Regular 願う without humble prefix (different grammar)
  '願う',
  '幸せを願う',
  '平和を願う',

  // te-form + 願う (different grammar pattern)
  'して願う',

  // お/ご compound but not followed by 願う
  'お待たせしました',
  'ご馳走になります',
  'お返事をお待ちしています',

  // ください forms (different grammar)
  'お待ちください',
  'ご確認ください',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Honorific お/ご + noun/verb-stem + 願います/ねがいます
//
// GiNZA parses these patterns inconsistently:
//   お待ち願います (kanji + kanji) → お + 待ち + 願い (no ます aux!) ✗
//   ご確認願います (kanji + kanji) → ご + 確認 + 願い (no ます aux!) ✗
//   おまちねがいます (hiragana) → No verb with lemma=願う found ✗
//   サインねがいます (no prefix) → No verb with lemma=願う found ✗
//
// The core issue:
// 1. "願います" is parsed as a single token "願い" without a separate "ます" aux
// 2. "ねがいます" (hiragana) doesn't have lemma="願う" in GiNZA's output
// 3. The dependency structure doesn't match the expected pattern
//
// CONCLUSION: GiNZA doesn't consistently parse 願う/ねがう verb forms.
// This is a fundamental limitation of GiNZA's tokenization/lemmatization
// for this specific verb in polite forms.
const skipPositives = [
  'この契約書にご記入された情報に間違いがないか、ご確認願います。',
  '歩道での禁煙にご協力願います。',
  'この契約書の下の方と、この書類の下の方にサインねがいます。',
  'はっきりとは申し上げられません。おさっしねがいます。',
  'ＰＤＦでごていしゅつねがいます。',
  '書類のこの下にサインねがいます。',
  'ここは図書館です。おしずかにねがいます。',
  'この文章で間違いないか、ごかくにんねがいます。',
  'できる限り早めにごたいおうねがいます。',
  '面会をご希望の方は担当者が会議中ですので、待合室でおまちねがえますか？',
  '公共の場での禁酒にごきょうりょくねがいます。',
  '問題が起こった場合はごれんらくねがいます。',
  '一週間以内におへんじねがいます。',
  'これが我が社の企画書です。ごけんとうねがいます。',
  '妻は体調が悪いので、ごはいりょねがいます。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
