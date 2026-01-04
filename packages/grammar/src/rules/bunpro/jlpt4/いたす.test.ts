import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いたす.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that can't be matched due to grammar rules:
//
// The sentences with 拝 kanji (拝借して, 拝見して):
//
// According to the Bunpro caution for いたす:
// "Words that include the kanji 拝 'to worship' do not usually require お, ご, or いたす.
//  This is due to the word itself being 'respectful'."
//
// These humble compound verbs use する (not いたす):
// - 拝借する (humbly borrow) - uses 拝借して, not 拝借いたして
// - 拝見する (humbly look) - uses 拝見して, not 拝見いたして
//
// These should NOT match the いたす rule since they use regular する.
const skipPositives = [
  'はいしゃくしてもよろしいですか。', // 拝借 (humble) uses する, not いたす
  'まだその映画をはいけんしていないので、一緒に見に行きませんか。', // 拝見 (humble) uses する, not いたす
];

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // Regular する (not humble form)
  '勉強する。',
  '掃除する。',
  '勉強します。',
  '掃除します。',

  // Honorific お〜になる (elevating others, not humble self)
  '社長がお話しになります。',
  '先生がお行きになります。',

  // Honorific なさる (honorific form, not humble)
  '社長がなさいます。',
  '先生が勉強なさいます。',

  // Already-humble verbs with 拝 kanji using regular する
  '拝見する。',
  '拝借する。',

  // Casual verbs (not humble)
  '待つ。',
  '知らせる。',
  '送る。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
