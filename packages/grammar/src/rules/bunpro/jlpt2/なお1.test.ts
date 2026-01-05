import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なお1.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the なお1 grammar rule
const negatives = [
  // まだ (mada) - "still" (less formal, everyday usage)
  'まだ雨が降っている。',
  '彼はまだ来ていない。',
  '朝ごはんはまだ食べていない。',

  // いまでも (ima demo) - "even now" (less formal)
  'いまでも彼のことを覚えている。',
  'その習慣はいまでも残っている。',

  // いまだに (imada ni) - "still yet" (emphasizes unexpectedness)
  'いまだに真相は不明だ。',
  'いまだに問題は解決していない。',

  // Similar adverbs with "na" but different meanings
  // なぜ (naze) - "why"
  'なぜ彼は来なかったのか。',

  // なに (nani) - "what"
  'なにをしているのですか。',

  // なのか (nanoka) - "the 7th day of month"
  '今日は7月7なのか。',

  // なの (nano) - sentence-final particle
  '彼は来ないの？',

  // なのか (nano ka) - "is it that...?" (sentence-final question)
  '彼は来るのか。',

  // なのに (nanoni) - "despite, even though"
  '勉強したのに、テストが悪かった。',

  // なるほど (naruhodo) - "indeed, certainly"
  'なるほど、そうでしたか。',

  // 直す (naosu) - "to fix, correct" (verb)
  '間違いを直す。',
  '壊れた椅子を直す。',

  // 治る (naoru) - "to be cured, heal" (verb)
  '風邪が治る。',
  '怪我が治る。',

  // Sentences with "nao" as part of larger words (not standalone なお)
  'なおかつ、安くて美味しい。',  // なおかつ (naokatsu) - "and at the same time"
  '詳細はなおさらのことだ。',    // なおさら (naosara) - "even more"
  'なお且つ確認が必要だ。',      // なお且つ (naokatsu) - "and also"

  // Note: なおも (nao mo) "still more" contains なお and will match this rule.
  // This is intentional - なおも is a compound of なお + も, and the grammar
  // point teaches the word なお itself in all its contexts.
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
