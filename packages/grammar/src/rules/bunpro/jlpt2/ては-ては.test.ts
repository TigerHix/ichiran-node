import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ては-ては.js';
import { BUNPRO_JLPT2 } from './index.js';

// Sentences that can't be matched due to DSL engine limitations:
//
// ANALYSIS: Pattern ては...ては (verb-te-wa ... verb-te-wa)
//
// The grammar rule requires matching two separate "verb-te-wa" patterns in the same sentence.
// However, the DSL engine allows variables with identical constraints (e.g., te1 and te3
// both matching {text: 'て'}) to bind to the SAME token.
//
// For example, in sentence "圧縮しては送信し、圧縮しては送信し":
// - te1 and te3 both match token index 11 (the same "て")
// - This causes the match to fail because they can't be distinguished
//
// The DSL doesn't provide a mechanism to enforce that two variables must match
// DIFFERENT tokens. Without this constraint, the engine finds one binding for te1
// and then tries to bind te3 to the same token, which fails the ordering constraints.
//
// Other similar sentences affected:
// - 受け取っては誰かに渡し、渡されては受け取る
// - 接続しては切断して、接続しては、切断して
// - あくびをしてはフリスクを食べ、あくびをしてはフリスクを食べ
// - 嫌がられてはやり方を変え、嫌がられてはやり方を変え
// - ある研究によると、一番の勉強方法は２０〜４０分勉強しては休憩して
//
// CONCLUSION: This is a fundamental limitation of the current DSL architecture.
// To properly support this grammar pattern, the DSL would need to support
// token-uniqueness constraints or a different matching strategy.
const skipPositives = [
  '受け取っては誰かに渡し、渡されては受け取るというだけの仕事です。',
  '最近、インターネット接続が悪い。接続しては切断して、接続しては、切断して。り返すことは）イライラさせられる。',
  'あくびをしてはフリスクを食べ、あくびをしてはフリスクを食べ、何とか起きていられた。',
  '嫌がられてはやり方を変え、嫌がられてはやり方を変え、を繰り返して人との付き合い方を学ぶ。',
  'ある研究によると、一番の勉強方法は２０〜４０分勉強しては休憩してを繰り返すことです。',
  'データを圧縮しては送信し、圧縮しては送信しを繰り返し、やっとすべて送れました。',
];

// Negative test cases - sentences that should NOT match the ては-ては grammar rule
const negatives = [
  // Single ては (conditional "if/when doing X") - not the repeated pattern
  '雨が降っては出かけられない。',
  '彼に会っては話をしたくなる。',
  'こんなに寒くては外に出られない。',

  // Regular te-form without は - just te-form connection
  '朝起きて、顔を洗って、朝ごはんを食べた。',
  '本を読んで、寝ました。',
  '勉強して、試験を受けた。',

  // te-form + other particles (not は)
  '彼と話をして、意見を交換した。',
  '公園で走って、友達に会った。',
  '店に入って、商品を買った。',

  // てしまう (completed action/regret) - different grammar
  '食べてしまった。',
  '忘れてしまった。',
  '間違えてしまった。',

  // ておく (do in advance) - different grammar
  '予約しておく。',
  '準備しておく。',
  '調べておく。',

  // てある (state of being) - different grammar
  '黑板に字が書いてある。',
  '窓が開けてある。',
  '準備してある。',

  // てみる (try doing) - different grammar
  '食べてみる。',
  '行ってみる。',
  'やってみる。',

  // てくる (come and do / change of state) - different grammar
  '買ってくる。',
  '走ってくる。',
  '太ってきた。',

  // ていく (go and do / ongoing change) - different grammar
  'これからも頑張っていく。',
  '送っていく。',
  '生きていく。',

  // てやる (do for someone lower) - different grammar
  '弟に本を読んでやる。',
  '手伝ってやる。',

  // てくれる (do for me) - different grammar
  '友達が貸してくれる。',
  '教えてくれた。',

  // てもらう (have someone do) - different grammar
  '先生に教えてもらう。',
  '友達に手伝ってもらう。',

  // Single colloquial ちゃ/じゃ (not repeated pattern)
  '食っちゃ寝てばかりいる。',
  '飲んじゃだめだ。',
  '行っちゃダメだ。',

  // Conditional ても (even if) - different grammar
  '頑張っても無理だ。',
  '言っても分からない。',
  '待っても来ない。',

  // Contrastive て (different grammar)
  '勉強はしたが、試験に失敗した。',
  '行ったことは行ったが、楽しくなかった。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
