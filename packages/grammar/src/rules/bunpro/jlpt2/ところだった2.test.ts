import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ところだった2.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ところだった2 grammar rule
const negatives = [
  // たところだ (JLPT4) - "just finished doing" (different tense/aspect)
  // Uses verb ta-form + ところ, not dictionary form + ところ
  '今帰ったところです。',
  '食べたところでケーキを出された。',
  '仕事は今終わったところ。',
  '来たところだ。',
  '買ったところです。',

  // るところだ (JLPT4) - "about to do now" (present tense, not past)
  // Uses present copula だ/です, not past だった/でした
  '今出発するところだ。',
  '帰るところです。',
  '行くところだ。',
  '食べるところ。',

  // ところ as a regular noun meaning "place"
  'いいところを見つけた。',
  'ここは静かなところです。',
  '暑いところに行きたくない。',
  '気持ちのいいところだ。',

  // にする (to decide on/choose) - completely different grammar
  'これにする。',
  'どれにしますか。',
  '注文を決定にする。',

  // だった (past copula) without ところ
  '学生だった。',
  '彼は先生だった。',
  '昨日は晴れだった。',
  '昔は東京だった。',

  // Verb + ところ + present tense (not past)
  '教わるところだ。',
  '出掛けるところです。',
  'するところだ。',

  // Other ところ grammar patterns
  // ところで (transition topic)
  'ところで、明日の天気はどうですか。',
  'ところで、彼は来ましたか。',

  // ところが (conjunctive - "however")
  'やってみたところが、意外と簡単だった。',
  '店に行ったところが、閉まっていた。',

  // どころではない (far from, no way)
  '冗談どころではない。',
  '泣きっ面に蜂どころではない。',
  '試合どころではない。',

  // どころか (let alone, far from)
  '英語どころか、平仮名も読めない。',
  '歩くどころか、走れなかった。',

  // ところまで (up to the point that)
  'そこまでする必要はない。',
  'ここまできたら引き返せない。',

  // だけに (only, precisely because) - different particle
  '安いだけに売れている。',
  '子供だけに可能だ。',

  // ばかりだった (just did, was just doing) - different grammar
  '買ったばかりだ。',
  '来たばかりです。',
  '作ったばかりの料理。',

  // Verb + に + だった (different structure)
  '東京に行った。',
  '家に帰った。',

  // Similar but unrelated patterns with tokoro
  '悪いところはない。',
  'いいところだと思った。',
  'どこかで見たことがあるところだ。',

  // Verb + た + だ + ところ (wrong order)
  '帰っただというところです。',
  '行っただのところです。',

  // ていた + ところ + present tense
  '食べていたところだ。',
  'やっていたところです。',
  '勉強していたところ。',

  // Negative form + ところ (different meaning)
  '知らないところだ。',
  '行かないところがある。',
  'しないところです。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
