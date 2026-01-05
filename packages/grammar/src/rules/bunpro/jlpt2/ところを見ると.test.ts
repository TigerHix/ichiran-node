import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ところを見ると.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ところを見ると grammar rule
const negatives = [
  // からして (karashite) - "judging from" (follows nouns)
  '彼の性格からして、彼と一緒に住むことは無理だろう。',
  'このゲームは名前からしてつまらなそうだ。',

  // からすると・からすれば (karasuruto/karasureba) - "judging from" (follows nouns)
  '彼の話からすると、嘘をついているようだ。',
  'この結果からすれば、成功は難しいだろう。',

  // にしては (nishite) - "considering, for"
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',
  'この製品は安いにしては品質が良い。',

  // からみて (karamite) - "judging from the perspective of"
  '私の立場から見て、それは間違いだ。',
  '彼の表情から見て、満足しているようだ。',

  // ところを (tokoro o) - "just as/when" (different meaning)
  // Example used for "caught in the act" or "at the moment"
  '彼は出かけるところを彼女に呼び止められた。',
  '私が食事をしているところを写真に撮られた。',

  // を見て (o mite) - "looking at" (without conditional と)
  '彼の顔を見て、笑った。',
  'この絵を見て、感動した。',

  // を見ると without ところ (different grammar)
  'この写真を見ると、昔を思い出す。',
  '彼を見ると、父を思い出す。',

  // 見る alone - "to see/look"
  '映画を見る。',
  '彼を見る。',
  '美術館を見る。',

  // ところ alone - "place" or "situation"
  'ここはいいところだ。',
  'そんなところに行きたくない。',
  '場所を変えるところだ。',

  // Similar grammar patterns with different structures
  // ところが (tokoro ga) - "however" (conjunctive)
  '頑張ったところが、失敗した。',

  // ところで (tokoro de) - "by the way" or "even if"
  'それについてところで、どう思いますか？',
  '今さら謝ったところで、無駄だ。',

  // Just と (to) - conditional particle
  '雨が降ると、行きません。',
  '春になると、桜が咲く。',

  // を (o) - object marker alone
  '本を読む。',
  'ご飯を食べる。',

  // Verb + て-form + いる (progressive form without ところを見ると)
  '彼が勉強している。',
  '彼女が眠ている。',
  '子供が遊んでいる。',

  // Noun + を見る (noun + o miru) without ところ
  '写真を見る。',
  '彼を見る。',
  '風景を見る。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
