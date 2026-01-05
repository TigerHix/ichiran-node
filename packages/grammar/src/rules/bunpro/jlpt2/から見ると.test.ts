import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './から見ると.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: sentences that should NOT match
const negatives = [
  // Simple 見る (to see/watch/look) - without から perspective marker
  'テレビを見ると時間が経つ。',
  '映画を見ましたか。',
  '景色を見るのが好きです。',
  '彼を見ると笑ってしまう。',
  '子供の頃、よく漫画を見ました。',

  // から alone (without 見る) - "from" or "because"
  '東京から大阪へ行く。',
  '彼からメールをもらった。',
  '雨だから行かない。',
  '8時から始まります。',

  // からして (karashite) - more subjective/emphatic judgment
  '彼の態度からして、信用できない。',
  'この店は名前からして高そうだ。',
  '親からして反対している。',

  // からすると・からすれば (karasuruto/karasureba) - more objective judgment using する
  '状況からすると、失敗は避けられない。',
  '彼の表情からすれば、試験は上手くいったようだ。',
  '今の結果からすれば、成功は難しいだろう。',

  // からいうと (karaiuto) - "speaking in terms of"
  '私の経験からいうと、そんなことはない。',
  '安全の点からいうと、この車が優れている。',

  // からいって・からいったら (karaiutte/karaittara) - "if I were to say from"
  '彼の性格からいって、断るだろう。',
  '今の状況からいったら、無理だ。',

  // からいえば (karaieba) - "if one were to say from"
  '健康からいえば、運動は大切だ。',

  // にしたら (nishitara) - "from the standpoint of" (more subjective)
  '私にしたら、それは不公平だ。',
  '子供にしたら、それは大問題だ。',

  // にしてみれば (nishitemireba) - "from X's perspective" (subjective)
  '親にしてみれば、心配なのは当然だ。',
  '彼にしてみれば、それは侮辱だ。',

  // にしては (nishite) - "considering, for"
  '子供にしては背が高い。',
  '新人にしてはよくやっている。',

  // Simple て-form of 見る (mite) - without から perspective
  'テレビを見て勉強します。',
  '映画を見て感動しました。',
  '景色を見て楽しんでいる。',

  // 見る (miru) in compound contexts (not perspective-taking)
  '見る見るうちに大きくなった。',
  '見るからに元気そうだ。',

  // 見た (mita) as past tense - not conditional perspective
  '昨日映画を見ました。',
  '彼を見たことがない。',

  // 見れば (mireba) as simple conditional - not perspective-taking
  'よく見れば、それは偽物だ。',
  '見れば見るほど好きになる。',

  // てから (tekara) - "after doing"
  '食事をしてから出かけます。',
  '日本に来てから、忙しい。',

  // に見えて (ni miete) - "looks like, appears to be"
  '彼は若く見える。',
  'この絵は本物に見える。',
  '子供に見えるが、実は大人だ。',

  // ところを見る (tokoro o miru) - "judging from the situation"
  '彼の様子を見ると、疲れているようだ。',
  'この結果を見ると、成功したらしい。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
