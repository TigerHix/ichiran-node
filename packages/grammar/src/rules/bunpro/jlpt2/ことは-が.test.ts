import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことは-が.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ことは-が grammar rule
const negatives = [
  // Simple が/けど conjunction (without the ことは pattern)
  '漢字は読めますが、簡単な漢字しか読めないです。',
  '新しい家は広いけど、家具が多いから狭く見える。',
  '雨に濡れたが、大したことはなかった。',
  '行ったけど、東京の空港を見ただけだよ。',

  // こと alone (without the ことは...X pattern)
  '大切なことを忘れた。',
  '何か言いたいことはありますか。',
  'それはいいことだ。',
  '彼のことが心配だ。',

  // ことは without repetition (not this grammar)
  '読むことは簡単です。',
  '勉強することは大事です。',
  '早起きすることは健康にいいです。',

  // Similar patterns with different meanings
  // ことだから (koto dakara) - "it is exactly because"
  '田中さんのことだから、今日も遅れてくるだろう。',
  '子供のことだから、仕方がない。',

  // ことになっている (koto ni natte iru) - "it is arranged that"
  '授業は9時に始まることになっている。',
  'ここに駐車してはいけないことになっている。',

  // ことにする (koto ni suru) - "decide to"
  '毎日運動することにした。',
  '明日出発することにしよう。',

  // ことになる (koto ni naru) - "it has been decided that"
  '来月日本に行くことになった。',
  '会議は明日開かれることになっている。',

  // Noun + は + Noun + が (different structure without こと)
  '彼は学生ですが、よくサボります。',
  'これは本ですが、面白くないです。',

  // Simple topic + contrast (not the repetitive pattern)
  '彼は来ませんでしたが、会議は始まりました。',
  '天気は悪いですが、出かけます。',

  // ということだ (to iu koto da) - "I heard that"
  '彼は来ないということだ。',
  '明日は雨だということです。',

  // ことか (kotoka) - emphatic expression
  'どれほど待ち望んだことか。',
  'どれだけ苦労したことか。',

  // さえ〜ば (sae...eba) - conditional
  'あなたさえいれば、大丈夫です。',
  '練習さえすれば、上手になります。',

  // とは限らない (to wa kagiranai) - "not necessarily"
  '高いからといって、いいとは限らない。',
  '有名だからといって、実力があるとは限らない。',

  // わりには (wari ni wa) - "considering, for"
  'この仕事は簡単なわりに給料がいい。',
  '彼は年齢のわりに若く見える。',

  // にしては (ni shite wa) - "considering"
  '子供にしては、よくやっている。',
  '新人にしては、実力がある。',

  // ものの (mono no) - "although"
  '承知したものの、少し不安だ。',
  '買ったものの、まだ使っていない。',

  // くせに (kuse ni) - "despite, even though" (critical)
  '彼はお金があるくせに、出さない。',
  '知っているくせに、教えてくれない。',

  // わりに (wari ni) - "proportionately"
  'この製品は安いわりに性能がいい。',
  '彼は小柄なわりに力が強い。',

  // ものだから (monodakara) - "because, since" (explanatory)
  '疲れていたものだから、早く寝てしまった。',
  '遅れたものだから、申し訳ありません。',

  // せいで (sei de) - "because of (negative result)"
  '雨のせいで、試合が中止になった。',
  '彼が遅れたせいで、バスに乗り遅れた。',

  // おかげで (okage de) - "thanks to" (positive)
  'あなたのおかげで、成功できました。',
  '先生のおかげで、合格できました。',

  // あげく (ageku) - "after, in the end" (often negative)
  'さんざん迷ったあげく、結局行かなかった。',
  '喧嘩したあげく、別れることになった。',

  // すら (sura) - "even"
  '子供ですら知っている。',
  '親ですら反対している。',

  // だけに (dake ni) - "precisely because"
  '試験だけに緊張している。',
  '重要な会議だけに、慎重に進めたい。',

  // ばかりに (bakari ni) - "just because, only because"
  '油断したばかりに、事故に遭った。',
  'うそをついたばかりに、信用を失った。',

  // 反之 (not applicable to Japanese - different language pattern)

  // とは (topic marker with explanation)
  '地震とは何ですか。',
  '結婚とはどういう意味ですか。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
