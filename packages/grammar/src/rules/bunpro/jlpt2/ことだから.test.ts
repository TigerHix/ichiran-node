import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことだから.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ことだから grammar rule
const negatives = [
  // こと alone (without だから) - "thing, matter"
  '大切なことを忘れた。',
  '何か言いたいことはありますか。',
  'それはいいことだ。',
  '彼のことが心配だ。',

  // だから alone (without こと) - "therefore, so"
  '疲れたから、寝ます。',
  '明日は雨だから、傘を持っていきます。',
  '彼は来ないだろうから、始めよう。',

  // ものだから (monodakara) - different grammar (more subjective/excusatory)
  // Note: This is a related but different grammar point

  // ことから (kotokara) - "from the fact that" (more objective)
  '彼が来なかったことから、何かあったのだろう。',
  '足跡から判断して、熊だったと思われる。',
  '彼の表情からすると、試験は悪くなかったようだ。',

  // からこそ (karakoso) - "precisely because" (more emphatic)
  'あなただからこそ、言ったのです。',
  '努力したからこそ、成功できた。',

  // にしては (nishite) - "considering, for"
  '子供にしては、よくやっている。',
  '新人にしては、実力がある。',

  // ことにする (kotonisuru) - "decide to"
  '毎日運動することにした。',
  '明日出発することにしよう。',

  // ことになる (kotoninaru) - "it has been decided that"
  '来月日本に行くことになった。',
  '会議は明日開かれることになっている。',

  // ことになっている (kotoniatteiru) - "it is arranged that"
  '授業は9時に始まることになっている。',
  'ここに駐車してはいけないことになっている。',

  // ことか (kotoka) - emphatic expression
  'どれほど待ち望んだことか。',
  'どれだけ苦労したことか。',

  // ことさ (kotosa) - informal explanatory
  'どうでもいいことさ。',
  '気にしないことさ。',

  // ことなしに (kotonashini) - "without"
  '努力することなしに成功はできない。',
  '彼の許可することなしに、入室できない。',

  // Similar sounding but unrelated patterns
  // がして (gashite) - different particle + verb
  '頭がして痛い。',
  '胸がして苦しい。',

  // ことで (kotode) - "by means of, in the matter of"
  '話し合いで解決することで、合意した。',
  'メールで連絡することで、手間が省ける。',

  // ことを (kotoo) - object marker + noun
  '彼が来たことを知らない。',
  '会議が終わったことを報告する。',

  // Noun + だ + から (without こと) - simple copula + because
  '彼は学生だから、勉強しなければならない。',
  '今日は日曜日だから、銀行は休みだ。',
  'それは大事なものだから、気をつけて。',

  // だ + から (copula + because) without preceding noun context
  'これは本だ。',
  '彼は医者だ。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, {
    negatives,
    skipPositives: [
      // Polite form variant "ことなので" instead of "ことだから"
      // The rule focuses on ことだから (casual form) and ことなので is a polite variant
      'お客様のプライバシーに関わることなので、これ以上詳しいことは言えません。'
    ]
  });
});
