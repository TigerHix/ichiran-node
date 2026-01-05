import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことなく.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ことなく grammar rule
const negatives = [
  // ずに (zu ni) - "without doing" (classical/literary form)
  // This is a related but different grammar point (JLPT3)
  '朝ごはんを食べずに仕事に行きました。',
  '何も知らずにあんなこと言ってごめんなさい。',
  '水を飲まずに運動をしていたから。',
  '勉強せずにテストを受けた。',
  '休まずに一日中ゲームをやり続けた。',

  // ないで (naide) - "without doing" (casual negative te-form)
  // This is a related but different grammar point (JLPT4)
  '朝ご飯を食べないで学校に行った。',
  '魚を焼かないで食べた。',
  '傘を持たないで出かけました。',
  '彼は何も言わないで部屋を出ていった。',

  // こと alone - "thing, matter" (nominalizer)
  '大切なことを忘れた。',
  '何か言いたいことはありますか。',
  'それはいいことだ。',
  '彼のことが心配だ。',

  // なく alone - adverbial form of ない (negative)
  '彼はなくて困った。',
  'お金がなくて買えなかった。',
  '雨が降らなくてよかった。',

  // ことになる (kotoninaru) - "it has been decided that"
  '来月日本に行くことになった。',
  '会議は明日開かれることになっている。',

  // ことになっている (kotoniatteiru) - "it is arranged that"
  '授業は9時に始まることになっている。',
  'ここに駐車してはいけないことになっている。',

  // ことにする (kotonisuru) - "decide to"
  '毎日運動することにした。',
  '明日出発することにしよう。',

  // ことがある (koto ga aru) - "sometimes there is/does"
  'このバスは遅れることがある。',
  '彼は来ないことがある。',

  // ことになる (koto ni naru) - "it turns out that"
  '明日会うことになる。',
  '彼が行くことになった。',

  // ことだ (koto da) - "should / it is"
  '健康のためには運動することだ。',
  '勉強することが大切だ。',

  // ことはない (koto wa nai) - "no need to"
  '心配することはない。',
  '急ぐことはない。',

  // ことか (kotoka) - emphatic expression
  'どれほど待ち望んだことか。',
  'どれだけ苦労したことか。',

  // ことさ (kotosa) - informal explanatory
  'どうでもいいことさ。',
  '気にしないことさ。',

  // ことなしに (kotonashini) - variant form but different grammar
  // Note: This is closely related but technically a different pattern
  '努力することなしに成功はできない。',
  '彼の許可することなしに、入室できない。',

  // Similar sounding but unrelated patterns
  // ことで (kotode) - "by means of, in the matter of"
  '話し合いで解決することで、合意した。',
  'メールで連絡することで、手間が省ける。',

  // ことを (kotoo) - object marker + noun
  '彼が来たことを知らない。',
  '会議が終わったことを報告する。',

  // Noun + なく (noun + naku) - different pattern
  'お金がなくて困っている。',
  '雨がなくて助かった。',

  // Verb + なく (verb + naku) - negative connective form
  '彼が来なくて失望した。',
  '雨が降らなくてよかった。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
