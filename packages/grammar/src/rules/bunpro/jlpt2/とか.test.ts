import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とか.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the quotational とか grammar rule
const negatives = [
  // Listing usage とか (noun + とか + noun) - "things like X, Y, etc."
  // This is the OTHER meaning of とか (listing examples), NOT the hearsay usage
  'りんごとかバナナを買う。',
  '本とか雑誌を読む。',
  '東京とか大阪に行きたい。',
  'コーヒーとか紅茶を飲む。',
  '日曜日とか土曜日に会いましょう。',
  'スーパーとかコンビニで買える。',
  'パンとか麺とかが好きです。',

  // Simple question particle か (not とか)
  '行くか？',
  '食べるかどうか分からない。',
  '来るかな。',
  '明日来るか？',

  // Quotative と alone (without か)
  '行くと言った。',
  '好きだと言った。',
  'いいよと言った。',

  // Different grammar patterns
  // かも (maybe) - not とか
  '雨が降るかも。',
  '行けるかも。',

  // かもしれない (might be) - not とか
  '雨が降るかもしれない。',
  '彼かもしれない。',

  // かどうか (whether or not) - not とか
  '行くかどうか分からない。',
  'できるかどうか試してみる。',

  // かと思うと (just when I thought) - different grammar
  '静かかと思うと、急に騒がしくなった。',
  '帰ったかと思うと、すぐに出て行った。',

  // かないかのうちに (as soon as) - different grammar
  '着いたか着ないかのうちにに出発した。',
  '終わるか終わらないかのうちに寝た。',

  // か何か (or something/someone) - similar but different structure
  'コーヒーか何か飲みたい。',
  '誰か何か言っていた。',
  '何か質問はありますか。',

  // Sentences where とか doesn't appear at all
  'これは本です。',
  '彼は行きました。',
  '今日は良い天気です。',

  // Listing pattern with multiple とか
  'AとかBとかCとかある。',
  'りんごとか、バナナとか、オレンジとかを買った。',

  // Noun + とか + verb (listing examples, not hearsay)
  '本とかを読んでいる。',  // ambiguous context but listing usage
  '映画とかを見る。',

  // Negative test - にとか (ni + toka) - different structure
  // '彼に行きたいとか言った。', // This might actually match (hearsay), so not a good negative

  // とかで (tokade) - instrumental listing
  'パソコンとかで仕事をする。',
  '車とかで行く。',

  // とかは (tokaha) - topic marking in listing
  'リンゴとかは好きじゃない。',
  '本とかは読まない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
