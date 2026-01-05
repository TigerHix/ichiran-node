import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かのようだ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the かのようだ grammar rule
// These test similar expressions and related grammar patterns
const negatives = [
  // ようだ/ようです (without かの - "seems like" without hypothetical nuance)
  '彼は元気なようです。',
  '雨が降るようだ。',
  '彼は来ないようです。',

  // まるで + ようだ (without か - less figurative)
  'まるで夢のようだ。',
  'まるで魔法のようです。',

  // みたいだ (mitai da - "looks like" - more casual)
  '彼は眠たいみたいだ。',
  'これは高いみたいです。',

  // みたいに (mitai ni - adverbial "like")
  '子供みたいに泣いている。',
  '彼女は子供みたいに振る舞う。',

  // みたいな (mitai na - adnominal "like")
  '子供みたいな人。',
  '夢みたいな話。',

  // ように (you ni - "in order to" or "like" - different usage)
  'わかるように説明してください。',
  '日本人のように話す。',
  '健康のために野菜を食べるようにしています。',

  // ような (you na - "such as" or "like")
  '東京のような大きな都市。',
  '彼のような人が欲しい。',

  // そうだ (sou da - "I heard" or "looks like")
  '雨が降るそうだ。',
  '彼は元気そうだ。',

  // そうに (sou ni - adverbial "looks")
  '彼は楽しそうに笑っている。',
  '美味しそうに食べている。',

  // らしい (rashii - "I heard" or "typical of")
  '彼は日本人らしい。',
  '明日は雨らしい。',

  // っぽい (ppoi - "-ish", "tending to")
  '彼は子供っぽい。',
  'この服は黒っぽい。',

  // がち (gachi - "tends to be", "often")
  '彼は遅れがちだ。',
  '病気がちな子供。',

  // ふうに (fuu ni - "in the manner of")
  'このように書く。',
  'そんなふうに言わないで。',

  // というふう (to iu fuu - "in such a way")
  '右へ左へというふうに動く。',
  'AというふうにBをする。',

  // か + の (separate question particle + nominalizer)
  '行くか行かないかの問題。',
  '彼は来るかどうかわからない。',
  '好きか嫌いかで選ぶ。',

  // かどうか (ka dou ka - "whether or not")
  '彼が来るかどうかわからない。',
  '行くかどうか決めていない。',

  // 単純な疑問文 (simple questions)
  '彼は来るか？',
  '明日は晴れるか？',

  // 間接疑問 (embedded questions)
  '彼が来るか知らない。',
  '明日雨が降るかどうか心配だ。',

  // かと思うと/かと思ったら (transition - "no sooner than")
  '泣いたかと思うと笑い出した。',
  '帰ってきたかと思ったらまた出かけた。',

  // かないかのうちに (transition - "as soon as")
  '着いたか着ないかのうちに出発した。',
  '終わったか終わらないかのうちに始まった。',

  // ようとする (you to suru - "about to", "try to")
  '彼は出ようとしている。',
  'その鳥は飛びようとした。',

  // ようになる (you ni naru - "come to be", "become able to")
  '日本語が話せるようになった。',
  '彼は勉強するようになった。',

  // ようがある (you ga aru - "there is a way to")
  '何とかするようがある。',
  '解決するようがあるかもしれない。',

  // ようがない/ようもない (you ga nai - "no way to")
  'もうどうしようもない。',
  '止めるようがない。',

  // かのようだ but without proper context (edge cases)
  // These might appear in other contexts
  '彼はか。のようだ。', // Ka as separate sentence end
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
