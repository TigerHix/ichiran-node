import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: ことだし (koto dashi) - "because X; and also X"
 *
 * Matches construction using こと + だ + し to give a reason,
 * often implying there are multiple reasons.
 *
 * Structure:
 * - Verb/い-adj (attributive form) + こと + だ + し
 * - Na-adj + な + こと + だ + し
 * - Noun + の + こと + だ + し
 *
 * Examples:
 * - 風邪を引いていることだし、今日くらいは何もしないでゆっくり休んだら？
 *   (Since you have a cold, why don't you rest today?)
 * - 天気もいいことだし、みんなで公園にでも行きましょう。
 *   (Since the weather is nice, let's all go to the park.)
 * - 彼が勤めている会社は有名なことだし、給料もいいんだろう。
 *   (Since the company he works for is famous, his salary must be good.)
 * - 彼女のことだし、きっと次の試合には勝つとわたしは信じているよ！
 *   (Knowing her, I am convinced that she will surely win the next match.)
 *
 * The し particle lists reasons, and こと + だ nominalizes the preceding
 * clause to make it one of the reasons.
 */
export default linguisticRule('ことだし', (r) => {
  r.either(
    // Branch 1: Na-adj + な + こと + だ + し
    (b1) => {
      const na = b1.particle('な', 'na');
      const koto = b1.noun({ lemma: 'こと' }, 'koto');
      const da = b1.tok({ lemma: 'だ' }, 'da');
      const shi = b1.tok({ text: 'し', pos: 'SCONJ' }, 'shi');
      b1.inOrder(na, koto, 1).inOrder(koto, da, 1).inOrder(da, shi, 1);
      b1.captureSpan('ことだし', na, shi);
    },
    // Branch 2: Noun + の + こと + だ + し
    (b2) => {
      const no = b2.particle('の', 'no');
      const koto = b2.noun({ lemma: 'こと' }, 'koto');
      const da = b2.tok({ lemma: 'だ' }, 'da');
      const shi = b2.tok({ text: 'し', pos: 'SCONJ' }, 'shi');
      b2.inOrder(no, koto, 1).inOrder(koto, da, 1).inOrder(da, shi, 1);
      b2.captureSpan('ことだし', no, shi);
    },
    // Branch 3: Verb/い-adj + こと + だ + し (main pattern)
    // This captures sentences like:
    // - 疲れたことだし (tired)
    // - お金がないことだし (no money - negative verb)
    // - 天気もいいことだし (weather is good - i-adj)
    // - 反省していることだし (reflecting - progressive verb)
    (b3) => {
      const koto = b3.noun({ lemma: 'こと' }, 'koto');
      const da = b3.tok({ lemma: 'だ' }, 'da');
      const shi = b3.tok({ text: 'し', pos: 'SCONJ' }, 'shi');
      b3.inOrder(koto, da, 1).inOrder(da, shi, 1);
      b3.captureSpan('ことだし', koto, shi);
    }
  );
});
