import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことに (koto ni) - "surprisingly/fortunately/unfortunately"
 *
 * Matches nominalized emotional evaluations: verb/adj + ことに
 * Expresses speaker's strong emotional response to a situation.
 *
 * Structure:
 * - Verb (any conjugated form with auxiliaries) + ことに
 * - ［い］Adj + ことに
 * - ［な］Adj + なことに
 *
 * Examples:
 * - 珍しいことに、彼がいいレストランに連れていってくれた (Surprisingly, he took me to a nice restaurant)
 * - 嬉しいことに日本語能力試験に合格しました (To my delight, I passed the JLPT)
 * - 幸せなことに、来年結婚します (Fortunately, I'm getting married next year)
 * - 驚いたことに、六ヶ月で赤ちゃんが初めて歩いた (To my surprise, the baby started walking at 6 months)
 * - ビックリしたことに、久しぶりに小学生時代の友達から電話がかかってきた (To my surprise, I got a call from an elementary school friend)
 * - 信じられないことに、毎日、彼は早起きして勉強している (Unbelievably, he wakes up early every day and studies)
 * - 驚くべきことに、彼は交通事故で怪我をしたあと、怪我を克服してオリンピックで金メダルを手にしたんだ (To our extreme surprise, after being injured in a traffic accident, he overcame his injury and won a gold medal at the Olympics)
 * - 残念なことに、その商品はもう販売中止になりました (Unfortunately, that product is no longer being sold)
 *
 * GiNZA parse structure:
 * - 珍しいことに: 珍しい(ADJ,dep=acl,head=1) + こと(NOUN,dep=obl) + に(ADP,dep=case,head=こと)
 * - 驚いたことに: 驚い(VERB,dep=acl,head=2) + た(AUX) + こと(NOUN,dep=obl) + に(ADP,dep=case,head=こと)
 * - 幸せなことに: 幸せ(ADJ,dep=acl,head=2) + な(AUX,lemma=だ) + こと(NOUN,dep=obl) + に(ADP,dep=case,head=こと)
 * - ビックリしたことに: ビックリ(VERB,dep=acl,head=3) + し(AUX) + た(AUX) + こと(NOUN,dep=obl) + に
 * - 信じられないことに: 信じ(VERB,dep=acl,head=3) + られ(AUX) + ない(AUX) + こと(NOUN,dep=obl) + に
 * - 驚くべきことに: 驚く(VERB,dep=acl,head=2) + べき(AUX) + こと(NOUN,dep=obl) + に
 *
 * Key discriminators:
 * - こと must have dep=obl (oblique nominal - indicates it's a nominalized clause)
 * - に must be a case particle (dep=case) attached to こと
 * - The predicate (verb/adj) has head pointing to the index of こと
 *
 * CRITICAL: In GiNZA's parse, the original predicate (verb/adj) has head=koto.index,
 * even when there are auxiliaries between them. We use this to find the real predicate.
 */
export default linguisticRule('ことに', (r) => {
  r.either(
    // Branch 1: Verb + (auxiliaries) + ことに
    (b) => {
      const koto = b.noun({ lemma: 'こと', dep: 'obl' }, 'koto');

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);
      b.caseMarker(koto, ni);

      // Find the verb whose head points to koto (may have auxiliaries in between)
      const verb = b.verb({ dep: 'acl' }, 'verb');

      b.captureSpan('ことに', verb, ni);
    },
    // Branch 2: ［い］Adj + ことに
    (b) => {
      const koto = b.noun({ lemma: 'こと', dep: 'obl' }, 'koto');

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);
      b.caseMarker(koto, ni);

      // Find the i-adj whose head points to koto
      const adj = b.adj({ dep: 'acl' }, 'adj');

      b.captureSpan('ことに', adj, ni);
    },
    // Branch 3: ［な］Adj + なことに
    (b) => {
      const adj = b.adj({ dep: 'acl' }, 'adj');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      b.auxOf(adj, na);

      const koto = b.noun({ lemma: 'こと', dep: 'obl' }, 'koto');
      b.inOrder(na, koto, 1);

      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);
      b.caseMarker(koto, ni);

      b.captureSpan('ことに', adj, ni);
    }
  );
});
