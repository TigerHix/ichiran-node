import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことから (koto kara) - "from the fact that", "because of the fact that"
 *
 * Matches nominalized clauses (こと) + から to indicate logical reasoning or conclusion.
 *
 * Structure:
 * - Verb/Adj (attributive form) + こと + から
 * - Na-adj + な + こと + から
 * - Noun + の + こと + から
 *
 * Examples:
 * - コーヒーが冷たいことから、淹れられたのは前だと分かる (From the fact that the coffee is cold...)
 * - 家の家具が全部新しいことから、彼は引っ越してきたばかりだと分かった (From the fact that all furniture is new...)
 * - 野球がとても上手なことから、将来は有名になりそうだ (From the fact that he's very good at baseball...)
 * - 以上のことから、この結論に至りました (From these facts, we reached this conclusion)
 *
 * GiNZA parse structure:
 * - こと: NOUN with dep=obl (oblique nominal - key discriminator!)
 * - から: ADP with lemma=から, dep=case, head=こと
 *
 * The key discriminator is that こと has dep=obl, which indicates it's functioning
 * as a nominalized clause marker rather than a regular noun.
 */
export default bunproLinguisticRule('ことから', (r) => {
  // こと (nominalizer) - must have dep=obl to distinguish from regular noun usage
  const koto = r.noun({ lemma: 'こと', dep: 'obl' }, 'koto');

  // から (case particle)
  const kara = r.particle('から', 'kara');

  // から must immediately follow こと
  r.inOrder(koto, kara, 1);

  // から must attach to こと as case marker
  r.caseMarker(koto, kara);

  // Capture the span from the nominalized content to から
  r.captureAs('koto', koto);
  r.captureAs('kara', kara);
  r.captureSpan('ことから', koto, kara);
});
