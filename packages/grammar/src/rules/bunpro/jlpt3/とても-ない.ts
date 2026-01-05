import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('とても-ない', (r) => {
  // とても-ない (totemo-nai) - "not at all, cannot possibly"
  // Pattern: とても + negative verb (cannot/very much not)
  // Used for emphasis with negatives to express impossibility
  //
  // Examples from test data:
  // - とても記憶できない (can't memorize at all)
  // - とても考えられない (can't think of at all)
  // - とても観光できない (can't do sightseeing at all)
  // - とても完成させられない (can't complete at all)
  // - とても敵わない (no match at all)
  // - とても信じられません (can't believe at all)
  // - とても歯が立たない (no chance at all)
  // - とても食べ切れない (no way to eat it all)
  // - とても理解できない (can't understand at all)
  // - とても車は買えません (can't buy a car at all)
  // - とても耐えられません (can't stand at all)
  //
  // GiNZA parsing notes:
  // - とても is ADV (副詞)
  // - Negative verb can be:
  //   - Potential form + ない/ません (e.g., できない, 買えません)
  //   - Verb + ない (e.g., 敵わない, 歯が立たない)
  //   - Negative auxiliary (ない, ません, れない, etc.)
  //
  // The rule should match とても followed by any verb in negative form

  const totemo = r.adv({ text: 'とても' }, 'totemo');

  // Negative auxiliary or verb ending
  // This matches the various negative forms:
  // - ない (plain negative)
  // - ません (polite negative)
  // - れない (potential negative)
  // - させられない (causative passive negative)
  // - 切れない (potential negative compound verb)
  const negAux = r.aux({
    lemmaOneOf: ['ない', 'ぬ', 'ます', 'ん'],
    textOneOf: ['ない', 'ません', 'ぬ', 'ん', 'ざる'],
  }, 'negAux');

  r.inOrder(totemo, negAux);
  r.captureSpan('とても-ない', totemo, negAux);
});
