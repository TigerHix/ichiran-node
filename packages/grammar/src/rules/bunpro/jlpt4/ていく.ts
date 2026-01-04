import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ていく', (r) => {
  // Verb[te] + いく (to go on doing, to continue toward future)
  //
  // Key discriminators:
  // - lemma=いく (distinguishes from くる)
  // - Must be immediately preceded by て-form (lemma=て, pos=SCONJ)
  //
  // This prevents false positives on standalone いく:
  //   ✓ 食べていく (verb-te + いく)
  //   ✗ 京都へいく (destination + いく)
  const te = r.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
  const iku = r.tok({ lemma: 'いく', pos: 'VERB' }, 'iku');

  // いく must come immediately after て
  r.inOrder(te, iku, 1);

  // Capture from て to いく
  r.captureSpan('ていく', te, iku);
});
