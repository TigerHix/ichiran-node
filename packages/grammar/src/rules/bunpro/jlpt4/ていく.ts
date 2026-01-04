import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ていく', (r) => {
  // Verb[te] + いく (to go on doing, to continue toward future)
  //
  // Key discriminators:
  // - lemma=いく (distinguishes from くる)
  // - Must be immediately preceded by て-form (て or んで)
  //
  // This prevents false positives on standalone いく:
  //   ✓ 食べていく (verb-te + いく)
  //   ✓ とんでいく (verb-nde + いく)
  //   ✗ 京都へいく (destination + いく)
  //
  // Note: GiNZA parses んで (from む/ぶ/ぬ verb stems) inconsistently:
  //   - Sometimes as pos=SCONJ (simpler sentences)
  //   - Sometimes as pos=AUX (complex sentences)
  //   Both have lemma=で (not だ, which is instrumental/copula)
  r.either(
    // Pattern 1: verb-te-form + いく (食べていく)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const iku = b.tok({ lemma: 'いく', pos: 'VERB' }, 'iku');
      b.inOrder(te, iku, 1);
      b.captureSpan('ていく', te, iku);
    },
    // Pattern 2: verb-nde-form + いく (とんでいく)
    // GiNZA parses んで as lemma=で with either pos=SCONJ or pos=AUX
    // (inconsistent depending on sentence context)
    (b) => {
      const nde = b.tok({ lemma: 'で', posOneOf: ['SCONJ', 'AUX'] }, 'nde');
      const iku = b.tok({ lemma: 'いく', pos: 'VERB' }, 'iku');
      b.inOrder(nde, iku, 1);
      b.captureSpan('んでいく', nde, iku);
    }
  );
});
