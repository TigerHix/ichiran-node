import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('いがい', (r) => {
  // 以外/いがい (except/other than) - NOUN/ADV usage
  // Key discriminator: must NOT be ADJ (which would be 意外 = surprise/unexpected)
  //
  // Pattern analysis:
  // - Noun + 以外 + の (modifying following noun): 和食以外のもの
  // - Noun + 以外 + に (adverbial with particle): 日本語以外に話せる
  // - Noun + 以外 (standalone): メアリー以外、誰も来なかった
  // - Verb phrase + 以外: 言われたこと以外何もしない
  // - Noun + 以外 as ROOT: ピザとコーラ以外他に...
  //
  // POS varies: NOUN (with nmod/obl/root) or ADV (with advmod/obl)
  // Lemma is either 以外 (kanji) or いがい (hiragana)
  //
  // Negative case: 意外 (surprise) is always ADJ

  r.either(
    // Pattern 1: 以外/いがい as NOUN modifying another noun (noun + 以外 + の)
    (b) => {
      const igai = b.tok({ lemmaOneOf: ['以外', 'いがい'], pos: 'NOUN', dep: 'nmod' }, 'igai');
      // Optionally capture following の particle
      const no = b.particle('の', 'no');
      b.inOrder(igai, no, 1);
      b.captureSpan('以外', igai, no);
    },
    // Pattern 2: 以外/いがい as NOUN with oblique/root relationship (often followed by に, or as ROOT)
    (b) => {
      const igai = b.tok({ lemmaOneOf: ['以外', 'いがい'], pos: 'NOUN', depOneOf: ['obl', 'nmod', 'root'] }, 'igai');
      b.capture(igai);
    },
    // Pattern 3: 以外/いがい as ADV (adverbial usage, modifying clause)
    (b) => {
      const igai = b.tok({ lemmaOneOf: ['以外', 'いがい'], pos: 'ADV', depOneOf: ['advmod', 'obl'] }, 'igai');
      b.capture(igai);
    }
  );
});
