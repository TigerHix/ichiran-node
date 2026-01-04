import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ではなくて-じゃなくて', (r) => {
  // ではなくて/じゃなくて (not X but Y) - conjunctive form of negation
  // Formal: Noun/Adj + の + ではなく（て）
  // Casual: Noun/Adj + の + じゃなく（て）
  //
  // GiNZA parsing patterns:
  // 1. Formal with は: で (AUX/ADP) + は (fixed) + なく (AUX/fixed, lemma=ない, text=なく) + て (mark)
  // 2. Formal without は: で (AUX/ADP) + なく (AUX/fixed, lemma=ない, text=なく) + て (mark)
  // 3. Casual: じゃ (AUX, lemma=だ) + なく (AUX/fixed, lemma=ない, text=なく) + て (mark)
  //
  // Key discriminator:
  // - Conjunctive form: text=なく (the conjunctive form)
  // - Plain negation: text=ない (should NOT match)
  //
  // Examples:
  // - 彼は歌手ではなくて俳優だ。
  // - 魚じゃなくて肉が食べたい。
  // - 彼は正社員でなくて、アルバイトです。

  r.either(
    // Pattern 1a: Formal ではなくて (with は and て)
    (b) => {
      const de = b.tok({
        text: 'で',
        lemmaOneOf: ['で', 'だ'],
        posOneOf: ['AUX', 'ADP'],
        depOneOf: ['cop', 'aux', 'case', 'fixed']
      }, 'de');
      const wa = b.tok({ text: 'は', dep: 'fixed' }, 'wa');
      const naku = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'naku');
      const te = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');

      b.inOrder(de, wa, 1);
      b.inOrder(wa, naku, 1);
      b.inOrder(naku, te, 1);
      b.captureSpan('ではなくて', de, te);
    },
    // Pattern 1b: Formal ではなく (with は, without て)
    (b) => {
      const de = b.tok({
        text: 'で',
        lemmaOneOf: ['で', 'だ'],
        posOneOf: ['AUX', 'ADP'],
        depOneOf: ['cop', 'aux', 'case', 'fixed']
      }, 'de');
      const wa = b.tok({ text: 'は', dep: 'fixed' }, 'wa');
      const naku = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'naku');

      b.inOrder(de, wa, 1);
      b.inOrder(wa, naku, 1);
      b.captureSpan('ではなく', de, naku);
    },
    // Pattern 2a: Formal ではなくて (without は, with て)
    (b) => {
      const de = b.tok({
        text: 'で',
        lemmaOneOf: ['で', 'だ'],
        posOneOf: ['AUX', 'ADP'],
        depOneOf: ['cop', 'aux', 'case', 'fixed']
      }, 'de');
      const naku = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'naku');
      const te = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');

      b.inOrder(de, naku, 2);
      b.inOrder(naku, te, 1);
      b.captureSpan('ではなくて', de, te);
    },
    // Pattern 2b: Formal ではなく (without は, without て)
    (b) => {
      const de = b.tok({
        text: 'で',
        lemmaOneOf: ['で', 'だ'],
        posOneOf: ['AUX', 'ADP'],
        depOneOf: ['cop', 'aux', 'case', 'fixed']
      }, 'de');
      const naku = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'naku');

      b.inOrder(de, naku, 2);
      b.captureSpan('ではなく', de, naku);
    },
    // Pattern 3a: Casual じゃなくて (with て)
    (b) => {
      const ja = b.aux({
        text: 'じゃ',
        lemma: 'だ',
        dep: 'cop'
      }, 'ja');
      const naku = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'naku');
      const te = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');

      b.inOrder(ja, naku, 1);
      b.inOrder(naku, te, 1);
      b.captureSpan('じゃなくて', ja, te);
    },
    // Pattern 3b: Casual じゃなく (without て)
    (b) => {
      const ja = b.aux({
        text: 'じゃ',
        lemma: 'だ',
        dep: 'cop'
      }, 'ja');
      const naku = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'naku');

      b.inOrder(ja, naku, 1);
      b.captureSpan('じゃなく', ja, naku);
    }
  );
});
