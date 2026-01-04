import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('すくなくない', (r) => {
  // Double negative i-adjective: 少ない/すくない (few) + ない (not) = 少なくない/すくなくない (not a few = quite a few)
  // This is a specific i-adjective that forms a double negative meaning "many" or "quite a few"
  //
  // Forms:
  // - 少なくない/すくなくない (casual: not a few)
  // - 少なくないです/すくなくないです (semi-polite: not a few)
  // - 少なくありません/すくなくありません (polite: not a few)
  //
  // Key: Match the lemma "少ない" or "すくない" (meaning "few") in its negative form
  // GiNZA parses this as an i-adjective stem (少なく/すくなく) followed by ない

  r.either(
    // Branch 1: Casual form (少なくない/すくなくない)
    // Adjective stem (少なく/すくなく) + ない
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['VERB', 'NOUN', 'ADJ'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        lemmaOneOf: ['少ない', 'すくない'],
      }, 'adjStem');
      const nai = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'nai');
      b.inOrder(adjStem, nai, 1);
      b.captureSpan('すくなくない', adjStem, nai);
    },

    // Branch 2: Semi-polite form (少なくないです/すくなくないです)
    // Adjective stem (少なく/すくなく) + ない + です
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['VERB', 'NOUN', 'ADJ'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        lemmaOneOf: ['少ない', 'すくない'],
      }, 'adjStem');
      const nai = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
      }, 'nai');
      const desu = b.aux({
        lemma: 'です',
      }, 'desu');
      b.inOrder(adjStem, nai, 1);
      b.inOrder(nai, desu, 1);
      b.captureSpan('すくなくない', adjStem, desu);
    },

    // Branch 3: Polite form (少なくありません/すくなくありません)
    // Adjective stem (少なく/すくなく) + あります (parsed as あり + ませ + ん)
    // We capture up to ませ
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['VERB', 'NOUN', 'ADJ'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        lemmaOneOf: ['少ない', 'すくない'],
      }, 'adjStem');
      const aru = b.verb({
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'aru');
      const mase = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');
      b.inOrder(adjStem, aru, 1);
      b.inOrder(aru, mase, 1);
      b.captureSpan('すくなくない', adjStem, mase);
    }
  );
});
