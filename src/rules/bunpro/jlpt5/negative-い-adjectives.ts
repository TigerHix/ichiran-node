import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('negative-い-adjectives', (r) => {
  // Negative i-adjectives: 形容詞-一般 in 連用形-一般 (ren'yo/ku-form) + ない/あります/です
  //
  // Examples:
  // - さむくない (not cold) - casual
  // - さむくないです (not cold) - semi-polite
  // - さむくありません (not cold) - polite
  //
  // Note: いい → よくない (irregular, but parsed as lemma=よい)
  //
  // GiNZA parsing notes:
  // - The adjective stem in 連用形-一般 has inconsistent POS: VERB, NOUN, or ADJ
  //   but always has tag=形容詞-一般 and conjugationClass=形容詞
  // - The ない auxiliary also has inconsistent POS: AUX or ADJ
  //   but always has lemma=ない and conjugationClass=形容詞
  // - The inflection form of ない varies: 終止形-一般 (terminal) or 連体形-一般 (attributive)
  // - The ありません form is parsed as "あり" + "ませ" + "ん" (three tokens!)
  // - Dependency structure varies significantly based on context, so we use adjacency

  r.either(
    // Branch 1: Casual form (～くない)
    // Token with tag=形容詞-一般 in 連用形-一般 + token with lemma=ない (adjacent)
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['VERB', 'NOUN', 'ADJ'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        conjugationClass: '形容詞',
      }, 'adjStem');
      const nai = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
        conjugationClass: '形容詞',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'nai');
      // Require adjacency: adjStem immediately followed by nai
      b.inOrder(adjStem, nai, 1);
      b.captureSpan('negative-い-adjectives', adjStem, nai);
    },

    // Branch 2: Semi-polite form (～くないです)
    // Token with tag=形容詞-一般 in 連用形-一般 + token with lemma=ない + AUX です
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['VERB', 'NOUN', 'ADJ'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        conjugationClass: '形容詞',
      }, 'adjStem');
      const nai = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
        conjugationClass: '形容詞',
      }, 'nai');
      const desu = b.aux({
        lemma: 'です',
      }, 'desu');
      // Require adjacency: adjStem -> nai -> desu
      b.inOrder(adjStem, nai, 1);
      b.inOrder(nai, desu, 1);
      b.captureSpan('negative-い-adjectives', adjStem, desu);
    },

    // Branch 3: Polite form (～くありません)
    // Token with tag=形容詞-一般 in 連用形-一般 + VERB ある in 連用形-一般 + AUX ます in 未然形-一般
    // Note: GiNZA parses ありません as three tokens: あり + ませ + ん
    // We capture up to ませ (before the ん)
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['VERB', 'NOUN', 'ADJ'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        conjugationClass: '形容詞',
      }, 'adjStem');
      const aru = b.verb({
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'aru');
      const mase = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');
      // Require adjacency: adjStem -> aru -> mase
      b.inOrder(adjStem, aru, 1);
      b.inOrder(aru, mase, 1);
      b.captureSpan('negative-い-adjectives', adjStem, mase);
    }
  );
});
