import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ぜんぜん', (r) => {
  // 全然 + negative (not at all) or positive slang (entirely/completely)
  // Patterns:
  // - 全然 + Verb［ない］: 全然食べない
  // - 全然 +［い］Adjective［ない］: 全然美味しくない
  // - 全然 + Noun + ではない/じゃない: 全然好きではない
  // - 全然 + positive (slang): 全然大丈夫, 全然いい

  const zenzen = r.adv({
    textOneOf: ['全然', 'ぜんぜん'],
  }, 'zenzen');

  r.either(
    // Pattern 1: Negative verb (～ない)
    // 全然食べない (zenzen + verb in 未然形-一般 + aux ない)
    (b) => {
      const verb = b.verb({
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.auxOf(verb, nai);
      b.inOrder(zenzen, verb, 5); // 全然 within 5 tokens of verb
      b.captureSpan('ぜんぜん', zenzen, nai);
    },

    // Pattern 2: Negative verb polite (～ません)
    // 全然ありません (zenzen + verb in 連用形-一般 + aux ませ in 未然形-一般)
    // Note: GiNZA parses ません as "ませ" + "ん"
    (b) => {
      const verb = b.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mase = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');
      b.auxOf(verb, mase);
      b.inOrder(zenzen, verb, 5);
      b.captureSpan('ぜんぜん', zenzen, mase);
    },

    // Pattern 3: Progressive negative (～ていない)
    // 全然理解していない (zenzen + verb in 連用形-一般 + SCONJ て + VERB いる in 未然形-一般 + aux ない)
    (b) => {
      const verb = b.verb({
        inflectionForm: '連用形-一般',
        tagOneOf: ['動詞-一般'],
      }, 'verb');
      const te = b.tok({
        pos: 'SCONJ',
        lemma: 'て',
      }, 'te');
      const iru = b.verb({
        lemma: 'いる',
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'iru');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.inOrder(verb, te, 1);
      b.inOrder(te, iru, 1);
      b.inOrder(iru, nai, 1);
      b.inOrder(zenzen, verb, 5);
      b.captureSpan('ぜんぜん', zenzen, nai);
    },

    // Pattern 3b: サ変 compound verb progressive negative (～していない)
    // 全然理解していない (zenzen + NOUN/VERB サ変 + aux する in 連用形 + SCONJ て + VERB いる in 未然形 + aux ない)
    (b) => {
      const sahen = b.tok({
        posOneOf: ['NOUN', 'VERB'],
        tagOneOf: ['名詞-普通名詞-サ変可能', '名詞-普通名詞-サ変形状'],
      }, 'sahen');
      const suru = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'する',
        inflectionForm: '連用形-一般',
      }, 'suru');
      b.inOrder(sahen, suru, 1);
      const te = b.tok({
        pos: 'SCONJ',
        lemma: 'て',
      }, 'te');
      const iru = b.tok({
        posOneOf: ['VERB'],
        lemma: 'いる',
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'iru');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.inOrder(suru, te, 1);
      b.inOrder(te, iru, 1);
      b.inOrder(iru, nai, 1);
      b.inOrder(zenzen, sahen, 5);
      b.captureSpan('ぜんぜん', zenzen, nai);
    },

    // Pattern 4: Negative i-adjective (～くない)
    // 全然美味しくない (zenzen + adj stem in 連用形-一般 + aux ない)
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
      b.inOrder(adjStem, nai, 1);
      b.inOrder(zenzen, adjStem, 5);
      b.captureSpan('ぜんぜん', zenzen, nai);
    },

    // Pattern 5: Negative i-adjective semi-polite (～くないです)
    // 全然美味しくないです
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
      b.inOrder(adjStem, nai, 1);
      b.inOrder(nai, desu, 1);
      b.inOrder(zenzen, adjStem, 5);
      b.captureSpan('ぜんぜん', zenzen, desu);
    },

    // Pattern 6: Negative i-adjective polite (～くありません)
    // 全然美味しくありません
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
      b.inOrder(adjStem, aru, 1);
      b.inOrder(aru, mase, 1);
      b.inOrder(zenzen, adjStem, 5);
      b.captureSpan('ぜんぜん', zenzen, mase);
    },

    // Pattern 7: Past negative i-adjective (～くなかった)
    // 全然良くなかった (zenzen + adj stem in 連用形-一般 + なかっ + た)
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['ADJ'],
        tagOneOf: ['形容詞-一般', '形容詞-非自立可能'],
        inflectionForm: '連用形-一般',
        conjugationClass: '形容詞',
      }, 'adjStem');
      const nakat = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
        conjugationClass: '形容詞',
        inflectionForm: '連用形-促音便',
      }, 'nakat');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');
      b.inOrder(adjStem, nakat, 1);
      b.inOrder(nakat, ta, 1);
      b.inOrder(zenzen, adjStem, 5);
      b.captureSpan('ぜんぜん', zenzen, ta);
    },

    // Pattern 8: Noun/Na-adjective + ではない
    // 全然好きではない, 全然綺麗ではない
    // Note: GiNZA parses ではない as "で" (lemma=だ or で) + "は" + "ない" with dep=fixed
    (b) => {
      const nounOrNa = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
        tagOneOf: ['名詞-普通名詞-一般', '形状詞-一般'],
      }, 'nounOrNa');
      const de = b.tok({
        textOneOf: ['で'],
      }, 'de');
      const wa = b.particle('は', 'wa');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '形容詞',
      }, 'nai');
      // では/じゃはない forms a fixed chunk
      b.inOrder(de, wa, 1);
      b.inOrder(wa, nai, 1);
      b.inOrder(nounOrNa, de, 3); // Optional modifier before で
      b.inOrder(zenzen, nounOrNa, 5);
      b.captureSpan('ぜんぜん', zenzen, nai);
    },

    // Pattern 9: Noun/Na-adjective + じゃない (casual)
    // 全然好きじゃない, 全然大丈夫じゃない
    // Note: GiNZA parses じゃない as "じゃ" (lemma=だ) + "ない" with dep=fixed
    (b) => {
      const nounOrNa = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
        tagOneOf: ['名詞-普通名詞-一般', '形状詞-一般'],
      }, 'nounOrNa');
      const ja = b.aux({
        lemma: 'だ',
        text: 'じゃ',
        inflectionForm: '連用形-融合',
      }, 'ja');
      const nai = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
        conjugationClass: '形容詞',
        dep: 'fixed',
      }, 'nai');
      b.inOrder(ja, nai, 1);
      b.inOrder(nounOrNa, ja, 2);
      b.inOrder(zenzen, nounOrNa, 5);
      b.captureSpan('ぜんぜん', zenzen, nai);
    },

    // Pattern 10: Positive usage (slang/informal)
    // 全然大丈夫, 全然いいよ, 全然OK
    // Matches zenzen + positive adjective/verb/noun (modern casual usage)
    (b) => {
      const pred = b.tok({
        posOneOf: ['ADJ', 'VERB', 'NOUN', 'AUX'],
      }, 'pred');
      b.inOrder(zenzen, pred, 3);
      b.captureSpan('ぜんぜん', zenzen, pred);
    }
  );
});
