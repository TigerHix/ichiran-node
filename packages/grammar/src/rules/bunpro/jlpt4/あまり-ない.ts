import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('あまり-ない', (r) => {
  // あまり + negative form (not very, not much)
  // Patterns: あまり + negative verb/adj/noun
  // - あまり + Verb［ない］: あまり食べない, ありません
  // - あまり +［い］Adjective［ない］: あまり美味しくない
  // - あまり + Noun + ではない: あまり好きではない
  // - Also accepts casual form: あんまり

  const amari = r.adv({
    lemmaOneOf: ['あまり', 'あんまり'],
  }, 'amari');

  r.either(
    // Pattern 1: Negative verb (～ない)
    // あまり食べない (amari + verb in 未然形-一般 + aux ない)
    (b) => {
      const verb = b.verb({
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.auxOf(verb, nai);
      b.inOrder(amari, verb, 5); // あまり within 5 tokens of verb
      b.captureSpan('あまり-ない', amari, nai);
    },

    // Pattern 2: Negative verb polite (～ません)
    // ありません (amari + verb in 連用形-一般 + aux ませ in 未然形-一般)
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
      b.inOrder(amari, verb, 5);
      b.captureSpan('あまり-ない', amari, mase);
    },

    // Pattern 3: Progressive negative (～ていない)
    // あまり考えていない (amari + verb in 連用形-一般 + SCONJ て + VERB いる in 未然形-一般 + aux ない)
    // Exclude auxiliary verbs like する (handled in Pattern 3b for サ変)
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
      b.inOrder(amari, verb, 5);
      b.captureSpan('あまり-ない', amari, nai);
    },

    // Pattern 3b: サ変 compound verb progressive negative (～していない)
    // あまり理解していない (amari + NOUN/VERB サ変 + aux する in 連用形 + SCONJ て + VERB いる in 未然形 + aux ない)
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
      b.inOrder(amari, sahen, 5);
      b.captureSpan('あまり-ない', amari, nai);
    },

    // Pattern 4: Negative i-adjective (～くない)
    // あまり美味しくない (amari + adj stem in 連用形-一般 + aux ない)
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
      b.inOrder(amari, adjStem, 5);
      b.captureSpan('あまり-ない', amari, nai);
    },

    // Pattern 5: Negative i-adjective semi-polite (～くないです)
    // あまり美味しくないです
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
      b.inOrder(amari, adjStem, 5);
      b.captureSpan('あまり-ない', amari, desu);
    },

    // Pattern 6: Negative i-adjective polite (～くありません)
    // あまり美味しくありません
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
      b.inOrder(amari, adjStem, 5);
      b.captureSpan('あまり-ない', amari, mase);
    },

    // Pattern 7: Past negative i-adjective (～くなかった)
    // あまり良くなかった (amari + adj stem in 連用形-一般 + なかっ + た)
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['ADJ'],
        tag: '形容詞-非自立可能',
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
      b.inOrder(amari, adjStem, 5);
      b.captureSpan('あまり-ない', amari, ta);
    },

    // Pattern 8: Noun/Na-adjective + ではない
    // あまり好きではない, あまり平和ではない
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
      b.inOrder(amari, nounOrNa, 5);
      b.captureSpan('あまり-ない', amari, nai);
    },

    // Pattern 9: Noun/Na-adjective + じゃない (casual)
    // あまり好きじゃない
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
      b.inOrder(amari, nounOrNa, 5);
      b.captureSpan('あまり-ない', amari, nai);
    }
  );
});
