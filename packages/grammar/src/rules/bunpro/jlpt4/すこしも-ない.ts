import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('すこしも-ない', (r) => {
  // すこしも + negative form (not even a little, not at all)
  // Patterns: すこしも + negative verb/adj/noun
  // - すこしも + Verb［ない］: 少しも食べない, 少しもない
  // - すこしも +［い］Adjective［ない］: 少しも美味しくない
  // - すこしも + Noun + ではない/じゃない: 少しも好きではない
  // Also accepts kanji form: 少しも

  // GiNZA may tokenize すこしも as a single ADV token or as NOUN すこし + particle も
  // Handle both cases with r.either

  r.either(
    // === Single token case: すこしも as one token ===
    (b) => {
      const sukoshimo = b.tok({
        textOneOf: ['すこしも', '少しも'],
      }, 'sukoshimo');

      b.either(
        // Pattern 1a: Negative verb (～ない)
        // 少しも食べない (sukoshimo + verb in 未然形-一般 + aux ない)
        (c) => {
          const verb = c.verb({
            inflectionForm: '未然形-一般',
          }, 'verb');
          const nai = c.aux({
            lemma: 'ない',
            conjugationClass: '助動詞-ナイ',
          }, 'nai');
          c.auxOf(verb, nai);
          c.inOrder(sukoshimo, verb, 5);
          c.captureSpan('すこしも-ない', sukoshimo, nai);
        },

        // Pattern 2a: Negative verb polite (～ません)
        // 少しもありません
        (c) => {
          const verb = c.verb({
            inflectionForm: '連用形-一般',
          }, 'verb');
          const mase = c.aux({
            lemma: 'ます',
            inflectionForm: '未然形-一般',
          }, 'mase');
          c.auxOf(verb, mase);
          c.inOrder(sukoshimo, verb, 5);
          c.captureSpan('すこしも-ない', sukoshimo, mase);
        },

        // Pattern 3a: Progressive negative (～ていない)
        // 少しも考えていない
        (c) => {
          const verb = c.tok({
            posOneOf: ['VERB', 'AUX'],
          }, 'verb');
          const te = c.tok({
            pos: 'SCONJ',
            lemma: 'て',
          }, 'te');
          const iru = c.verb({
            lemma: 'いる',
            inflectionForm: '未然形-一般',
            dep: 'fixed',
          }, 'iru');
          const nai = c.aux({
            lemma: 'ない',
            conjugationClass: '助動詞-ナイ',
          }, 'nai');
          c.inOrder(verb, te, 1);
          c.inOrder(te, iru, 1);
          c.inOrder(iru, nai, 1);
          c.inOrder(sukoshimo, verb, 5);
          c.captureSpan('すこしも-ない', sukoshimo, nai);
        },

        // Pattern 4a: Negative i-adjective (～くない)
        // 少しも美味しくない
        (c) => {
          const adjStem = c.tok({
            posOneOf: ['VERB', 'NOUN', 'ADJ'],
            tag: '形容詞-一般',
            inflectionForm: '連用形-一般',
            conjugationClass: '形容詞',
          }, 'adjStem');
          const nai = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
            inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
          }, 'nai');
          c.inOrder(adjStem, nai, 1);
          c.inOrder(sukoshimo, adjStem, 5);
          c.captureSpan('すこしも-ない', sukoshimo, nai);
        },

        // Pattern 5a: Negative i-adjective semi-polite (～くないです)
        // 少しも美味しくないです
        (c) => {
          const adjStem = c.tok({
            posOneOf: ['VERB', 'NOUN', 'ADJ'],
            tag: '形容詞-一般',
            inflectionForm: '連用形-一般',
            conjugationClass: '形容詞',
          }, 'adjStem');
          const nai = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
          }, 'nai');
          const desu = c.aux({
            lemma: 'です',
          }, 'desu');
          c.inOrder(adjStem, nai, 1);
          c.inOrder(nai, desu, 1);
          c.inOrder(sukoshimo, adjStem, 5);
          c.captureSpan('すこしも-ない', sukoshimo, desu);
        },

        // Pattern 6a: Negative i-adjective polite (～くありません)
        // 少しも美味しくありません
        (c) => {
          const adjStem = c.tok({
            posOneOf: ['VERB', 'NOUN', 'ADJ'],
            tag: '形容詞-一般',
            inflectionForm: '連用形-一般',
            conjugationClass: '形容詞',
          }, 'adjStem');
          const aru = c.verb({
            lemma: 'ある',
            inflectionForm: '連用形-一般',
          }, 'aru');
          const mase = c.aux({
            lemma: 'ます',
            inflectionForm: '未然形-一般',
          }, 'mase');
          c.inOrder(adjStem, aru, 1);
          c.inOrder(aru, mase, 1);
          c.inOrder(sukoshimo, adjStem, 5);
          c.captureSpan('すこしも-ない', sukoshimo, mase);
        },

        // Pattern 7a: Past negative i-adjective (～くなかった)
        // 少しも良くなかった
        (c) => {
          const adjStem = c.tok({
            posOneOf: ['ADJ'],
            tag: '形容詞-非自立可能',
            inflectionForm: '連用形-一般',
            conjugationClass: '形容詞',
          }, 'adjStem');
          const nakat = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
            inflectionForm: '連用形-促音便',
          }, 'nakat');
          const ta = c.aux({
            lemma: 'た',
          }, 'ta');
          c.inOrder(adjStem, nakat, 1);
          c.inOrder(nakat, ta, 1);
          c.inOrder(sukoshimo, adjStem, 5);
          c.captureSpan('すこしも-ない', sukoshimo, ta);
        },

        // Pattern 7.5a: Past negative verb (～なかった)
        // 少しも理解出来なかった (potential verb + past negative)
        (c) => {
          const verb = c.tok({
            posOneOf: ['VERB', 'AUX'],
          }, 'verb');
          const nakat = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
            inflectionForm: '連用形-促音便',
          }, 'nakat');
          const ta = c.aux({
            lemma: 'た',
          }, 'ta');
          c.inOrder(verb, nakat, 3); // Allow multiple tokens (compound verbs)
          c.inOrder(nakat, ta, 1);
          c.inOrder(sukoshimo, verb, 5);
          c.captureSpan('すこしも-ない', sukoshimo, ta);
        },

        // Pattern 8a: Noun/Na-adjective + ではない
        // 少しも好きではない, 少しも綺麗ではない
        (c) => {
          const nounOrNa = c.tok({
            posOneOf: ['NOUN', 'ADJ'],
          }, 'nounOrNa');
          const de = c.tok({
            textOneOf: ['で'],
          }, 'de');
          const wa = c.particle('は', 'wa');
          const nai = c.aux({
            lemma: 'ない',
            conjugationClass: '形容詞',
          }, 'nai');
          c.inOrder(de, wa, 1);
          c.inOrder(wa, nai, 1);
          c.inOrder(nounOrNa, de, 3);
          c.inOrder(sukoshimo, nounOrNa, 5);
          c.captureSpan('すこしも-ない', sukoshimo, nai);
        },

        // Pattern 9a: Noun/Na-adjective + じゃない (casual)
        // 少しも好きじゃない, 少しも安全じゃない
        (c) => {
          const nounOrNa = c.tok({
            posOneOf: ['NOUN', 'ADJ'],
          }, 'nounOrNa');
          const ja = c.aux({
            lemma: 'だ',
            text: 'じゃ',
            inflectionForm: '連用形-融合',
          }, 'ja');
          const nai = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
            dep: 'fixed',
          }, 'nai');
          c.inOrder(ja, nai, 1);
          c.inOrder(nounOrNa, ja, 2);
          c.inOrder(sukoshimo, nounOrNa, 5);
          c.captureSpan('すこしも-ない', sukoshimo, nai);
        }
      );
    },

    // === Two token case: すこし/少し + particle も ===
    (b) => {
      const sukoshi = b.tok({
        lemmaOneOf: ['すこし', '少し'],
      }, 'sukoshi');
      const mo = b.particle('も', 'mo');
      b.inOrder(sukoshi, mo, 1);

      b.either(
        // Pattern 1b: Negative verb (～ない)
        // 少しも食べない (sukoshi + mo + verb in 未然形-一般 + aux ない)
        (c) => {
          const verb = c.verb({
            inflectionForm: '未然形-一般',
          }, 'verb');
          const nai = c.aux({
            lemma: 'ない',
            conjugationClass: '助動詞-ナイ',
          }, 'nai');
          c.auxOf(verb, nai);
          c.inOrder(mo, verb, 5);
          c.captureSpan('すこしも-ない', sukoshi, nai);
        },

        // Pattern 2b: Negative verb polite (～ません)
        (c) => {
          const verb = c.verb({
            inflectionForm: '連用形-一般',
          }, 'verb');
          const mase = c.aux({
            lemma: 'ます',
            inflectionForm: '未然形-一般',
          }, 'mase');
          c.auxOf(verb, mase);
          c.inOrder(mo, verb, 5);
          c.captureSpan('すこしも-ない', sukoshi, mase);
        },

        // Pattern 3b: Progressive negative (～ていない)
        (c) => {
          const verb = c.tok({
            posOneOf: ['VERB', 'AUX'],
          }, 'verb');
          const te = c.tok({
            pos: 'SCONJ',
            lemma: 'て',
          }, 'te');
          const iru = c.verb({
            lemma: 'いる',
            inflectionForm: '未然形-一般',
            dep: 'fixed',
          }, 'iru');
          const nai = c.aux({
            lemma: 'ない',
            conjugationClass: '助動詞-ナイ',
          }, 'nai');
          c.inOrder(verb, te, 1);
          c.inOrder(te, iru, 1);
          c.inOrder(iru, nai, 1);
          c.inOrder(mo, verb, 5);
          c.captureSpan('すこしも-ない', sukoshi, nai);
        },

        // Pattern 4b: Negative i-adjective (～くない)
        (c) => {
          const adjStem = c.tok({
            posOneOf: ['VERB', 'NOUN', 'ADJ'],
            tag: '形容詞-一般',
            inflectionForm: '連用形-一般',
            conjugationClass: '形容詞',
          }, 'adjStem');
          const nai = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
            inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
          }, 'nai');
          c.inOrder(adjStem, nai, 1);
          c.inOrder(mo, adjStem, 5);
          c.captureSpan('すこしも-ない', sukoshi, nai);
        },

        // Pattern 5b: Negative i-adjective semi-polite (～くないです)
        (c) => {
          const adjStem = c.tok({
            posOneOf: ['VERB', 'NOUN', 'ADJ'],
            tag: '形容詞-一般',
            inflectionForm: '連用形-一般',
            conjugationClass: '形容詞',
          }, 'adjStem');
          const nai = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
          }, 'nai');
          const desu = c.aux({
            lemma: 'です',
          }, 'desu');
          c.inOrder(adjStem, nai, 1);
          c.inOrder(nai, desu, 1);
          c.inOrder(mo, adjStem, 5);
          c.captureSpan('すこしも-ない', sukoshi, desu);
        },

        // Pattern 6b: Negative i-adjective polite (～くありません)
        (c) => {
          const adjStem = c.tok({
            posOneOf: ['VERB', 'NOUN', 'ADJ'],
            tag: '形容詞-一般',
            inflectionForm: '連用形-一般',
            conjugationClass: '形容詞',
          }, 'adjStem');
          const aru = c.verb({
            lemma: 'ある',
            inflectionForm: '連用形-一般',
          }, 'aru');
          const mase = c.aux({
            lemma: 'ます',
            inflectionForm: '未然形-一般',
          }, 'mase');
          c.inOrder(adjStem, aru, 1);
          c.inOrder(aru, mase, 1);
          c.inOrder(mo, adjStem, 5);
          c.captureSpan('すこしも-ない', sukoshi, mase);
        },

        // Pattern 7b: Past negative i-adjective (～くなかった)
        (c) => {
          const adjStem = c.tok({
            posOneOf: ['ADJ'],
            tag: '形容詞-非自立可能',
            inflectionForm: '連用形-一般',
            conjugationClass: '形容詞',
          }, 'adjStem');
          const nakat = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
            inflectionForm: '連用形-促音便',
          }, 'nakat');
          const ta = c.aux({
            lemma: 'た',
          }, 'ta');
          c.inOrder(adjStem, nakat, 1);
          c.inOrder(nakat, ta, 1);
          c.inOrder(mo, adjStem, 5);
          c.captureSpan('すこしも-ない', sukoshi, ta);
        },

        // Pattern 7.5b: Past negative verb (～なかった)
        // 少しも理解出来なかった (potential verb + past negative)
        (c) => {
          const verb = c.tok({
            posOneOf: ['VERB', 'AUX'],
          }, 'verb');
          const nakat = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
            inflectionForm: '連用形-促音便',
          }, 'nakat');
          const ta = c.aux({
            lemma: 'た',
          }, 'ta');
          c.inOrder(verb, nakat, 3); // Allow multiple tokens (compound verbs)
          c.inOrder(nakat, ta, 1);
          c.inOrder(mo, verb, 5);
          c.captureSpan('すこしも-ない', sukoshi, ta);
        },

        // Pattern 8b: Noun/Na-adjective + ではない
        (c) => {
          const nounOrNa = c.tok({
            posOneOf: ['NOUN', 'ADJ'],
          }, 'nounOrNa');
          const de = c.tok({
            textOneOf: ['で'],
          }, 'de');
          const wa = c.particle('は', 'wa');
          const nai = c.aux({
            lemma: 'ない',
            conjugationClass: '形容詞',
          }, 'nai');
          c.inOrder(de, wa, 1);
          c.inOrder(wa, nai, 1);
          c.inOrder(nounOrNa, de, 3);
          c.inOrder(mo, nounOrNa, 5);
          c.captureSpan('すこしも-ない', sukoshi, nai);
        },

        // Pattern 9b: Noun/Na-adjective + じゃない (casual)
        (c) => {
          const nounOrNa = c.tok({
            posOneOf: ['NOUN', 'ADJ'],
          }, 'nounOrNa');
          const ja = c.aux({
            lemma: 'だ',
            text: 'じゃ',
            inflectionForm: '連用形-融合',
          }, 'ja');
          const nai = c.tok({
            posOneOf: ['AUX', 'ADJ'],
            lemma: 'ない',
            conjugationClass: '形容詞',
            dep: 'fixed',
          }, 'nai');
          c.inOrder(ja, nai, 1);
          c.inOrder(nounOrNa, ja, 2);
          c.inOrder(mo, nounOrNa, 5);
          c.captureSpan('すこしも-ない', sukoshi, nai);
        }
      );
    }
  );
});
