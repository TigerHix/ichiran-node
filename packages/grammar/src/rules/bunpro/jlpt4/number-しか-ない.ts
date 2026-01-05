import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('number-しか-ない', (r) => {
  // Number/Noun + しか + Verb[negative] (Only X, nothing but X)
  // Examples: 100円しかない, ２時間しか遊べない, 一人しか行かない
  //
  // This is different from JLPT3's しかない (Verb + しか + ない = "have no choice but to")
  // and the general しか～ない pattern (Noun + しか + negative verb).
  // Here we specifically match: Number/Amount + しか + negative verb
  //
  // GiNZA parses numbers as NUM tokens, sometimes followed by counter words (NOUN)
  // Examples:
  // - 100円 (NUM + NOUN) + しか + ない
  // - 2時間 (NUM + NOUN) + しか + 遊べない (potential negative)
  // - 1度 (NUM + NOUN) + しか + 経験していない (progressive negative)
  //
  // The particle しか marks the number/amount and must be followed by negative verb

  r.either(
    // Pattern 1: NUM + counter (optional) + しか + simple ない (copula/auxiliary)
    // e.g., ４８時間しかない, １０冊しかない
    (b) => {
      const num = b.tok({ pos: 'NUM' }, 'num');
      const shika = b.particle('しか', 'shika');
      const nai = b.tok({
        lemmaOneOf: ['ない', 'ある'],
        depOneOf: ['fixed', 'aux', 'cop', 'root'],
      }, 'nai');

      b.inOrder(num, shika, 3);
      b.inOrder(shika, nai, 2);

      b.captureSpan('number-しか-ない', num, nai);
    },

    // Pattern 2: NUM + counter (optional) + しか + past なかった
    // e.g., (numbers only, not nouns)
    (b) => {
      const num = b.tok({ pos: 'NUM' }, 'num');
      const shika = b.particle('しか', 'shika');
      const nakat = b.aux({
        lemma: 'ない',
        inflectionForm: '連用形-促音便',
      }, 'nakat');
      const ta = b.aux({
        lemma: 'た',
        dep: 'aux'
      }, 'ta');

      b.inOrder(num, shika, 3);
      b.inOrder(shika, nakat, 2);
      b.inOrder(nakat, ta, 1);

      b.captureSpan('number-しか-ない', num, ta);
    },

    // Pattern 3: NUM + counter + しか + verb in negative form (potential or regular)
    // e.g., ２時間しか遊べない, 五キロしか走れない
    (b) => {
      const num = b.tok({ pos: 'NUM' }, 'num');
      const shika = b.particle('しか', 'shika');
      const verb = b.verb({}, 'verb');

      b.inOrder(num, shika, 3);
      b.inOrder(shika, verb, 5);

      b.captureSpan('number-しか-ない', num, verb);
    },

    // Pattern 4: NUM + counter + しか + progressive negative (～ていない)
    // e.g., ２回しか経験していない, 彼女にはまだ１回しか会っていない, １００円しか持っていない, ３本しかはえていない, 彼には１度しかかっていない
    (b) => {
      const num = b.tok({ pos: 'NUM' }, 'num');
      const shika = b.tok({ text: 'しか' }, 'shika');
      const verb = b.tok({
        inflectionForm: '連用形-一般',
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

      b.inOrder(num, shika, 3);
      b.inOrder(shika, verb, 5);
      b.inOrder(verb, te, 1);
      b.inOrder(te, iru, 1);
      b.inOrder(iru, nai, 1);

      b.captureSpan('number-しか-ない', num, nai);
    },

    // Pattern 5: NUM + counter + しか + past negative with verb (～なかった)
    // e.g., 半年で１回しか喧嘩しなかった, ２節しか進まなかった
    (b) => {
      const num = b.tok({ pos: 'NUM' }, 'num');
      const shika = b.particle('しか', 'shika');
      const verb = b.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const nakat = b.aux({
        lemma: 'ない',
        inflectionForm: '連用形-促音便',
      }, 'nakat');
      const ta = b.aux({
        lemma: 'た',
        dep: 'aux'
      }, 'ta');

      b.inOrder(num, shika, 3);
      b.inOrder(shika, verb, 5);
      b.inOrder(verb, nakat, 3);
      b.auxOf(verb, ta);
      b.inOrder(nakat, ta, 1);

      b.captureSpan('number-しか-ない', num, ta);
    },

    // Pattern 6: NUM + counter + しか + negative polite (～ません)
    // e.g., １０冊しかありません, ５人しかいません
    (b) => {
      const num = b.tok({ pos: 'NUM' }, 'num');
      const shika = b.particle('しか', 'shika');
      const verb = b.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mase = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');

      b.inOrder(num, shika, 3);
      b.inOrder(shika, verb, 5);
      b.auxOf(verb, mase);

      b.captureSpan('number-しか-ない', num, mase);
    }
  );
});
