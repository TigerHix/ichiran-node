import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: ずじまい (zujimai) - "ended up not doing X; unfortunately X didn't happen"
 *
 * Verb negative stem (zu-form) + じまい = "ended up not doing X"
 *
 * This is a classical/literary form expressing regret that something
 * unfortunately did not happen or come to fruition. The ず is the
 * classical negative auxiliary (equivalent to ぬ/ない), and じまい
 * is the voiced conjunctive form of しまう (to finish/complete).
 *
 * Formation:
 * - Remove ない from the negative form and attach ずじまい
 * - Five-grade verbs (五段動詞): 行かない → 行かずじまい
 * - Ichidan verbs (一段動詞): 食べない → 食べずじまい
 * - Irregular verbs:
 *   - する → せずじまい (NOT しずじまい)
 *   - 来る → 来ずじまい
 *
 * Examples:
 * - 結局、やらずじまいで新学期になっちゃった (Ended up not doing it and new term started)
 * - 仕事から手が離せなくて結局いけずじまいだった (Ended up not being able to go)
 * - 色々とやることがあって行かずじまいだった (Ended up not going due to various things)
 * - 伝えたかったことを伝えずじまいで元カノと別れた (Ended up not conveying what I wanted)
 * - 仲直り出来ずじまいだった旧友と５年ぶりに再会した (Old friend I couldn't make up with)
 *
 * GiNZA parse patterns:
 * - ず as AUX with dep=aux attached to verb stem
 * - ず as PART with dep=mark or dep=case
 * - じまい as NOUN or AUX following ず
 */
export default bunproLinguisticRule('ずじまい', (r) => {
  r.either(
    // Pattern 1: ず as auxiliary (AUX) with dep=aux
    (b) => {
      const zu = b.aux({
        text: 'ず',
        dep: 'aux',
      }, 'zu');
      const jima = b.tok({
        text: 'じまい',
        posOneOf: ['NOUN', 'AUX'],
      }, 'jimai');
      b.inOrder(zu, jima, 1);
      b.captureSpan('ずじまい', zu, jima);
    },

    // Pattern 2: ず as particle (PART) with dep=mark or dep=case
    (b) => {
      const zu = b.tok({
        text: 'ず',
        pos: 'PART',
        depOneOf: ['mark', 'case'],
      }, 'zu');
      const jima = b.tok({
        text: 'じまい',
        posOneOf: ['NOUN', 'AUX'],
      }, 'jimai');
      b.inOrder(zu, jima, 1);
      b.captureSpan('ずじまい', zu, jima);
    },

    // Pattern 3: ず with any POS when lemma indicates classical negative
    (b) => {
      const zu = b.tok({
        text: 'ず',
        lemmaOneOf: ['ず', 'ずに', 'ぬ'],
      }, 'zu');
      const jima = b.tok({
        text: 'じまい',
      }, 'jimai');
      b.inOrder(zu, jima, 1);
      b.captureSpan('ずじまい', zu, jima);
    },

    // Pattern 4: ず with lemma=する for irregular する verb
    (b) => {
      const zu = b.tok({
        text: 'ず',
        lemma: 'する',
      }, 'zu');
      const jima = b.tok({
        text: 'じまい',
      }, 'jimai');
      b.inOrder(zu, jima, 1);
      b.captureSpan('ずじまい', zu, jima);
    }
  );
});
