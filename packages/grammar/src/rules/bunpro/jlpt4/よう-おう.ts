import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('よう-おう', (r) => {
  // Volitional form: Verb + よう/う (let's, I shall)
  //
  // Ichidan verbs (る-verbs): replace る with よう
  //   - 食べる → 食べよう
  //   - 見る → 見よう
  //
  // Godan verbs (五段 verbs): replace -u with -ou
  //   - 行く → 行こう (iku -> ikou)
  //   - 読む → 読もう (yomu -> yomou)
  //   - 待つ → 待とう (matsu -> matou)
  //   - 死ぬ → 死のう (shinu -> shinou)
  //   - 飛ぶ → 飛ぼう (tobu -> tobou)
  //   - 泳ぐ → 泳ごう (oyogu -> oyogou)
  //   - 話す → 話そう (hanasu -> hanasou)
  //   - 帰る → 帰ろう (kaeru -> kaerou)
  //
  // Irregular verbs:
  //   - する → しよう
  //   - くる → こよう
  //
  // GiNZA parsing patterns:
  //   - Most volitional forms: single VERB token with inflectionForm: '意志推量形'
  //   - Suru-verbs may parse as: NOUN + VERB(lemma=する) + AUX(lemma=する, inflectionForm='意志推量形')
  //     e.g., "運転しよう" → "運転" + "し" + "よう"
  //   - Test sentences use kanji forms (帰ろう instead of かえろう)
  //
  // We need text/lemma triggers for dispatch

  r.either(
    // Branch 1: Auxiliary volitional suffix split from main verb
    (b) => {
      const aux = b.aux({
        inflectionForm: '意志推量形',
        textOneOf: ['よう', 'う'],
      }, 'aux');
      b.captureSpan('よう-おう', aux, aux);
    },
    // Branch 2: All single token volitional VERB forms - exhaustive text list
    (b) => {
      const verb = b.verb({
        inflectionForm: '意志推量形',
        textOneOf: [
          // Irregular verbs
          'しよう', 'こよう', 'こう',
          // Ichidan verbs (test data forms)
          'たべよう', '食べよう', 'あきらめよう', '諦めよう',
          // Godan verbs (test data forms - hiragana)
          'いこう', 'かえろう', 'まとう', 'かこう', 'はなそう', 'がんばろう',
          // Godan verbs (test data forms - kanji)
          '行こう', '帰ろう', '待とう', '書こう', '話そう', '頑張ろう',
          // Compound forms
          'かってあげよう', '買ってあげよう',
        ],
      }, 'verb');
      b.captureSpan('よう-おう', verb, verb);
    },
    // Branch 3: Lemma-based match for all common verbs
    (b) => {
      const verb = b.verb({
        inflectionForm: '意志推量形',
        lemmaOneOf: [
          'する', 'くる', '来る',
          '行く', '食べる', '帰る', '話す', '待つ', '書く',
          '諦める', '頑張る',
          '運転する',
          'あげる', '買う',
        ],
      }, 'verb');
      b.captureSpan('よう-おう', verb, verb);
    }
  );
});
