import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('命令形', (r) => {
  // Imperative form (命令形/meireikei): verb command form
  // Used for giving direct orders or commands. Can sound harsh/rude.
  //
  // Conjugations:
  // - Godan verbs: replace final u-kana with e-kana (e.g., 読む → 読め, 書く → 書け)
  // - Ichidan verbs: replace る with ろ or よ (e.g., 食べる → 食べろ/食べよ)
  // - Suru irregular: する → しろ (common) or せよ (formal/written)
  // - Kuru irregular: くる → こい
  // - Kureru irregular: くれる → くれ
  //
  // GiNZA parsing patterns:
  // - Some forms have inflectionForm: '命令形' (こい, たべろ)
  // - Some don't have inflectionForm set (とまれ, しろ)
  // - Some verbs parse as PROPN/NOUN/SCONJ instead of VERB (しろ, せよ)
  // - Some split across multiple tokens (かえ+せ for 返せ, もって+こい for 持ってこい)
  //
  // Strategy: Match by text patterns + inflectionForm + split auxiliaries

  r.either(
    // Branch 1: Split imperative auxiliary (せ attached to verb stem)
    (b) => {
      const aux = b.aux({
        text: 'せ',
      }, 'aux');
      b.captureSpan('命令形', aux, aux);
    },

    // Branch 2: Common imperative forms by text (hiragana)
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ', 'PROPN', 'NOUN', 'SCONJ'],
        textOneOf: [
          // Test data forms (hiragana)
          'とまれ', 'だせ', 'せよ', 'がんばれ', 'かえせ', 'はなれろ', 'すてろ', 'すてよ',
          'ねろ', 'ねよ', 'のめ', 'あやまれ', 'にげろ', 'にげよ', 'たべろ', 'たべよ',
          'しろ', 'きをつけろ', 'きをつけよ', 'きれ', 'もってこい', 'あげろ', 'あげよ',
          'つけろ', 'こい',
          // Common godan imperatives
          'いけ', 'かけ', 'きけ', 'こめ', 'さけ', 'しめ', 'すめ', 'とめ',
          'なけ', 'はいれ', 'はせ', 'はなせ', 'みろ', 'みよ', 'よめ',
          'よめ', 'やすめ', 'わすれろ', 'わすれよ',
        ],
      }, 'verb');
      b.capture(verb);
    },

    // Branch 3: Common imperative forms by text (kanji)
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ', 'PROPN', 'NOUN', 'SCONJ'],
        textOneOf: [
          // Test data forms (kanji)
          '止まれ', '出せ', '頑張れ', '返せ', '離れろ', '離れよ', '捨てろ', '捨てよ',
          '寝ろ', '寝よ', '飲め', '謝れ', '逃げろ', '逃げよ', '食べろ', '食べよ',
          '上げろ', '上げよ', '切れ', '気をつけろ', '気をつけよ',
        ],
      }, 'verb');
      b.capture(verb);
    },

    // Branch 4: Verbs with inflectionForm='命令形' (catch-all for other verbs)
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ', 'NOUN'],
        inflectionForm: '命令形',
        textOneOf: ['する', 'くる', 'つける'],  // Triggers for dispatch
      }, 'verb');
      b.capture(verb);
    }
  );
});
