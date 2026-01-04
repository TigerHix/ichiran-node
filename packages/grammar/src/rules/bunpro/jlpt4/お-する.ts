import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('お-する', (r) => {
  // Humble action form: お/ご + verb-stem + する/します
  // Examples:
  //   お電話します (I humbly call)
  //   ご確認する (to humbly check)
  //   お閉めしました (I humbly closed)
  //
  // GiNZA parses this in several ways:
  // 1. Separate prefix: お (NOUN, tag=接頭辞, dep=compound) + 電話 (VERB) + し (AUX, lemma=する)
  // 2. Merged compound: お守り (single NOUN) + し (AUX, lemma=する)
  // 3. Noun stems: 休み (NOUN, lemma=休む) + します
  // 4. Sometimes prefix has different tag (感動詞-フィラー)
  //
  // This rule must handle all these variations while avoiding:
  // - Regular suru-verbs without お/ご: 電話する, 勉強します
  // - Regular verbs without お/ご: 閉めます, 呼びました
  // - お as non-humble prefix: お茶, ご飯

  r.either(
    // Pattern 1: お/ご prefix (接頭辞) + suru-verb + する (casual)
    // Example: ご確認する, お勉強する
    (b) => {
      const prefix = b.tok({
        textOneOf: ['お', 'ご'],
        tag: '接頭辞',
        dep: 'compound'
      }, 'prefix');

      const verb = b.verb({}, 'verb');

      const suru = b.tok({
        lemma: 'する',
        posOneOf: ['AUX', 'VERB'],  // GiNZA varies
        inflectionForm: '終止形-一般'
      }, 'suru');

      b.inOrder(prefix, verb, 2);
      b.auxOf(verb, suru);
      b.captureSpan('お-する', prefix, suru);
    },

    // Pattern 2: お/ご prefix (接頭辞) + verb + します (polite)
    // Example: お電話します, ご確認します, お持ちします
    (b) => {
      const prefix = b.tok({
        textOneOf: ['お', 'ご'],
        tag: '接頭辞',
        dep: 'compound'
      }, 'prefix');

      const stem = b.tok({
        posOneOf: ['VERB', 'NOUN'],
      }, 'stem');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般'
      }, 'masu');

      b.inOrder(prefix, stem, 2);
      b.auxOf(stem, shi);
      b.auxOf(stem, masu);
      b.captureSpan('お-する', prefix, masu);
    },

    // Pattern 3: お/ご prefix (接頭辞) + verb + しました (polite past)
    // Example: お閉めしました, お電話しました, お借りしました
    (b) => {
      const prefix = b.tok({
        textOneOf: ['お', 'ご'],
        tag: '接頭辞',
        dep: 'compound'
      }, 'prefix');

      const stem = b.tok({
        posOneOf: ['VERB', 'NOUN'],
      }, 'stem');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const mashita = b.aux({
        lemma: 'ます',
        inflectionForm: '連用形-一般'
      }, 'mashita');

      const ta = b.aux({
        lemma: 'た'
      }, 'ta');

      b.inOrder(prefix, stem, 2);
      b.auxOf(stem, shi);
      b.auxOf(stem, mashita);
      b.auxOf(stem, ta);
      b.inOrder(mashita, ta, 1);
      b.captureSpan('お-する', prefix, ta);
    },

    // Pattern 4: お/ご prefix (接頭辞) + verb + して (te-form)
    // Example: お呼びして, お渡しして
    (b) => {
      const prefix = b.tok({
        textOneOf: ['お', 'ご'],
        tag: '接頭辞',
        dep: 'compound'
      }, 'prefix');

      const stem = b.verb({}, 'stem');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const te = b.tok({
        text: 'て',
        pos: 'SCONJ'
      }, 'te');

      b.inOrder(prefix, stem, 2);
      b.auxOf(stem, shi);
      b.inOrder(shi, te, 1);
      b.captureSpan('お-する', prefix, te);
    },

    // Pattern 5: お/ご prefix (接頭辞) + verb + しましょう (volitional)
    // Example: お読みしましょう
    (b) => {
      const prefix = b.tok({
        textOneOf: ['お', 'ご'],
        tag: '接頭辞',
        dep: 'compound'
      }, 'prefix');

      const stem = b.verb({}, 'stem');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const mashou = b.aux({
        lemma: 'ます',
        inflectionForm: '意志推量形'
      }, 'mashou');

      b.inOrder(prefix, stem, 2);
      b.auxOf(stem, shi);
      b.auxOf(stem, mashou);
      b.captureSpan('お-する', prefix, mashou);
    },

    // Pattern 6: お/ご prefix (接頭辞) + verb + した (casual past)
    // Example: 彼にお会いした
    (b) => {
      const prefix = b.tok({
        textOneOf: ['お', 'ご'],
        tag: '接頭辞',
        dep: 'compound'
      }, 'prefix');

      const stem = b.verb({}, 'stem');

      const shi = b.tok({
        lemma: 'する',
        posOneOf: ['AUX', 'VERB'],
        inflectionForm: '連用形-一般'
      }, 'shi');

      const ta = b.aux({
        lemma: 'た'
      }, 'ta');

      b.inOrder(prefix, stem, 2);
      b.auxOf(stem, shi);
      b.auxOf(stem, ta);
      b.inOrder(shi, ta, 1);
      b.captureSpan('お-する', prefix, ta);
    },

    // Pattern 7: お/ご prefix (any tag) + verb + して (te-form)
    // For cases where GiNZA tags prefix as 感動詞-フィラー
    // Example: 先生をお呼びして
    (b) => {
      const prefix = b.tok({
        textOneOf: ['お', 'ご'],
        dep: 'compound'
      }, 'prefix');

      const stem = b.verb({}, 'stem');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const te = b.tok({
        text: 'て',
        pos: 'SCONJ'
      }, 'te');

      b.inOrder(prefix, stem, 2);
      b.auxOf(stem, shi);
      b.inOrder(shi, te, 1);
      b.captureSpan('お-する', prefix, te);
    },

    // Pattern 8: Merged compound starting with お/ご + します
    // Example: お守りします, お任せします, おとりします (hiragana)
    (b) => {
      const compound = b.tok({
        textRe: /^(お|ご)/,
        tagOneOf: ['名詞-普通名詞-サ変可能', '名詞-普通名詞-一般'],
      }, 'compound');

      const shi = b.tok({
        lemma: 'する',
        posOneOf: ['AUX', 'VERB'],  // GiNZA varies with punctuation
        inflectionForm: '連用形-一般'
      }, 'shi');

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般'
      }, 'masu');

      b.auxOf(compound, shi);
      b.auxOf(compound, masu);
      b.captureSpan('お-する', compound, masu);
    },

    // Pattern 9: VERB/ADP merged compound starting with お/ご + します
    // Example: おとりします, おやすみします, おしめします
    (b) => {
      const compound = b.tok({
        textRe: /^(お|ご)/,
        posOneOf: ['VERB', 'ADP'],
        tag: '名詞-普通名詞-一般',
      }, 'compound');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般'
      }, 'masu');

      b.auxOf(compound, shi);
      b.auxOf(compound, masu);
      b.captureSpan('お-する', compound, masu);
    },

    // Pattern 9b: VERB merged compound starting with お/ご + tag=動詞-一般 + して (te-form)
    // Example: 先生におよびして (hiragana "および" parsed as single token)
    (b) => {
      const compound = b.verb({
        textRe: /^(お|ご)/,
        tag: '動詞-一般',
      }, 'compound');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const te = b.tok({
        text: 'て',
        pos: 'SCONJ'
      }, 'te');

      b.auxOf(compound, shi);
      b.inOrder(shi, te, 1);
      b.captureSpan('お-する', compound, te);
    },

    // Pattern 10: VERB compound starting with お/ご + tag=動詞-非自立可能 + します
    // Example: おかりします
    (b) => {
      const compound = b.verb({
        textRe: /^(お|ご)/,
        tag: '動詞-非自立可能',
      }, 'compound');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般'
      }, 'masu');

      b.auxOf(compound, shi);
      b.auxOf(compound, masu);
      b.captureSpan('お-する', compound, masu);
    },

    // Pattern 11: Merged compound starting with お/ご + しました (past)
    // Example: お守りしました
    (b) => {
      const compound = b.tok({
        textRe: /^(お|ご)/,
        tagOneOf: ['名詞-普通名詞-サ変可能', '名詞-普通名詞-一般'],
      }, 'compound');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const mashita = b.aux({
        lemma: 'ます',
        inflectionForm: '連用形-一般'
      }, 'mashita');

      const ta = b.aux({
        lemma: 'た'
      }, 'ta');

      b.auxOf(compound, shi);
      b.auxOf(compound, mashita);
      b.auxOf(compound, ta);
      b.inOrder(mashita, ta, 1);
      b.captureSpan('お-する', compound, ta);
    },

    // Pattern 12: VERB/ADP merged compound starting with お/ご + しました (past)
    // Example: おとりしました, おやすみしました, おしめしました
    (b) => {
      const compound = b.tok({
        textRe: /^(お|ご)/,
        posOneOf: ['VERB', 'ADP'],
        tag: '名詞-普通名詞-一般',
      }, 'compound');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const mashita = b.aux({
        lemma: 'ます',
        inflectionForm: '連用形-一般'
      }, 'mashita');

      const ta = b.aux({
        lemma: 'た'
      }, 'ta');

      b.auxOf(compound, shi);
      b.auxOf(compound, mashita);
      b.auxOf(compound, ta);
      b.inOrder(mashita, ta, 1);
      b.captureSpan('お-する', compound, ta);
    },

    // Pattern 13: VERB compound starting with お/ご + tag=動詞-非自立可能 + しました (past)
    // Example: おかりしました
    (b) => {
      const compound = b.verb({
        textRe: /^(お|ご)/,
        tag: '動詞-非自立可能',
      }, 'compound');

      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const mashita = b.aux({
        lemma: 'ます',
        inflectionForm: '連用形-一般'
      }, 'mashita');

      const ta = b.aux({
        lemma: 'た'
      }, 'ta');

      b.auxOf(compound, shi);
      b.auxOf(compound, mashita);
      b.auxOf(compound, ta);
      b.inOrder(mashita, ta, 1);
      b.captureSpan('お-する', compound, ta);
    },

    // Pattern 14: VERB compound starting with お/ご + irrealis form + る + しました
    // Example: おかりしました (hiragana: おか + り + しました)
    (b) => {
      const prefix = b.verb({
        textRe: /^(お|ご)/,
        inflectionForm: '未然形-一般'
      }, 'prefix');

      const ru = b.aux({
        lemma: 'る',
        inflectionForm: '終止形-一般'
      }, 'ru');

      const shi = b.tok({
        lemma: 'する',
        posOneOf: ['AUX', 'VERB'],
        inflectionForm: '連用形-一般'
      }, 'shi');

      const mashita = b.aux({
        lemma: 'ます',
        inflectionForm: '連用形-一般'
      }, 'mashita');

      const ta = b.aux({
        lemma: 'た'
      }, 'ta');

      b.inOrder(prefix, ru, 1);
      b.inOrder(ru, shi, 1);
      b.auxOf(shi, mashita);
      b.auxOf(shi, ta);
      b.inOrder(mashita, ta, 1);
      b.captureSpan('お-する', prefix, ta);
    },

    // Pattern 15: ADP compound starting with お + します (where "し" is root VERB)
    // Example: ドアをおしめします (hiragana: おしめ is ADP, し is VERB root)
    (b) => {
      const compound = b.tok({
        textRe: /^お/,
        pos: 'ADP',
        tag: '名詞-普通名詞-一般',
      }, 'compound');

      const shi = b.verb({
        lemma: 'する',
        inflectionForm: '連用形-一般'
      }, 'shi');

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般'
      }, 'masu');

      b.inOrder(compound, shi, 1);
      b.auxOf(shi, masu);
      b.captureSpan('お-する', compound, masu);
    }
  );
});
