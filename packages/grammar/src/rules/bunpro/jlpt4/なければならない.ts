import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なければならない', (r) => {
  r.either(
    // Pattern 1: Standard form - verb[nai-condition] + ならない (casual)
    // e.g., しなければならない, 行かなければならない, 買わなければならない
    // GiNZA parses: なけれ (AUX, lemma=ない, infl=仮定形-一般) + ば (SCONJ) + なら (VERB, lemma=なる, infl=未然形) + ない (AUX)
    (b) => {
      // Negative auxiliary in conditional form: なけれ (before ば)
      const nakere = b.aux({
        lemma: 'ない',
        inflectionForm: '仮定形-一般',
      }, 'nakere');

      // Conditional particle: ば
      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      // Verb naru in negative form: なら (未然形 before ない)
      const nara = b.verb({
        lemma: 'なる',
        inflectionForm: '未然形-一般',
      }, 'nara');

      // Negative auxiliary: ない
      const nai = b.tok({
        text: 'ない',
        posOneOf: ['AUX', 'VERB'],
        lemma: 'ない',
      }, 'nai');

      // Structure constraints
      b.inOrder(nakere, ba, 1);
      b.inOrder(ba, nara, 2);
      b.inOrder(nara, nai, 1);

      // Capture from nakere through naranai
      b.captureSpan('なければならない', nakere, nai);
    },

    // Pattern 2: Polite form - verb[nai-condition] + なりません (polite)
    // e.g., しなければなりません, 行かなければなりません
    (b) => {
      // Negative auxiliary in conditional form: なけれ
      const nakere = b.aux({
        lemma: 'ない',
        inflectionForm: '仮定形-一般',
      }, 'nakere');

      // Conditional particle: ば
      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      // Verb naru in polite form: なり (連用形 before ません)
      const nari = b.verb({
        lemma: 'なる',
        inflectionForm: '連用形-一般',
      }, 'nari');

      // Polite negative auxiliary: ませ (first part of ません)
      const mase = b.aux({
        text: 'ませ',
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');

      // Structure constraints
      b.inOrder(nakere, ba, 1);
      b.inOrder(ba, nari, 2);
      b.auxOf(nari, mase);

      // Capture from nakere through narimasen
      b.captureSpan('なければならない', nakere, mase);
    },

    // Pattern 3: Past tense form - verb[nai-condition] + ならなかった (casual past)
    // e.g., 返さなければならなかった, 歩かなければならなかった
    // GiNZA parses as: なけれ(AUX) + ば(SCONJ) + なら(VERB) + なかっ(AUX) + た(AUX)
    (b) => {
      // Negative auxiliary in conditional form: なけれ
      const nakere = b.aux({
        lemma: 'ない',
        inflectionForm: '仮定形-一般',
      }, 'nakere');

      // Conditional particle: ば
      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      // Verb naru in negative form: なら (未然形)
      const nara = b.verb({
        lemma: 'なる',
        inflectionForm: '未然形-一般',
      }, 'nara');

      // Past tense auxiliary: なかっ (連用形-促音便)
      const nakat = b.aux({
        lemma: 'ない',
        inflectionForm: '連用形-促音便',
      }, 'nakat');

      // Past tense marker: た
      const ta = b.aux({
        lemma: 'た',
        inflectionForm: '終止形-一般',
      }, 'ta');

      // Structure constraints
      b.inOrder(nakere, ba, 1);
      b.inOrder(ba, nara, 2);
      b.inOrder(nara, nakat, 1);
      b.inOrder(nakat, ta, 1);

      // Capture from nakere through naranakatta
      b.captureSpan('なければならない', nakere, ta);
    },

    // Pattern 4: Past polite form - verb[nai-condition] + なりませんでした (polite past)
    // e.g., 返さなければなりませんでした, 歩かなければなりませんでした
    (b) => {
      // Negative auxiliary in conditional form: なけれ
      const nakere = b.aux({
        lemma: 'ない',
        inflectionForm: '仮定形-一般',
      }, 'nakere');

      // Conditional particle: ば
      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      // Verb naru in polite form: なり (連用形)
      const nari = b.verb({
        lemma: 'なる',
        inflectionForm: '連用形-一般',
      }, 'nari');

      // Polite auxiliary: ません (past form)
      const masen = b.aux({
        textOneOf: ['ませんでした', 'ませんだ'],
        lemma: 'ます',
      }, 'masen');

      // Structure constraints
      b.inOrder(nakere, ba, 1);
      b.inOrder(ba, nari, 2);
      b.auxOf(nari, masen);

      // Capture from nakere through narimasendeshita
      b.captureSpan('なければならない', nakere, masen);
    },

    // Pattern 5: Contracted form - verb[nai-stem] + きゃ + ならない
    // e.g., やらなきゃならない
    // GiNZA parses: なきゃ (AUX, lemma=ない, infl=仮定形-融合)
    (b) => {
      // Contracted conditional form: なきゃ (fused form)
      const nakya = b.aux({
        text: 'なきゃ',
        lemma: 'ない',
        inflectionForm: '仮定形-融合',
      }, 'nakya');

      // Verb naru in negative form: なら (未然形)
      const nara = b.verb({
        lemma: 'なる',
        inflectionForm: '未然形-一般',
      }, 'nara');

      // Negative auxiliary: ない
      const nai = b.tok({
        text: 'ない',
        posOneOf: ['AUX', 'VERB'],
        lemma: 'ない',
      }, 'nai');

      // Structure constraints
      b.inOrder(nakya, nara, 1);
      b.inOrder(nara, nai, 1);

      // Capture from nakya through naranai
      b.captureSpan('なければならない', nakya, nai);
    },

    // Pattern 6: Contracted form without naranai - just verb + なきゃ
    // e.g., 帰らなきゃ, しなきゃ (shortened conversational form)
    // GiNZA parses: verb(未然形) + なきゃ (AUX, lemma=ない, infl=仮定形-融合)
    (b) => {
      // Contracted conditional form: なきゃ
      const nakya = b.aux({
        text: 'なきゃ',
        lemma: 'ない',
        inflectionForm: '仮定形-融合',
      }, 'nakya');

      // Capture from nakya
      b.capture(nakya);
    }
  );
});
