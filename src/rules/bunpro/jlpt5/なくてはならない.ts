import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('なくてはならない', (r) => {
  r.either(
    // Pattern 1: Standard form - verb[nai-ren'you] + なく + て + は + ならない (casual)
    // e.g., 飲まなくてはならない, 行かなくてはならない, しなくてはならない
    (b) => {
      // Negative auxiliary in conjunctive form: なく (from ない)
      const naku = b.aux(
        {
          text: 'なく',
          lemma: 'ない',
          inflectionForm: '連用形-一般',
        },
        'naku'
      );

      // Te-form marker: て
      const te = b.tok(
        {
          text: 'て',
          pos: 'SCONJ',
        },
        'te'
      );

      // Topic particle は
      const wa = b.particle('は', 'wa');

      // Verb naru in negative form: なら (未然形 before ない)
      const nara = b.verb(
        {
          lemma: 'なる',
          inflectionForm: '未然形-一般',
        },
        'nara'
      );

      // Negative auxiliary: ない
      const nai = b.tok(
        {
          text: 'ない',
          pos: 'AUX',
          lemma: 'ない',
        },
        'nai'
      );

      // Structure constraints
      b.inOrder(naku, te, 1);
      b.inOrder(te, wa, 1);
      b.inOrder(wa, nara, 2);
      b.inOrder(nara, nai, 1);

      // Capture from naku through naranai
      b.captureSpan('なくてはならない', naku, nai);
    },

    // Pattern 2: Polite form - verb[nai-ren'you] + なく + て + は + なりません (polite)
    // e.g., 勉強しなくてはなりません
    (b) => {
      // Negative auxiliary in conjunctive form: なく
      const naku = b.aux(
        {
          text: 'なく',
          lemma: 'ない',
          inflectionForm: '連用形-一般',
        },
        'naku'
      );

      // Te-form marker: て
      const te = b.tok(
        {
          text: 'て',
          posOneOf: ['SCONJ', 'ADP'],
        },
        'te'
      );

      // Topic particle は
      const wa = b.particle('は', 'wa');

      // Verb naru in polite form: なり (連用形 before ません)
      const nari = b.verb(
        {
          lemma: 'なる',
          inflectionForm: '連用形-一般',
        },
        'nari'
      );

      // Polite negative auxiliary: ませ (first part of ません)
      const mase = b.aux(
        {
          text: 'ませ',
          lemma: 'ます',
          inflectionForm: '未然形-一般',
        },
        'mase'
      );

      // Structure constraints
      b.inOrder(naku, te, 1);
      b.inOrder(te, wa, 1);
      b.inOrder(wa, nari, 2);
      b.auxOf(nari, mase);

      // Capture from naku through narimasen
      b.captureSpan('なくてはならない', naku, mase);
    },

    // Pattern 3: Contracted form - verb[nai-ren'you] + なく + ちゃ + ならない
    // e.g., 行かなくちゃならない
    (b) => {
      // Negative auxiliary in conjunctive form: なく
      const naku = b.aux(
        {
          text: 'なく',
          lemma: 'ない',
          inflectionForm: '連用形-一般',
        },
        'naku'
      );

      // Contracted te-wa: ちゃ
      const cha = b.tok(
        {
          text: 'ちゃ',
          pos: 'SCONJ',
        },
        'cha'
      );

      // Verb naru in negative form: なら
      const nara = b.verb(
        {
          lemma: 'なる',
          inflectionForm: '未然形-一般',
        },
        'nara'
      );

      // Negative auxiliary: ない
      const nai = b.tok(
        {
          text: 'ない',
          pos: 'AUX',
          lemma: 'ない',
        },
        'nai'
      );

      // Structure constraints
      b.inOrder(naku, cha, 1);
      b.inOrder(cha, nara, 1);
      b.inOrder(nara, nai, 1);

      // Capture from naku through naranai
      b.captureSpan('なくてはならない', naku, nai);
    },

    // Pattern 4: Contracted polite form - verb[nai-ren'you] + なく + ちゃ + なりません
    // e.g., 行かなくちゃなりません
    (b) => {
      // Negative auxiliary in conjunctive form: なく
      const naku = b.aux(
        {
          text: 'なく',
          lemma: 'ない',
          inflectionForm: '連用形-一般',
        },
        'naku'
      );

      // Contracted te-wa: ちゃ
      const cha = b.tok(
        {
          text: 'ちゃ',
          pos: 'SCONJ',
        },
        'cha'
      );

      // Verb naru in polite form: なり (連用形)
      const nari = b.verb(
        {
          lemma: 'なる',
          inflectionForm: '連用形-一般',
        },
        'nari'
      );

      // Polite negative auxiliary: ませ
      const mase = b.aux(
        {
          text: 'ませ',
          lemma: 'ます',
          inflectionForm: '未然形-一般',
        },
        'mase'
      );

      // Structure constraints
      b.inOrder(naku, cha, 1);
      b.inOrder(cha, nari, 1);
      b.auxOf(nari, mase);

      // Capture from naku through narimasen
      b.captureSpan('なくてはならない', naku, mase);
    }
  );
});
