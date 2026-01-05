import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('なくてはいけない', (r) => {
  r.either(
    // Pattern 1a: Standard form - verb[nai-ren'you] + なく + て + は + いけない (casual, separate tokens)
    // e.g., 飲まなくてはいけない, 行かなくてはいけない, しなくてはいけない
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

      // Potential form of iku in negative form: いけ
      const ike = b.verb(
        {
          lemma: 'いける',
          inflectionForm: '未然形-一般',
        },
        'ike'
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
      b.inOrder(wa, ike, 2);
      b.inOrder(ike, nai, 1);

      // Capture from naku through ikenai
      b.captureSpan('なくてはいけない', naku, nai);
    },

    // Pattern 2a: Polite form - verb[nai-ren'you] + なく + て + は + いけません (polite, separate tokens)
    // e.g., 勉強しなくてはいけません
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

      // Potential form of iku in polite form: いけ (連用形 before ません)
      const ike = b.verb(
        {
          lemma: 'いける',
          inflectionForm: '連用形-一般',
        },
        'ike'
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
      b.inOrder(wa, ike, 2);
      b.auxOf(ike, mase);

      // Capture from naku through ikemase(n)
      b.captureSpan('なくてはいけない', naku, mase);
    },

    // Pattern 3: Contracted form - verb[nai-ren'you] + なく + ちゃ + いけない
    // e.g., 行かなくちゃいけない
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

      // Potential form of iku in negative form: いけ
      const ike = b.verb(
        {
          lemma: 'いける',
          inflectionForm: '未然形-一般',
        },
        'ike'
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
      b.inOrder(cha, ike, 1);
      b.inOrder(ike, nai, 1);

      // Capture from naku through ikenai
      b.captureSpan('なくてはいけない', naku, nai);
    },

    // Pattern 4: Contracted polite form - verb[nai-ren'you] + なく + ちゃ + いけません
    // e.g., 行かなくちゃいけません
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

      // Potential form of iku in polite form: いけ (連用形)
      const ike = b.verb(
        {
          lemma: 'いける',
          inflectionForm: '連用形-一般',
        },
        'ike'
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
      b.inOrder(cha, ike, 1);
      b.auxOf(ike, mase);

      // Capture from naku through ikemasen
      b.captureSpan('なくてはいけない', naku, mase);
    },

    // Pattern 5: Casual form with dame - verb[nai-ren'you] + なく + て + は + ダメ
    // e.g., 行かなくてはダメ, 食べなくてはダメ
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
          pos: 'SCONJ',
        },
        'te'
      );

      // Topic particle は
      const wa = b.particle('は', 'wa');

      // Dame (casual prohibition/obligation): ダメ or だめ
      const dame = b.tok(
        {
          textOneOf: ['ダメ', 'だめ'],
        },
        'dame'
      );

      // Structure constraints
      b.inOrder(naku, te, 1);
      b.inOrder(te, wa, 1);
      b.inOrder(wa, dame, 1);

      // Capture from naku through dame
      b.captureSpan('なくてはいけない', naku, dame);
    },

    // Pattern 6: Casual contracted form with dame - verb[nai-ren'you] + なく + ちゃ + ダメ
    // e.g., 行かなくちゃダメ, 食べなくちゃダメ
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

      // Dame (casual prohibition/obligation): ダメ or だめ
      const dame = b.tok(
        {
          textOneOf: ['ダメ', 'だめ'],
        },
        'dame'
      );

      // Structure constraints
      b.inOrder(naku, cha, 1);
      b.inOrder(cha, dame, 1);

      // Capture from naku through dame
      b.captureSpan('なくてはいけない', naku, dame);
    },

    // Pattern 7: Formal alternative with narau - verb[nai-ren'you] + なく + て + は + ならない
    // e.g., 行かなくてはならない, 勉強しなくてはならない
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
          pos: 'SCONJ',
        },
        'te'
      );

      // Topic particle は
      const wa = b.particle('は', 'wa');

      // Naranai (formal obligation): なら
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
      b.captureSpan('なくてはいけない', naku, nai);
    }
  );
});
