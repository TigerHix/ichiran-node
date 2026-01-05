import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('てはならない', (r) => {
  r.either(
    // Pattern 1a: Standard form - verb[te] + て/で + は + ならない (casual, te and wa separate)
    // e.g., 泳いではならない, 運転してはならない, 吸ってはならない, 飲んではならない
    (b) => {
      // Te-form verb (should be VERB, NOUN, or AUX)
      const teVerb = b.tok(
        {
          posOneOf: ['VERB', 'NOUN', 'AUX'],
        },
        'teVerb'
      );

      // Te-form marker: て or で (can be SCONJ or ADP depending on parse)
      const teOrDe = b.tok(
        {
          textOneOf: ['て', 'で'],
          posOneOf: ['SCONJ', 'ADP'],
        },
        'teOrDe'
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
      b.inOrder(teVerb, teOrDe, 5); // Allow up to 5 tokens between teVerb and teOrDe
      b.inOrder(teOrDe, wa, 5); // Allow more distance for cases where particles intervene
      b.inOrder(wa, nara, 2);
      b.inOrder(nara, nai, 1);

      // Capture from te-form verb through naranai
      b.captureSpan('てはならない', teVerb, nai);
    },

    // Pattern 1b: Combined form - verb[te-wa] + ならない (casual, te+wa combined)
    // e.g., 飲んではならない where "のんでは" or "のでは" is a single token
    (b) => {
      // Te-form + wa combined (e.g., のんでは, しては)
      const teWa = b.tok(
        {
          textEndsWith: 'ては',
        },
        'teWa'
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
      b.inOrder(teWa, nara, 1);
      b.inOrder(nara, nai, 1);

      // Capture from te-wa through naranai
      b.captureSpan('てはならない', teWa, nai);
    },

    // Pattern 1c: Combined form with de (e.g., のんでは, んでは)
    (b) => {
      // De-form + wa combined (e.g., んでは, んでは)
      const deWa = b.tok(
        {
          textEndsWith: 'では',
        },
        'deWa'
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
      b.inOrder(deWa, nara, 1);
      b.inOrder(nara, nai, 1);

      // Capture from de-wa through naranai
      b.captureSpan('てはならない', deWa, nai);
    },

    // Pattern 2a: Polite form - verb[te] + て/で + は + なりません (polite, te and wa separate)
    // e.g., 飲んではなりません, 泳いではなりません
    (b) => {
      // Te-form verb
      const teVerb = b.tok(
        {
          posOneOf: ['VERB', 'NOUN'], // Sometimes giNZA parses te-form verbs as NOUN
        },
        'teVerb'
      );

      // Te-form marker: て or で
      const teOrDe = b.tok(
        {
          textOneOf: ['て', 'で'],
          posOneOf: ['SCONJ', 'ADP'],
        },
        'teOrDe'
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

      // Structure constraints: ensure proper ordering
      b.inOrder(teVerb, teOrDe, 1);
      b.inOrder(teOrDe, wa, 1);
      b.inOrder(wa, nari, 2);
      b.auxOf(nari, mase);

      // Capture from te-form verb through narimasen
      b.captureSpan('てはならない', teVerb, mase);
    },

    // Pattern 3: Contracted form - verb[te] + ちゃ/じゃ + ならない
    // e.g., 行っちゃならない (though not in test data, included for completeness)
    (b) => {
      // Te-form verb (sometimes parsed as NOUN)
      const teVerb = b.tok(
        {
          posOneOf: ['VERB', 'NOUN'],
        },
        'teVerb'
      );

      // Contracted te-wa: ちゃ or じゃ
      const cha = b.tok(
        {
          textOneOf: ['ちゃ', 'じゃ'],
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
      b.inOrder(teVerb, cha, 1);
      b.inOrder(cha, nara, 1);
      b.inOrder(nara, nai, 1);

      // Capture from te-form verb through naranai
      b.captureSpan('てはならない', teVerb, nai);
    }
  );
});
