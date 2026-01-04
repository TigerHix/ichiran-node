import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('てはいけない', (r) => {
  r.either(
    // Pattern 1a: Standard form - verb[te] + て/で + は + いけない (casual, te and wa separate)
    // e.g., 入ってはいけない, 行ってはいけない, 食べてはいけない, やすんではいけない
    (b) => {
      // Te-form verb (any verb in conjunctive form)
      const teVerb = b.verb({}, 'teVerb');

      // Te-form marker: て or で
      const teOrDe = b.tok(
        {
          textOneOf: ['て', 'で'],
          pos: 'SCONJ',
        },
        'teOrDe'
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

      // Negative auxiliary: ない (can have dep=fixed or dep=aux)
      const nai = b.tok(
        {
          text: 'ない',
          pos: 'AUX',
          lemma: 'ない',
        },
        'nai'
      );

      // Structure constraints
      b.headChild(teVerb, teOrDe, 'mark');
      b.inOrder(teVerb, teOrDe, 1);
      b.inOrder(teOrDe, wa, 1);
      b.inOrder(wa, ike, 2);
      b.inOrder(ike, nai, 1);

      // Capture from te-form verb through ikenai
      b.captureSpan('てはいけない', teVerb, nai);
    },

    // Pattern 1b: Standard form - verb[te] + ては + いけない (casual, te+wa combined)
    // e.g., 食べてはいけない (sometimes parsed as single token)
    (b) => {
      // Te-form verb (any verb in conjunctive form)
      const teVerb = b.verb({}, 'teVerb');

      // Combined te-wa marker: ては or では
      const teaWa = b.tok(
        {
          textOneOf: ['ては', 'では'],
          pos: 'SCONJ',
        },
        'teaWa'
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
      b.headChild(teVerb, teaWa, 'mark');
      b.inOrder(teVerb, teaWa, 1);
      b.inOrder(teaWa, ike, 1);
      b.inOrder(ike, nai, 1);

      // Capture from te-form verb through ikenai
      b.captureSpan('てはいけない', teVerb, nai);
    },

    // Pattern 2a: Polite form - verb[te] + て/で + は + いけません (polite, te and wa separate)
    // e.g., 飲んではいけません, 泳いではいけません
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

      // Structure constraints: ensure proper ordering
      b.inOrder(teVerb, teOrDe, 1);
      b.inOrder(teOrDe, wa, 1);
      b.inOrder(wa, ike, 2);
      b.auxOf(ike, mase);

      // Capture from te-form verb through ikemase(n)
      b.captureSpan('てはいけない', teVerb, mase);
    },

    // Pattern 3: Contracted form - verb[te] + ちゃ + いけない
    // e.g., 行っちゃいけない, 入っちゃいけない
    (b) => {
      // Te-form verb
      const teVerb = b.verb({}, 'teVerb');

      // Contracted te-wa: ちゃ or じゃ
      const cha = b.tok(
        {
          textOneOf: ['ちゃ', 'じゃ'],
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
      b.headChild(teVerb, cha, 'mark');
      b.inOrder(teVerb, cha, 1);
      b.inOrder(cha, ike, 1);
      b.inOrder(ike, nai, 1);

      // Capture from te-form verb through ikenai
      b.captureSpan('てはいけない', teVerb, nai);
    },

    // Pattern 4: Contracted polite form - verb[te] + ちゃ + いけません
    // e.g., 行っちゃいけません
    (b) => {
      // Te-form verb
      const teVerb = b.verb({}, 'teVerb');

      // Contracted te-wa: ちゃ or じゃ
      const cha = b.tok(
        {
          textOneOf: ['ちゃ', 'じゃ'],
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
      b.headChild(teVerb, cha, 'mark');
      b.inOrder(teVerb, cha, 1);
      b.inOrder(cha, ike, 1);
      b.auxOf(ike, mase);

      // Capture from te-form verb through ikemasen
      b.captureSpan('てはいけない', teVerb, mase);
    }
  );
});
