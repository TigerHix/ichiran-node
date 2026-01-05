import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('のがすき', (r) => {
  // Pattern: Verb (dictionary form) + の + が + 好き(すき)
  // Meaning: "Like doing, love doing" something
  // Examples:
  // - 本を読むのが好き (I like reading books)
  // - 映画を見るのが好き (I like watching movies)
  // - 私はラーメンを食べるのが好きです (I like eating ramen)

  r.either(
    // Branch 1: Verb + の + が + 好き (casual)
    (b1) => {
      const verb = b1.verb({}, 'verb');

      const no = b1.tok(
        {
          text: 'の',
          tag: '助詞-準体助詞',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'no'
      );

      const ga = b1.particle('が', 'ga', {
        tag: '助詞-格助詞',
        dep: 'case',
      });

      const suki = b1.tok(
        {
          lemmaOneOf: ['すき', '好き'],
          posOneOf: ['NOUN', 'ADJ', 'VERB'],
        },
        'suki'
      );

      b1.inOrder(verb, no, 2);
      b1.inOrder(no, ga, 1);
      b1.inOrder(ga, suki, 1);

      b1.captureSpan('のがすき', verb, suki);
    },
    // Branch 2: Verb + の + が + 好き + です (polite)
    (b2) => {
      const verb = b2.verb({}, 'verb');

      const no = b2.tok(
        {
          text: 'の',
          tag: '助詞-準体助詞',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'no'
      );

      const ga = b2.particle('が', 'ga', {
        tag: '助詞-格助詞',
        dep: 'case',
      });

      const suki = b2.tok(
        {
          lemmaOneOf: ['すき', '好き'],
          posOneOf: ['NOUN', 'ADJ', 'VERB'],
        },
        'suki'
      );

      const desu = b2.aux({
        lemma: 'です',
      }, 'desu');

      b2.inOrder(verb, no, 2);
      b2.inOrder(no, ga, 1);
      b2.inOrder(ga, suki, 1);
      b2.inOrder(suki, desu, 3); // Allow some distance for sentence particles

      b2.captureSpan('のがすき', verb, desu);
    },
    // Branch 3: Verb + の + が + 好き + だ (casual copula)
    (b3) => {
      const verb = b3.verb({}, 'verb');

      const no = b3.tok(
        {
          text: 'の',
          tag: '助詞-準体助詞',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'no'
      );

      const ga = b3.particle('が', 'ga', {
        tag: '助詞-格助詞',
        dep: 'case',
      });

      const suki = b3.tok(
        {
          lemmaOneOf: ['すき', '好き'],
          posOneOf: ['NOUN', 'ADJ', 'VERB'],
        },
        'suki'
      );

      const da = b3.aux({
        lemma: 'だ',
      }, 'da');

      b3.inOrder(verb, no, 2);
      b3.inOrder(no, ga, 1);
      b3.inOrder(ga, suki, 1);
      b3.inOrder(suki, da, 3);

      b3.captureSpan('のがすき', verb, da);
    },
    // Branch 4: Verb + の + が + 好き + でした (past polite)
    (b4) => {
      const verb = b4.verb({}, 'verb');

      const no = b4.tok(
        {
          text: 'の',
          tag: '助詞-準体助詞',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'no'
      );

      const ga = b4.particle('が', 'ga', {
        tag: '助詞-格助詞',
        dep: 'case',
      });

      const suki = b4.tok(
        {
          lemmaOneOf: ['すき', '好き'],
          posOneOf: ['NOUN', 'ADJ', 'VERB'],
        },
        'suki'
      );

      const deshita = b4.aux({
        lemma: 'でした',
      }, 'deshita');

      b4.inOrder(verb, no, 2);
      b4.inOrder(no, ga, 1);
      b4.inOrder(ga, suki, 1);
      b4.inOrder(suki, deshita, 3);

      b4.captureSpan('のがすき', verb, deshita);
    },
    // Branch 5: Verb + の + が + 好き + だった (past casual)
    (b5) => {
      const verb = b5.verb({}, 'verb');

      const no = b5.tok(
        {
          text: 'の',
          tag: '助詞-準体助詞',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'no'
      );

      const ga = b5.particle('が', 'ga', {
        tag: '助詞-格助詞',
        dep: 'case',
      });

      const suki = b5.tok(
        {
          lemmaOneOf: ['すき', '好き'],
          posOneOf: ['NOUN', 'ADJ', 'VERB'],
        },
        'suki'
      );

      const datta = b5.aux({
        lemma: 'だった',
      }, 'datta');

      b5.inOrder(verb, no, 2);
      b5.inOrder(no, ga, 1);
      b5.inOrder(ga, suki, 1);
      b5.inOrder(suki, datta, 3);

      b5.captureSpan('のがすき', verb, datta);
    }
  );
});
