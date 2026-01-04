import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: が気になる - "to be interested in, to be concerned about"
 *
 * Matches: noun/clause + が + 気 + に + なる
 *
 * This expression highlights when something has become the focus of attention
 * or interest. It indicates that something is weighing on one's mind or that
 * one is concerned about it.
 *
 * Structure variants:
 * - Noun + が気になる (casual, present)
 * - Noun + が気になった (casual, past)
 * - Noun + が気になります (polite, present)
 * - Noun + が気になりました (polite, past)
 * - Noun + が気になっている (progressive/state)
 *
 * Examples:
 * - 結果が気になる。 (I'm concerned about the results.)
 * - 彼の言葉が気になっている。 (His words are weighing on my mind.)
 * - 家自体は良いと思うんですけど、やっぱり値段が気になります。
 *
 * GiNZA parse structure (for "結果が気になる"):
 * - 結果(NOUN) --case--> が(ADP)
 * - 気(NOUN) --obl--> なる(VERB)
 * - なる --case--> に(ADP)
 *
 * Also handles hiragana "きになる" where GiNZA may parse "き" with lemma="くる"
 * and merged "がき" tokens
 */
export default linguisticRule('が気になる', (r) => {
  r.either(
    // Branch 1: Casual present (〜が気になる)
    (b) => {
      const topic = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'topic');
      const ga = b.particle('が', 'ga');
      b.caseMarker(topic, ga);

      const ki = b.tok({
        lemmaOneOf: ['気', 'き', 'くる'],
        posOneOf: ['NOUN'],
      }, 'ki');
      b.inOrder(ga, ki, 5);

      const ni = b.particle('に', 'ni');
      b.inOrder(ki, ni, 1);

      const naru = b.verb({
        lemma: 'なる',
        inflectionForm: '終止形-一般',
      }, 'naru');
      b.inOrder(ni, naru, 3);

      b.captureSpan('が気になる', topic, naru);
    },
    // Branch 2: Merged "がき" token - casual present (〜がきになる)
    // GiNZA sometimes tokenizes "がき" as a single noun
    (b) => {
      const topic = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['compound', 'nmod', 'nsubj', 'obj', 'obl'],
      }, 'topic');

      const gaki = b.noun({ lemma: 'がき' }, 'gaki');
      b.inOrder(topic, gaki, 5);

      const ni = b.particle('に', 'ni');
      b.inOrder(gaki, ni, 1);

      const naru = b.verb({
        lemma: 'なる',
        inflectionForm: '終止形-一般',
      }, 'naru');
      b.inOrder(ni, naru, 3);

      b.captureSpan('が気になる', topic, naru);
    },
    // Branch 3: Polite present (〜が気になります)
    (b) => {
      const topic = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'topic');
      const ga = b.particle('が', 'ga');
      b.caseMarker(topic, ga);

      const ki = b.tok({
        lemmaOneOf: ['気', 'き', 'くる'],
        posOneOf: ['NOUN'],
      }, 'ki');
      b.inOrder(ga, ki, 5);

      const ni = b.particle('に', 'ni');
      b.inOrder(ki, ni, 1);

      const nari = b.verb({
        lemma: 'なる',
        inflectionForm: '連用形-一般',
      }, 'nari');
      b.inOrder(ni, nari, 3);

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般',
      }, 'masu');
      b.auxOf(nari, masu);

      b.captureSpan('が気になる', topic, masu);
    },
    // Branch 4: Merged "がき" token - polite present (〜がきになります)
    (b) => {
      const topic = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['compound', 'nmod', 'nsubj', 'obj', 'obl'],
      }, 'topic');

      const gaki = b.noun({ lemma: 'がき' }, 'gaki');
      b.inOrder(topic, gaki, 5);

      const ni = b.particle('に', 'ni');
      b.inOrder(gaki, ni, 1);

      const nari = b.verb({
        lemma: 'なる',
        inflectionForm: '連用形-一般',
      }, 'nari');
      b.inOrder(ni, nari, 3);

      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般',
      }, 'masu');
      b.auxOf(nari, masu);

      b.captureSpan('が気になる', topic, masu);
    },
    // Branch 5: Progressive/state - casual (〜が気になっている)
    (b) => {
      const topic = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'topic');
      const ga = b.particle('が', 'ga');
      b.caseMarker(topic, ga);

      const ki = b.tok({
        lemmaOneOf: ['気', 'き', 'くる'],
        posOneOf: ['NOUN'],
      }, 'ki');
      b.inOrder(ga, ki, 5);

      const ni = b.particle('に', 'ni');
      b.inOrder(ki, ni, 1);

      const nat = b.verb({
        lemma: 'なる',
        inflectionForm: '連用形-促音便',
      }, 'nat');
      b.inOrder(ni, nat, 1);

      const te = b.tok({ lemma: 'て' }, 'te');
      b.auxOf(nat, te);

      const iru = b.aux({ lemma: 'いる' }, 'iru');
      b.auxOf(nat, iru);

      b.captureSpan('が気になる', topic, iru);
    },
    // Branch 6: Merged "がき" token - casual progressive (〜がきになっている)
    (b) => {
      const topic = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['compound', 'nmod', 'nsubj', 'obj', 'obl'],
      }, 'topic');

      const gaki = b.noun({ lemma: 'がき' }, 'gaki');
      b.inOrder(topic, gaki, 5);

      const ni = b.particle('に', 'ni');
      b.inOrder(gaki, ni, 1);

      const nat = b.verb({
        lemma: 'なる',
        inflectionForm: '連用形-促音便',
      }, 'nat');
      b.inOrder(ni, nat, 1);

      const te = b.tok({ lemma: 'て' }, 'te');
      b.auxOf(nat, te);

      const iru = b.aux({ lemma: 'いる' }, 'iru');
      b.auxOf(nat, iru);

      b.captureSpan('が気になる', topic, iru);
    }
  );
});
