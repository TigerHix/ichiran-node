import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('おそらく', (r) => {
  // おそらく (osoraku) - adverb expressing probability, "probably, perhaps, likely"
  // A formal adverb used to express likelihood, often paired with conjecture forms
  // like だろう, でしょう, かもしれない, etc.
  //
  // Pattern: おそらく + phrase (sentence, clause, or noun phrase)
  // - おそらく明日雨が降るだろう - It will probably rain tomorrow
  // - おそらく彼は来ない - He probably won't come
  // - おそらく彼が犯人だろう - He's probably the culprit
  // - おそらく11時ごろ帰ってこない - She probably won't be back until around 11

  const osoraku = r.adv({
    textOneOf: ['おそらく', '恐らく'],
  }, 'osoraku');

  // Main pattern: おそらく followed by any predicate, clause, or phrase
  // Since おそらく is an adverb that modifies entire sentences/clauses,
  // we need to capture what follows it
  r.either(
    // Pattern 1: おそらく + verb (with or without auxiliaries)
    // おそらく明日雨が降る, おそらく彼は来ない, おそらく帰ってこない
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(osoraku, verb, 10);
      b.captureSpan('おそらく', osoraku, verb);
    },

    // Pattern 2: おそらく + auxiliary (だ/でしょう/だろう/かもしれない etc.)
    // This captures conjecture endings
    (b) => {
      const aux = b.aux({}, 'aux');
      b.inOrder(osoraku, aux, 10);
      b.captureSpan('おそらく', osoraku, aux);
    },

    // Pattern 3: おそらく + adjective
    // (less common but possible)
    (b) => {
      const adj = b.adj({}, 'adj');
      b.inOrder(osoraku, adj, 10);
      b.captureSpan('おそらく', osoraku, adj);
    },

    // Pattern 4: おそらく + noun (when used as sentence starter)
    // おそらく明日, おそらく彼
    (b) => {
      const noun = b.noun({}, 'noun');
      b.inOrder(osoraku, noun, 5);
      b.captureSpan('おそらく', osoraku, noun);
    }
  );
});
