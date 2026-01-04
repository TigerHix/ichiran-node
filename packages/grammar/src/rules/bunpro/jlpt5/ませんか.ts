import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ませんか', (r) => {
  // ませんか (masen ka): won't you, would you not
  // Polite expression for directly inviting someone to do something
  // Verb stem + ませんか
  //
  // GiNZA parsing notes:
  // - GiNZA splits "ません" into two tokens: ませ (AUX, lemma=ます) + ん (AUX, lemma=ぬ)
  // - The particle か typically has dep=mark
  // - Suru-verbs are parsed as noun + し (AUX, lemma=する)

  r.either(
    // Pattern 1: Regular verbs with stem form (polite)
    // GiNZA parses: verb (連用形) + ませ (未然形 of ます) + ん (negation) + か
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const mase = b1.aux({
        text: 'ませ',
        lemma: 'ます',
      }, 'mase');
      const nun = b1.aux({
        text: 'ん',
        lemma: 'ぬ',
      }, 'nun');
      const ka = b1.particle('か', 'ka', { dep: 'mark' });

      b1.auxOf(verb, mase);
      b1.inOrder(mase, nun, 1);
      b1.inOrder(nun, ka, 1);
      b1.captureSpan('ませんか', verb, ka);
    },

    // Pattern 2: Suru-verbs (noun verb + し + ませんか)
    // GiNZA parses: noun + し (AUX, lemma=する) + ませ + ん + か
    (b2) => {
      const nounVerb = b2.tok({}, 'verb');
      const shi = b2.aux({
        lemma: 'する',
      }, 'shi');
      const mase = b2.aux({
        text: 'ませ',
        lemma: 'ます',
      }, 'mase');
      const nun = b2.aux({
        text: 'ん',
        lemma: 'ぬ',
      }, 'nun');
      const ka = b2.particle('か', 'ka', { dep: 'mark' });

      b2.auxOf(nounVerb, shi);
      b2.auxOf(nounVerb, mase);
      b2.inOrder(mase, nun, 1);
      b2.inOrder(nun, ka, 1);
      b2.captureSpan('ませんか', nounVerb, ka);
    },

    // Pattern 3: Casual negative form (ないか) - same function as ませんか
    // Some Bunpro examples use casual form: 行かないか = "won't you go?"
    // GiNZA parses: verb (未然形) + ない (auxiliary) + か
    (b3) => {
      const verb = b3.verb({}, 'verb');
      const nai = b3.aux({
        lemma: 'ない',
      }, 'nai');
      const ka = b3.particle('か', 'ka', { dep: 'mark' });

      b3.auxOf(verb, nai);
      b3.inOrder(nai, ka, 2);
      b3.captureSpan('ませんか', verb, ka);
    }
  );
});
