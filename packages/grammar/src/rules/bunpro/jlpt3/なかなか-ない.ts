import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('なかなか-ない', (r) => {
  // なかなか-ない (nakanaka-nai) - "not easily, hardly, not readily"
  // This is the NEGATIVE usage of なかなか, different from positive なかなか (JLPT3)
  //
  // Patterns:
  // - なかなか + negative verb: なかなか治らない, なかなかできない, なかなか進まない
  // - なかなか + negative verb (polite): なかなか言ってくれません
  // - なかなか + negative verb (complex): なかなか飲めない, なかなか理解できない
  // - なかなか + negative verb (compound): なかなか出てこない, なかなか取れない
  //
  // GiNZA parsing notes:
  // - なかなか is ADV (副詞)
  // - Negative verbs have aux with lemma containing negative (ない, ません, etc.)

  const nakanaka = r.adv({
    lemma: 'なかなか',
  }, 'nakanaka');

  r.either(
    // Pattern 1: なかなか + verb + negative auxiliary (direct)
    // なかなか治らない, なかなか進まない, なかなか出ない
    (b) => {
      const verb = b.verb({}, 'verb');
      const negAux = b.aux({
        lemmaOneOf: ['ない', 'ぬ', 'ん'],
      }, 'negAux');

      // The negative auxiliary should be attached to the verb
      b.auxOf(verb, negAux);

      // なかなか comes before the verb
      b.inOrder(nakanaka, verb, 5);

      // Capture from なかなか to the negative auxiliary
      b.captureSpan('なかなか-ない', nakanaka, negAux);
    },

    // Pattern 2: なかなか + verb + ません (polite negative)
    // なかなか言ってくれません
    (b) => {
      const verb = b.verb({}, 'verb');
      const masu = b.aux({
        lemma: 'ます',
        text: 'ません',
      }, 'masu');

      // ます auxiliary should be attached to the verb (possibly through other auxiliaries)
      b.inOrder(verb, masu, 10);

      // なかなか comes before the verb
      b.inOrder(nakanaka, verb, 5);

      // Capture from なかなか to ません
      b.captureSpan('なかなか-ない', nakanaka, masu);
    },

    // Pattern 3: なかなか + potential verb + negative auxiliary
    // なかなか飲めない (cannot drink), なかなか理解できない (cannot understand)
    (b) => {
      const verb = b.verb({
        inflectionForm: '連体形-一般',
      }, 'verb');
      const negAux = b.aux({
        lemmaOneOf: ['ない', 'ぬ', 'ん'],
      }, 'negAux');

      // The negative auxiliary should be attached to the verb
      b.auxOf(verb, negAux);

      // なかなか comes before the verb
      b.inOrder(nakanaka, verb, 5);

      // Capture from なかなか to the negative auxiliary
      b.captureSpan('なかなか-ない', nakanaka, negAux);
    }
  );
});
