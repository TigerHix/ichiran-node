import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: う-Verb (Past) - u-verb past tense
 *
 * Matches u-verbs (godan verbs) in past tense form.
 * Both casual and polite forms:
 * - Casual: verb stem + た/だ (e.g., 買った, 書いた, 死んだ)
 * - Polite: verb stem + ました (e.g., 買いました, 書きました, 死にました)
 *
 * U-verbs have conjugation classes starting with "五段-" (godan).
 * The past tense auxiliary is 助動詞-タ.
 *
 * This rule matches both terminal form (終止形) and attributive form (連体形)
 * since past tense verbs can end sentences or modify nouns.
 */
export default linguisticRule('う-verb-past', (r) => {
  // All godan (u-verb) conjugation classes
  const godanClasses = [
    '五段-カ行',
    '五段-ガ行',
    '五段-サ行',
    '五段-タ行',
    '五段-ナ行',
    '五段-バ行',
    '五段-マ行',
    '五段-ラ行',
    '五段-ワア行',
  ];

  // Match any u-verb (godan) followed by past tense auxiliary
  r.either(
    // Branch for each godan conjugation class
    ...godanClasses.map((cc): Parameters<typeof r.either>[0] =>
      (b) => {
        const uVerb = b.verb({ conjugationClass: cc as any }, 'verb');

        // Past tense auxiliary た (or だ after n-sound verbs)
        // Note: lemma can be た or だ depending on the verb stem sound
        const pastAux = b.aux({
          lemmaOneOf: ['た', 'だ'],  // Either form of the past auxiliary
          conjugationClass: '助動詞-タ',
        }, 'past');

        // Require the auxiliary to attach to the verb
        b.auxOf(uVerb, pastAux);

        // Capture from verb to auxiliary (span includes the full past form)
        b.captureSpan('match', uVerb, pastAux);
      }
    )
  );
});
