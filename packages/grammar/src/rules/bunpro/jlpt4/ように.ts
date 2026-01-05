import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ように - So that, In order to, In such a way that
 *
 * Matches patterns where verb + ように expresses purpose or goal,
 * meaning "so that X" or "in order to X".
 *
 * Structure:
 * - Verb (potential/negative/dictionary) + ように + following clause
 *
 * Examples:
 * - 離れないように手をつなぐ (hold hands so won't get separated)
 * - 入れないように家のカギを閉める (lock house so burglars can't enter)
 * - できるように毎日勉強している (study every day so can pass)
 * - 話せるように日本語を勉強します (study Japanese to be able to speak)
 *
 * Key discriminators:
 * - Follows verb in potential, negative, or dictionary form
 * - ように can be parsed as single token or as よう + に
 * - Expresses purpose/aim (not manner/similarity like "～のように" = "like X")
 *
 * Common verb forms:
 * - Negative: ない-form + ように (e.g., 離れないように, 入れないように)
 * - Potential: potential form + ように (e.g., できるように, 話せるように)
 * - Dictionary: base form + ように (e.g., 間に合うように)
 *
 * GiNZA parsing notes:
 * - ように often parsed as single token (text=ように, pos=SCONJ)
 * - Sometimes parsed as よう (pos=NOUN) + に (pos=ADP/particle)
 * - The verb before ように is the action being aimed for
 *
 * Note: This rule matches verb + ように broadly. Related grammar points:
 * - ようになる (change of state: "came to be that...") - Different rule
 * - ようにする (make effort: "try to/make sure to...") - Different rule
 * These are semantically different but structurally similar, and may have
 * some overlap. The rule focuses on matching the core pattern correctly.
 */
export default bunproLinguisticRule('ように', (r) => {
  r.either(
    // Branch 1: ように as single token
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const yoni = b.tok({ text: 'ように' }, 'yoni');

      b.inOrder(verb, yoni, 5);
      b.captureSpan('ように', verb, yoni);
    },
    // Branch 2: よう and に as separate tokens
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const you = b.tok({ text: 'よう' }, 'you');
      const ni = b.particle('に', 'ni');

      b.inOrder(verb, you, 5);
      b.inOrder(you, ni, 1);
      b.captureSpan('ように', verb, ni);
    }
  );
});
