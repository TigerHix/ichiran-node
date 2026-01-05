import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: かねない (kanenai) - Might well happen, There's a risk of
 *
 * The negative form of かねる, indicating that something negative might happen or
 * there's a risk/possibility of an undesirable outcome. Expresses that the speaker
 * sees a real possibility of something bad occurring based on the current situation.
 *
 * Structures:
 * - Verb［stem］+ かねない (casual)
 * - Verb［stem］+ かねません (polite)
 *
 * Examples:
 * - この事故は今後増えるかねない。
 *   (This accident might increase in the future.)
 * - 彼はそんなことをしかねない。
 *   (He might do such a thing.)
 * - 誤解を招きかねない発言だ。
 *   (This is a remark that might invite misunderstanding.)
 * - 危険な運転をすれば、事故が起こりかねない。
 *   (If you drive dangerously, an accident might occur.)
 *
 * Key discriminators:
 * - かねない is formed from verb stem + かね + ない
 * - Indicates possibility/risk of negative outcome (opposite of かねる)
 * - かね is an auxiliary verb form (stem of かねる)
 * - ない is an auxiliary verb indicating negation
 * - Usually used with negative or undesirable outcomes
 * - Different from かねる (positive form - "cannot do", different grammar)
 * - Different from independent use of 兼ねない (combining/chance)
 *
 * GiNZA parse structure:
 * - Multiple possible parses for かねない:
 *   1. かね(AUX) + ない(AUX/VERB) - most common
 *   2. Stem(VERB) + かね(VERB/AUX) + ない(AUX)
 *   3. Various dependency relations (aux, fixed, compound, advcl)
 *   4. かねる(VERB, 連用形) + ない(AUX)
 *
 * Important: This rule must NOT match:
 * - かねる (positive form - "cannot do", different grammar)
 * - かねて (te-form - "serving dual purpose")
 * - Independent 兼ねない (coincidental/by chance)
 */
export default bunproLinguisticRule('かねない', (r) => {
  r.either(
    // Branch 1: かね + ない where both are auxiliaries
    // Most common pattern: verb stem + かね (aux) + ない (aux)
    (b) => {
      const kane = b.aux({
        lemmaOneOf: ['かね', '兼ね'],
        dep: 'aux',
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(kane, nai, 3);
      b.auxOf(kane, nai);
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 2: かね + ない with fixed dependency
    // Alternative parsing for some expressions
    (b) => {
      const kane = b.aux({
        lemmaOneOf: ['かね', '兼ね'],
        dep: 'fixed',
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(kane, nai, 3);
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 3: かねる (VERB, 連用形) + ない (AUX)
    // When かねる appears in 連用形 and acts as auxiliary to ない
    // This handles cases where GiNZA parses かねる as a VERB in stem form
    (b) => {
      const kane = b.tok({
        lemmaOneOf: ['かねる', '兼ねる'],
        inflectionForm: '連用形-一般',
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(kane, nai, 3);
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 4: Verb stem (連用形) + かねない with compound structure
    // Stem and かねない form a compound
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kane = b.aux({
        lemmaOneOf: ['かね', '兼ね'],
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.headChild(stem, kane, 'compound');
      b.inOrder(kane, nai, 3);
      b.captureSpan('かねない', stem, nai);
    },

    // Branch 5: かねない as VERB token following verb stem
    // When GiNZA parses the entire expression as VERB instead of AUX
    // Must be preceded by verb stem (連用形)
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kanenai = b.verb({
        lemmaOneOf: ['かねない', '兼ねない'],
      }, 'kanenai');
      b.inOrder(stem, kanenai, 5);
      b.captureSpan('かねない', stem, kanenai);
    },

    // Branch 6: かね (AUX) + ない (VERB) with advcl/compound dependencies
    // Alternative structure where ない is parsed as VERB
    (b) => {
      const kane = b.aux({
        lemmaOneOf: ['かね', '兼ね'],
      }, 'kane');
      const nai = b.verb({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(kane, nai, 3);
      b.either(
        (b2) => {
          b2.headChild(kane, nai, 'advcl');
        },
        (b2) => {
          b2.headChild(kane, nai, 'compound');
        }
      );
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 7: かね (any POS with text containing かね) + ない (AUX)
    // Fallback for unusual parses where かね has unexpected POS tagging
    // but is clearly part of かねない construction
    (b) => {
      const kane = b.tok({
        textOneOf: ['かね', '兼ね'],
        lemmaOneOf: ['かね', '兼ね', 'かねる', '兼ねる'],
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(kane, nai, 3);
      b.captureSpan('かねない', kane, nai);
    }
  );
});
