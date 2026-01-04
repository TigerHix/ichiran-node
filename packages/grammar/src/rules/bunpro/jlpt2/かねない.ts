import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: かねない (kanenai) - Quite possible, there's a risk that, might well
 *
 * Expresses that something negative might happen or is quite possible.
 * The negative form of かねる (cannot do), but with different meaning:
 * instead of "cannot", it means "might" or "there's a risk that".
 * Used to emphasize that a negative situation could realistically occur
 * given the underlying logic.
 *
 * Structures:
 * - Verb［stem］+ かねない (casual)
 * - Verb［stem］+ かねません (polite)
 * - Noun + に + かねない
 *
 * Examples:
 * - それは命を落としかねない感染症らしい。
 *   (That appears to be an infectious disease that could be fatal.)
 * - 地震の後には津波が起こりかねない。
 *   (A tsunami might occur after an earthquake.)
 * - 操作を間違えれば怪我人が出かねない。
 *   (If you operate it wrong, injuries could result.)
 * - 危険な運転をすれば、事故が起こり兼ねない。
 *   (If you drive dangerously, an accident might occur.)
 *
 * Key discriminators:
 * - かね is the stem of かねる (kaneru) + auxiliary ない (nai)
 * - かね indicates the verb stem attaches to かねる
 * - ない is the negative auxiliary meaning "not"
 * - But together かねない means "might happen" (positive possibility)
 * - Must be attached to verb stem or noun+に
 * - Usually expresses negative/unwelcome possibilities
 * - Different from かねる (cannot do - hesitation/inability)
 * - Different from independent use of かね + ない (separate words)
 *
 * GiNZA parse structure:
 * - Verb stem (連用形-一般) + かね(AUX/VERB) + ない(AUX)
 * - Various dependency relations (aux, fixed, compound, advcl)
 * - Sometimes parsed as two tokens: かね + ない
 * - Sometimes かねない appears as a single token
 *
 * Important: Matches only when かね is attached as auxiliary stem
 * to exclude:
 * - かねる (positive form - different grammar, "cannot do")
 * - かねて (te-form - as in を兼ねて, "serving two purposes")
 * - Independent use of かね + ない (separate words)
 */
export default linguisticRule('かねない', (r) => {
  r.either(
    // Branch 1: かね (auxiliary stem) + ない (negative auxiliary)
    // Pattern: verb stem + かね + ない
    // かね has dep=aux, ない has dep=aux
    (b) => {
      const kane = b.aux({
        textOneOf: ['かね', '兼ね'],
        lemmaOneOf: ['かねる', '兼ねる'],
        dep: 'aux',
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.auxOf(kane, nai);
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 2: かね (auxiliary stem) + ない with fixed dependency
    (b) => {
      const kane = b.aux({
        textOneOf: ['かね', '兼ね'],
        lemmaOneOf: ['かねる', '兼ねる'],
        dep: 'fixed',
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.auxOf(kane, nai);
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 3: Verb stem (ren'youkei) + かね + ない
    // Stem is syntactic head, かねない modifies it
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kane = b.aux({
        textOneOf: ['かね', '兼ね'],
        lemmaOneOf: ['かねる', '兼ねる'],
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(stem, kane, 3);
      b.auxOf(kane, nai);
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 4: Verb stem + かね + ない with compound dependency
    // More flexible - just require the order, not specific inflection form
    (b) => {
      const stem = b.verb({}, 'stem');
      const kane = b.tok({
        textOneOf: ['かね', '兼ね'],
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(stem, kane, 5);
      b.inOrder(kane, nai, 2);
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 5: かねない as a single compound verb/auxiliary
    // For cases where GiNZA parses it as one token
    (b) => {
      const kanenai = b.tok({
        textOneOf: ['かねない', '兼ねない'],
        lemma: 'かねる',
      }, 'kanenai');
      b.capture(kanenai);
    },

    // Branch 6: Noun + に + かねない
    // Nounに attached to かねない pattern
    (b) => {
      const noun = b.noun({}, 'noun');
      const ni = b.particle('に', 'ni');
      const kane = b.tok({
        textOneOf: ['かね', '兼ね'],
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(noun, ni, 2);
      b.inOrder(ni, kane, 3);
      b.inOrder(kane, nai, 2);
      b.captureSpan('かねない', kane, nai);
    },

    // Branch 7: Any token before かね + ない
    // Most flexible pattern - just requires かね + ない in sequence
    (b) => {
      const kane = b.tok({
        textOneOf: ['かね', '兼ね'],
      }, 'kane');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(kane, nai, 2);
      b.captureSpan('かねない', kane, nai);
    }
  );
});
