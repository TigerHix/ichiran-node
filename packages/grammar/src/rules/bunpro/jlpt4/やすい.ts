import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: やすい (易い) - Easy to do; Likely to
 *
 * Attached to verb stem (masu form) to express that an action is easy to do
 * or likely to happen. Opposite of にくい (difficult to do).
 *
 * Structures:
 * - Verb［stem］+ やすい (casual)
 * - Verb［stem］+ やすい + です (polite)
 * - Verb［stem］+ やすい + かった (past)
 * - Verb［stem］+ やすく + て (te-form)
 *
 * Examples:
 * - 食べやすい (easy to eat)
 * - しやすい (easy to do)
 * - 読みやすい (easy to read)
 * - 泣きやすい (prone to cry/likely to cry)
 * - 怒りやすい (prone to get angry)
 * - この白い服は、汚れやすいです (These white clothes are easy to get dirty)
 *
 * Key discriminators:
 * - やすい is an auxiliary (AUX) or adjective (ADJ) depending on parsing
 * - lemma is 'やすい' (distinguish from 安い 'cheap')
 * - Attaches to verb stem (連用形-一般)
 * - Can be parsed as single ADJ token or separate AUX
 *
 * Note: やすい is written in hiragana. The kanji 易い is rarely used.
 * Don't confuse with 安い (cheap) which is a different word.
 */
export default linguisticRule('やすい', (r) => {
  // GiNZA parses verb stem + やすい in multiple ways:
  // 1. As separate tokens (verb stem + auxiliary)
  // 2. As single ADJ token
  // 3. With various dependency relations (aux, advcl, compound, fixed)

  r.either(
    // Branch 1: やすい as auxiliary with dep=aux attached to verb
    // This is the most common and reliable pattern
    // Only match when there's a verb before it (preventing standalone 安い)
    (b) => {
      const yasui = b.aux({
        lemma: 'やすい',
        dep: 'aux',
      }, 'yasui');
      // Require that there's a verb token immediately before
      // This prevents matching standalone "やすい" (安い = cheap)
      const stem = b.verb({}, 'stem');
      b.inOrder(stem, yasui, 1);
      b.captureSpan('やすい', stem, yasui);
    },

    // Branch 2: Verb stem (ren'youkei) + やすい as advcl modifier
    // Stem is syntactic head, やすい modifies it
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const yasui = b.aux({
        lemma: 'やすい',
      }, 'yasui');
      b.headChild(stem, yasui, 'advcl');
      b.captureSpan('やすい', stem, yasui);
    },

    // Branch 3: Verb stem + やすい with compound dependency
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const yasui = b.aux({
        lemma: 'やすい',
      }, 'yasui');
      b.headChild(stem, yasui, 'compound');
      b.captureSpan('やすい', stem, yasui);
    },

    // Branch 4: やすい as auxiliary with dep=fixed
    // Alternative parsing for some verb forms
    (b) => {
      const yasui = b.aux({
        lemma: 'やすい',
        dep: 'fixed',
      }, 'yasui');
      // Require that there's a verb token nearby
      const stem = b.verb({}, 'stem');
      b.inOrder(stem, yasui, 1);
      b.captureSpan('やすい', stem, yasui);
    },

    // Branch 5: やすい as single ADJ token (compound with verb)
    // GiNZA sometimes parses "verb+やすい" as one adjective token
    // We need to distinguish from "安い" (cheap) which also has lemma=やすい
    (b) => {
      const yasui = b.adj({
        lemma: 'やすい',
      }, 'yasui');
      b.capture(yasui);
    },

    // Branch 6: Any AUX with lemma=やすい
    // Some verb stems might not be recognized as VERB
    (b) => {
      const yasui = b.aux({
        lemma: 'やすい',
      }, 'yasui');
      b.capture(yasui);
    },

    // Branch 7: Any token with lemma=やすい (catch-all)
    (b) => {
      const yasui = b.tok({
        lemma: 'やすい',
      }, 'yasui');
      b.capture(yasui);
    }
  );
});
