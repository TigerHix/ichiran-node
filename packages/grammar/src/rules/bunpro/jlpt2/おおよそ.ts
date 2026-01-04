import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('おおよそ', (r) => {
  // おおよそ (approximately, roughly, about)
  // Slightly formal adverb used in announcements and formal contexts
  // Can be written as:
  //   - おおよそ (with honorific)
  //   - 大凡 (kanji form with honorific)
  //   - およそ (without honorific - alternate form)
  //   - 凡そ (kanji form without honorific)
  //
  // Meanings:
  // - "approximately" with numbers/time/distance (most common)
  // - "roughly" for general estimations
  // - "most" as in "most of" (e.g., おおよその見当)
  // - "outline/gist" as in "the gist of the plan" (noun usage)
  // - "as a rule" (rare, written with 凡)
  //
  // POS can be ADV (adverb) or NOUN (when meaning "outline/gist")
  // Modifies verbs, nouns, or appears at sentence start

  r.either(
    // Branch 1: おおよそ/およそ as adverb (POS=ADV)
    (branch) => {
      const ooyosoAdv = branch.tok({
        pos: 'ADV',
        lemmaOneOf: ['おおよそ', '大凡', 'およそ', '凡そ'],
      }, 'ooyoso');
      branch.capture(ooyosoAdv);
    },
    // Branch 2: おおよそ/およそ as noun meaning "outline/gist" (POS=NOUN)
    (branch) => {
      const ooyosoNoun = branch.tok({
        pos: 'NOUN',
        lemmaOneOf: ['おおよそ', '大凡', 'およそ', '凡そ'],
      }, 'ooyoso');
      branch.capture(ooyosoNoun);
    }
  );
});
