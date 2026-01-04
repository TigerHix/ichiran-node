import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: せめて (semete) - "at least, at the very least"
 *
 * An adverb meaning "at least" or "at the very least". It expresses a minimal
 * hope or expectation in an otherwise unfavorable situation. Coming from the
 * verb 責める (semeru - to blame/press), it carries a nuance of "the minimum
 * that should be done" or "the least one can expect".
 *
 * Structure:
 * - せめて + Phrase
 * - せめて + Noun + だけ/くらい + ...
 * - せめて + 数量詞 + ...
 *
 * Common collocations:
 * - せめて + たい (desire)
 * - せめて + ほしい (desire for something)
 * - せめて + てほしい (want someone to do)
 * - せめて + べき (should)
 * - せめて + だけ (only/just)
 * - せめて + くらい (approximately/at least)
 *
 * Examples:
 * - せめて電話くらいくれればよかった。
 *   (At least a phone call would have been nice.)
 * - せめて一回行ってみたい。
 *   (I want to go at least once.)
 * - せめて1万は欲しいよ。
 *   (I want at least 10,000 yen.)
 * - せめておかずだけでも食べて。
 *   (At least eat your sides.)
 *
 * GiNZA parse structure:
 * - せめて is tagged as an adverb (ADV)
 * - Can be a single token or sometimes analyzed differently
 * - Typically appears at the beginning of a clause or sentence
 *
 * Different from:
 * - 少なくとも (sukunakutommo) - more neutral "at least"
 * - だけましだ (dake mashida) - "it's better than nothing" (evaluative)
 */
export default linguisticRule('せめて', (r) => {
  r.either(
    // Pattern 1: Single token せめて (ADV)
    // This is the most common pattern - せめて as a standalone adverb
    // Example: せめて一回行ってみたい。
    (b1) => {
      const semete = b1.adv({ text: 'せめて' }, 'semete');
      b1.captureSpan('せめて', semete, semete);
    },

    // Pattern 2: せめて as any token type (fallback)
    // In some cases, GiNZA might tag it differently or it might be part of
    // a larger expression
    (b2) => {
      const semete = b2.tok({ text: 'せめて' }, 'semete');
      b2.captureSpan('せめて', semete, semete);
    }
  );
});
