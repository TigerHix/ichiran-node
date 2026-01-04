import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: だんだん (段々) - Gradually / Little by little / Step by step
 *
 * Matches だんだん (dandan) as an adverb expressing gradual progression.
 *
 * Structures:
 * - だんだん + Verb/Adjective (gradually becomes/does X)
 * - だんだんと + Verb/Adjective (same meaning, particle と is optional)
 *
 * Examples:
 * - だんだん暑くなる (gradually getting hotter)
 * - だんだん大きくなる (gradually getting bigger)
 * - だんだん英語がうまくなってきました (gradually improving in English)
 * - だんだんと寒くなる (gradually becoming colder)
 *
 * Key discriminators:
 * - POS is ADV (adverb)
 * - textOneOf allows both hiragana (だんだん) and kanji (段々) forms
 * - The optional particle と can follow だんだん
 *
 * Note: Similar to どんどん (rapid progression) but だんだん expresses
 * slower, step-by-step progression.
 */
export default linguisticRule('だんだん', (r) => {
  const dandan = r.adv({
    textOneOf: ['だんだん', '段々'],
  }, 'dandan');

  // Capture the adverb itself
  // Note: The particle と is optional and not part of the grammar rule
  r.capture(dandan);
});
