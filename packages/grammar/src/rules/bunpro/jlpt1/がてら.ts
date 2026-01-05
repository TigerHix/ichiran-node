import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: がてら (gatera) - "while doing X", "on the occasion of"
 *
 * A formal particle indicating doing something on the same occasion as another action.
 * Similar to ついでに but more polite/formal, and often used with movement phrases.
 *
 * Structure:
 * - Verb stem (masu form) + がてら
 * - Noun + がてら
 *
 * Examples:
 * - 帰りがてら手紙出してくるね (I'll post the letter on my way home)
 * - 散歩がてら、公園を散歩しよう (Let's walk in the park while viewing the blossoms)
 * - 買い物がてら、紅葉を見に行かないか？
 * - 友達の家に行きがてら、近くの神社にお参りに行った
 * - 運動がてら会社まで自転車で行く (Ride bike to work for exercise)
 *
 * Unlike ついでに, がてら:
 * - Connects directly to nouns (no の needed)
 * - Has a more formal/literary tone
 * - Often used with movement verbs (散歩, 帰り, 行き, etc.)
 *
 * GiNZA parse structure:
 * - Noun + がてら: 散歩 (NOUN) + がてら (NOUN, tag=接尾辞-名詞的-副詞可能)
 * - Noun + がてら (special): 帰り (NOUN) + がてら (ADV, tag=接尾辞-名詞的-副詞可能)
 * - Verb stem + がてら: 行き (VERB, inflectionForm=連用形-一般) + がてら (NOUN, tag=接尾辞-名詞的-副詞可能)
 *
 * Note: がてら is parsed inconsistently by GiNZA:
 * - Most nouns + がてら: pos=NOUN (e.g., 散歩がてら, 買い物がてら, 運動がてら)
 * - Some nouns + がてら: pos=ADV (e.g., 帰りがてら)
 * - Verb stems + がてら: pos=NOUN (e.g., 行きがてら)
 * The tag is always: 接尾辞-名詞的-副詞可能
 */
export default linguisticRule('がてら', (r) => {
  // Common constraint: がてら has this specific tag
  const gateraTag = '接尾辞-名詞的-副詞可能';

  r.either(
    // Pattern 1: Noun + がてら (NOUN after most nouns)
    // 散歩がてら, 買い物がてら, 帰省がてら, 運動がてら, 勉強がてら
    (b) => {
      const noun = b.noun({}, 'noun');
      const gatera = b.tok({
        text: 'がてら',
        pos: 'NOUN',
        tag: gateraTag,
      }, 'gatera');
      b.inOrder(noun, gatera, 1);
      b.captureSpan('がてら', noun, gatera);
    },
    // Pattern 2: Noun + がてら (ADV after special nouns like 帰り)
    // 帰りがてら
    (b) => {
      const noun = b.noun({}, 'noun');
      const gatera = b.tok({
        text: 'がてら',
        pos: 'ADV',
        tag: gateraTag,
      }, 'gatera');
      b.inOrder(noun, gatera, 1);
      b.captureSpan('がてら', noun, gatera);
    },
    // Pattern 3: Verb stem (連用形) + がてら (NOUN after verb stem)
    // 行きがてら, 迎えがてら
    (b) => {
      const stem = b.tok({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const gatera = b.tok({
        text: 'がてら',
        pos: 'NOUN',
        tag: gateraTag,
      }, 'gatera');
      b.inOrder(stem, gatera, 1);
      b.captureSpan('がてら', stem, gatera);
    }
  );
});
