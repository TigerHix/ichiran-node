import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: より - Comparison marker (from/than)
 *
 * Matches the particle より used for comparisons between two items.
 * The item marked by より is the "less than" item in the comparison.
 *
 * Structures:
 * - Noun + より + [comparison result]
 * - Verb + より + [comparison result]
 * - Adjective + より + [comparison result]
 *
 * Examples:
 * - 私の絵より上手 (better than my painting)
 * - 日本語は英語より話しにくい (Japanese is harder to speak than English)
 * - 月曜日より日曜日がいい (Sunday is better than Monday)
 * - スポーツをするよりゲームの方が楽しい (Games are more fun than sports)
 * - 太陽はロウソクより明るい (The sun is brighter than a candle)
 *
 * Key discriminators:
 * - より is the case marking particle (dep=case)
 * - Attaches to nouns, pronouns, verbs, or adjectives as the comparison baseline
 * - NOT preceded by いう (to avoid matching "というより" = "rather than saying")
 *
 * GiNZA parse structure:
 * - POSITIVE: 日本語は英語より話しにくい
 *   - 英語(NOUN) + より(ADP, lemma=より, dep=case, head=1→NOUN)
 * - POSITIVE: スポーツをするより、ゲームの方が楽しい
 *   - する(VERB) + より(ADP, lemma=より, dep=case, head=0→VERB)
 * - POSITIVE: 太陽はロウソクより明るい
 *   - ロウソク(ADJ) + より(ADP, lemma=より, dep=case, head=0→ADJ)
 * - NEGATIVE (というより): これはゲームというよりスポーツだ
 *   - いう(VERB, dep=acl) + より(ADP, lemma=より, dep=case, head=2→VERB)
 *
 * Note: This is the standalone comparison marker. For the pattern
 * "より～のほうが" (more X than Y), see JLPT5 rule より-のほうが.
 */
export default bunproLinguisticRule('より', (r) => {
  const yori = r.particle('より', 'yori', { dep: 'case' });

  r.either(
    // Pattern 1: NOUN, PRON, PROPN, ADJ (normal comparison)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'ADJ'] }, 'noun');
      b.caseMarker(noun, yori);
      b.capture(yori);
    },
    // Pattern 2: VERB (like する in "スポーツをするより")
    (b) => {
      const verb = b.tok({ pos: 'VERB' }, 'verb');
      b.caseMarker(verb, yori);
      b.capture(yori);
    }
  );
});
