import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: なさい (imperative suffix)
 *
 * Matches verb stem + なさい (do X! - command form).
 * This is a polite/soft imperative form used by parents, teachers, or superiors
 * to give commands to children, students, or subordinates.
 *
 * Structure:
 * - Verb stem (連用形 - masu stem) + なさい
 *
 * Examples:
 * - 勉強しなさい (Study!)
 * - 野菜を食べなさい (Eat your vegetables!)
 * - 早く帰りなさい (Hurry home!)
 * - 注意しなさい (Be careful!)
 *
 * Key discriminators:
 * - なさい must have lemma=なさる (honorific verb)
 * - なさい must have inflectionForm=命令形 (imperative form)
 * - Must be preceded by verb form (VERB or AUX)
 * - Verb attaches directly to なさい via aux dependency
 *
 * GiNZA parse structure:
 * - For しなさい: し is often parsed as AUX (lemma=する), not VERB
 * - なさい: AUX with lemma=なさる, inflectionForm=命令形
 * - For suru verbs: 勉強(NOUN) + し(AUX) + なさい(AUX)
 *
 * Note: The casual short form (verb stem + な) is not supported due to
 * GiNZA limitations in distinguishing it from the prohibitive pattern (verb dict form + な).
 */
export default linguisticRule('なさい', (r) => {
  // なさい as imperative auxiliary (lemma=なさる, imperative form)
  const nasai = r.aux({
    text: 'なさい',
    lemma: 'なさる',
    inflectionForm: '命令形'
  }, 'nasai');

  // Find the verb form before なさい
  // For most verbs: 直接连接 (verb stem + なさい)
  // For suru verbs: noun + し + なさい
  // We use r.auxOf to find the head that なさい attaches to
  const verbStem = r.tok({
    posOneOf: ['VERB', 'AUX'],
  }, 'verbStem');

  // Dependency: なさい attaches as aux to the verb
  r.auxOf(verbStem, nasai);

  // Capture the full span from verb stem to なさい
  r.captureSpan('なさい', verbStem, nasai);
});
