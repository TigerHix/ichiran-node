import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ても (temo/demo) - even if / even though
 *
 * Verb[te-form] + も expresses "even if" or "even though".
 *
 * Examples:
 * - しても (even if [someone] does)
 * - 行っても (even if [someone] goes)
 * - なくても (even if [someone] doesn't)
 * - 寒くても (even if cold)
 * - 大変でも (even if difficult)
 *
 * Also handles:
 * - Adjective て-form + も (e.g., 寒くても)
 * - Negative verb form: なくても (e.g., なくてもいい)
 *
 * GiNZA parsing notes:
 * - Verb-te-forms are parsed as: verb stem + て (SCONJ)
 * - て has dep=mark, attaching to the verb stem
 * - も is a particle (PART)
 *
 * Forms handled:
 * - Verb-te + も: 行っても, しても, 食べても
 * - Verb-nakute + も: なくても, いなくても
 * - I-adj-te + も: 寒くても, 高くても
 * - Noun/Na-adj + でも: 大変でも (copula form)
 *
 * Excludes:
 * - でも as conjunction "but" (dep=cc)
 * - でも after question words (lemma=で, pos=ADP)
 */
export default linguisticRule('ても', (r) => {
  r.either(
    // Pattern 1: Verb te-form + も (ても)
    // Example: しても, 行っても, 食べても, 負けても
    // GiNZA: verb + て (SCONJ, lemma=て) + も (PART)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const mo = b.particle('も', 'mo');

      b.inOrder(te, mo, 1);
      b.captureSpan('ても', te, mo);
    },

    // Pattern 2: Verb negative te-form + も (なくても)
    // Example: なくても, いなくても, かわらなくても
    // GiNZA: verb + なく (AUX) + て (SCONJ) + も (PART)
    (b) => {
      const naku = b.aux({ lemma: 'ない' }, 'naku');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const mo = b.particle('も', 'mo');

      b.inOrder(naku, te, 1);
      b.inOrder(te, mo, 1);
      b.captureSpan('なくても', naku, mo);
    },

    // Pattern 3: I-adjective te-form + も (くても)
    // Example: 冷たくても, 痛くても, 高くても
    // GiNZA: i-adj + く (SCONJ) + て (SCONJ) + も (PART)
    (b) => {
      const ku = b.tok({ text: 'く', pos: 'SCONJ' }, 'ku');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const mo = b.particle('も', 'mo');

      b.inOrder(ku, te, 1);
      b.inOrder(te, mo, 1);
      b.captureSpan('くても', ku, mo);
    },

    // Pattern 4: I-adjective negative te-form + も (くなくても)
    // Example: 痛くなくても
    // GiNZA: i-adj + く (SCONJ) + なく (AUX) + て (SCONJ) + も (PART)
    (b) => {
      const ku = b.tok({ text: 'く', pos: 'SCONJ' }, 'ku');
      const naku = b.aux({ lemma: 'ない' }, 'naku');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const mo = b.particle('も', 'mo');

      b.inOrder(ku, naku, 1);
      b.inOrder(naku, te, 1);
      b.inOrder(te, mo, 1);
      b.captureSpan('くなくても', ku, mo);
    },

    // Pattern 5: Na-adjective + でも (copula form)
    // Example: 大変でも, でも should have lemma=だ (copula), pos=AUX, dep=aux
    // This distinguishes from "でも" as "but" (lemma=で, pos=ADP, dep=cc)
    (b) => {
      const de = b.tok({
        lemma: 'だ',
        pos: 'AUX',
        inflectionForm: '連用形-一般',
      }, 'de');
      const mo = b.particle('も', 'mo');

      b.inOrder(de, mo, 1);
      b.captureSpan('でも', de, mo);
    },

    // Pattern 6: Noun + でも (case markers, "even with X")
    // Example: 安い電子レンジでも (even with a cheap microwave)
    // GiNZA: noun + で (ADP, dep=case) + も (ADP, dep=case)
    // Both attach to the same noun head with dep=case
    // This matches "noun + でも" but NOT "question word + でも"
    (b) => {
      const de = b.tok({
        lemma: 'で',
        pos: 'ADP',
        dep: 'case',
      }, 'de');
      const mo = b.tok({
        lemma: 'も',
        pos: 'ADP',
        dep: 'case',
      }, 'mo');

      // Require that de and mo attach to the same head
      // This ensures they form a unit
      b.inOrder(de, mo, 1);
      b.captureSpan('でも', de, mo);
    },

    // Pattern 7: Na-adjective/Noun negative + でも (ではなくても/じゃなくても)
    // Example: 好きじゃなくても, 運転手じゃなくても
    // GiNZA: na-adj/noun + じゃなく (AUX/ADP) + て (SCONJ) + も (PART)
    (b) => {
      const janaku = b.tok({ textOneOf: ['じゃなく', 'ではなく'] }, 'janaku');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const mo = b.particle('も', 'mo');

      b.inOrder(janaku, te, 1);
      b.inOrder(te, mo, 1);
      b.captureSpan('なくても', janaku, mo);
    }
  );
});
