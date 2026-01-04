import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: というより - rather than saying / more X than Y
 *
 * Matches phrase + という + より
 *
 * This pattern presents (A) and then makes a more correct statement (B).
 * Used to correct or replace the first expression with a more accurate one.
 * "Rather than saying (A), (B)" or "(B) rather than (A)"
 *
 * Structure:
 * - Verb + というより (e.g., 来月というより来年)
 * - I-adjective + というより (e.g., 暖かいというより暑い)
 * - Na-adjective + (だ) + というより (e.g., 聡明(だ)というより)
 * - Noun + (だ) + というより (e.g., 技術者(だ)というより芸術家)
 *
 * The だ before というより is often omitted in casual speech, but may be
 * included for formal grammar.
 *
 * Examples:
 * - 今日は暖かいというより暑い。
 *   (It is hot rather than warm today.)
 * - 彼は聡明というよりずる賢い人です。
 *   (Rather than saying that he is wise, he is cunning.)
 * - 彼は技術者だというより芸術家だ。
 *   (He is more of an artist than an engineer.)
 *
 * This is different from:
 * - より (JLPT4) - simple comparison "more than" (月曜日より水曜日がいい)
 * - という - quotation/called pattern (田中という人)
 *
 * GiNZA parse structure:
 * - というより: と(particle) + いう(verb) + より(particle/aux)
 */
export default linguisticRule('というより', (r) => {
  // Quote particle と (marks what's being quoted/evaluated)
  const to = r.particle('と', 'to');

  r.either(
    // Pattern 1: Verb/Adj + というより (no だ)
    // Examples: 暖かいというより暑い, 来月というより来年
    (b) => {
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const yori = b.tok({ lemma: 'より' }, 'yori');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, yori, 1);
      b.captureSpan('というより', to, yori);
    },

    // Pattern 2: Noun/Na-adj + だ + というより
    // Examples: 技術者だというより芸術家, 聡明だというよりずる賢い
    (b) => {
      const da = b.aux({ lemma: 'だ' }, 'da');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const yori = b.tok({ lemma: 'より' }, 'yori');

      b.inOrder(da, to, 1);
      b.inOrder(to, iu, 1);
      b.inOrder(iu, yori, 1);
      b.captureSpan('というより', da, yori);
    }
  );
});
