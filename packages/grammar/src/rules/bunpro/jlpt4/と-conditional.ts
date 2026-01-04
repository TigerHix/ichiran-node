import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: と-conditional (when/if - conditional form)
 *
 * Matches verb/adj/noun + と as a conditional marker meaning "when" or "if".
 * Expresses a natural consequence, habitual result, or certain outcome.
 *
 * Examples:
 * - 勉強をすると、テストは簡単になる。 (If you study, tests become easy.)
 * - 甘いものをいっぱい食べると、太る。 (If you eat a lot of sweet things, you'll gain weight.)
 * - 部屋が汚いと、お母さんに怒られる。 (If my room is messy, my mom will yell at me.)
 * - 地下鉄だと、五分早く着く。 (If it's the subway, you'll arrive 5 minutes faster.)
 *
 * Key discriminators from other と usages:
 * - Conditional と: pos=SCONJ, dep=mark (marks subordinate clauses)
 * - Quotation と: pos=ADP, dep=case (marks quotes/complements)
 * - Case marker と: pos=ADP, dep=case (marks nouns with nmod/obl deps)
 *
 * GiNZA parses conditional と as SCONJ with dep=mark, distinguishing it from
 * the case marking particle と (ADP, dep=case).
 *
 * Structure:
 * - Verb (dictionary form) + と
 * - I-adjective + と
 * - Na-adjective/Noun + だと
 */
export default linguisticRule('と-conditional', (r) => {
  // Conditional と is marked as SCONJ with dep=mark
  const to = r.tok({
    text: 'と',
    pos: 'SCONJ',
    dep: 'mark'
  }, 'to');

  // Also match だと (noun + だ + と)
  // GiNZA parses だと as compound: だ(AUX) + と(SCONJ, dep=mark)
  const da = r.aux({ text: 'だ' }, 'da');
  const to2 = r.tok({
    text: 'と',
    pos: 'SCONJ',
    dep: 'mark'
  }, 'to2');

  r.either(
    // Pattern 1: Simple と conditional
    (branch1) => {
      branch1.capture(to);
    },
    // Pattern 2: だと conditional (noun + だ + と)
    (branch2) => {
      branch2.inOrder(da, to2, 1);
      branch2.captureSpan('だと', da, to2);
    }
  );
});
