import { bunproLinguisticRule } from '../../../engine/lang.js';

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
 * GiNZA parsing:
 * - Simple conditional (食べると): と = SCONJ, dep=mark
 * - Te-form conditional (点けると): と = SCONJ, dep=case (with te-form verb)
 * - Suru-verb conditional (ロッククライミングをすると): と = ADP, dep=case
 * - だと conditional (静かだと): だ(AUX,dep=aux) + と(ADP,dep=case)
 *
 * The だと form is parsed differently: GiNZA treats it as a copula compound
 * rather than a pure conditional marker. The と has dep=case like other particles.
 *
 * To distinguish conditional だと from quotation だと:
 * - Conditional: noun/adj has dep=advcl (adverbial clause modifier)
 * - Quotation: noun/adj has dep=ccomp (clausal complement of speech verb)
 *
 * Structure:
 * - Verb (dictionary form) + と (SCONJ, dep=mark or case)
 * - I-adjective + と (SCONJ, dep=mark)
 * - Na-adjective/Noun + だ (AUX) + と (ADP, dep=case) with advcl dependency
 */
export default bunproLinguisticRule('と-conditional', (r) => {
  r.either(
    // Pattern 1: Simple と conditional (verb/i-adj + と)
    // Example: 食べると, 行くと, 汚いと
    // GiNZA: と = SCONJ with dep=mark or dep=case (te-form)
    (branch1) => {
      const to = branch1.tok({
        text: 'と',
        pos: 'SCONJ'
      }, 'to');
      branch1.capture(to);
    },

    // Pattern 2: すると conditional (suru-verb + を + すると)
    // Example: ロッククライミングをすると
    // GiNZA: する(VERB,advcl) + と(ADP,case)
    // Discriminator: する must have dep=advcl (conditional clause)
    (branch2) => {
      const suru = branch2.tok({
        lemma: 'する',
        pos: 'VERB',
        dep: 'advcl'
      }, 'suru');
      const to = branch2.tok({
        text: 'と',
        pos: 'ADP',
        dep: 'case'
      }, 'to');
      branch2.caseMarker(suru, to);
      branch2.capture(to);
    },

    // Pattern 3: だと conditional (na-adj/noun + だ + と)
    // Example: 静かだと, 地下鉄だと
    // GiNZA: だ(AUX,dep=aux) + と(ADP,dep=case)
    // Discriminator from quotation: noun/adj must have dep=advcl (not ccomp)
    (branch3) => {
      const head = branch3.tok({
        dep: 'advcl'
      }, 'head');
      const da = branch3.aux({
        text: 'だ',
        dep: 'aux'
      }, 'da');
      const to = branch3.tok({
        text: 'と',
        pos: 'ADP',
        dep: 'case'
      }, 'to');

      branch3.auxOf(head, da);
      branch3.inOrder(da, to, 1);
      branch3.captureSpan('だと', da, to);
    }
  );
});
