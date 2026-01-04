import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('だが・ですが', (r) => {
  // だが・ですが (but / however - conjunction at sentence start or between clauses)
  // Formal conjunction combining copula (だ/です) + conjunction particle (が)
  // Matches:
  // - だが (copula だ + conjunction が)
  // - ですが (copula です + conjunction が)
  //
  // GiNZA parses this pattern differently depending on position:
  // 1. At sentence start: だ/です has dep=cc, が has dep=fixed
  // 2. After adjective/verb: です has dep=aux, が has dep=mark
  //
  // The が particle is always pos=SCONJ to distinguish from subject particle (ADP,case).
  //
  // Usage: Connects two clauses, showing contrast between them.
  // Can appear at sentence start or between clauses.
  // More formal than けど/だけど.

  r.either(
    // Pattern 1: だが (copula だ + conjunction が)
    (b) => {
      const da = b.tok({ textOneOf: ['だ', 'だー'] }, 'da');
      const ga = b.particle('が', 'ga', { pos: 'SCONJ', depOneOf: ['fixed', 'mark', 'dep'] });
      b.inOrder(da, ga, 1);
      b.captureSpan('だが', da, ga);
    },
    // Pattern 2: ですが (copula です + conjunction が)
    (b) => {
      const desu = b.tok({ text: 'です' }, 'desu');
      const ga = b.particle('が', 'ga', { pos: 'SCONJ', depOneOf: ['fixed', 'mark', 'dep'] });
      b.inOrder(desu, ga, 1);
      b.captureSpan('ですが', desu, ga);
    }
  );
});
