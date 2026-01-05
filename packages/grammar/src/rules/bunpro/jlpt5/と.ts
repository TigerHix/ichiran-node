import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('と', (r) => {
  // Match quotation particle と (quotative marker)
  // This is distinct from:
  // - Conditional と (SCONJ + mark dep)
  // - Accompaniment と "with" (ADP + case dep + noun with nmod/obl dep)
  const to = r.particle('と', 'to', { pos: 'ADP', dep: 'case' });

  // The head of と should be the quoted content
  // Key discriminators:
  // - Quotation と: head has dep=ccomp (clausal complement), advcl, acl
  // - With と: head has dep=nmod or dep=obl with NOUN/PRON/PROPN pos
  //
  // Accept: dep=ccomp, advcl, acl, root (quoted clauses)
  // Also accept: dep=obl when pos is ADJ/VERB/AUX (quoted predicates like 危ないとかいた)
  // Reject: dep=nmod or (dep=obl AND pos is NOUN/PRON/PROPN) (with/accompaniment)

  r.either(
    // Pattern 1: Clause with ccomp/advcl/acl/root dep (full quoted sentences/clauses)
    (r1) => {
      const quoted = r1.tok({
        depOneOf: ['ccomp', 'advcl', 'acl', 'root']
      }, 'quoted');
      r1.headChild(quoted, to);
      r1.capture(to);
    },
    // Pattern 2: Predicate with obl dep (quoted predicates like 危ないとかいた)
    (r2) => {
      const quoted = r2.tok({
        dep: 'obl',
        posOneOf: ['ADJ', 'VERB', 'AUX']
      }, 'quoted');
      r2.headChild(quoted, to);
      r2.capture(to);
    }
  );
});
