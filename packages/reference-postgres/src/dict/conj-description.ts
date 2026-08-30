// Conjugation type ID to description mapping
//
// In Lisp (dict-load.lisp line 257-260), this is loaded from jmdictdb/data/conj.csv at
// compile/load time via the csv-hash macro, which creates:
//   - A hash table *conj-description*
//   - An accessor function get-conj-description
//
// For the TypeScript port, we hard-code this as a constant object because:
//   1. The data is tiny (only 18 entries)
//   2. It rarely changes (conjugation types are stable)
//   3. No runtime file I/O or CSV parsing needed
//   4. Type-safe and more efficient than loading from file
//
// Source data:
//   - Lines 1-13 from jmdictdb/data/conj.csv
//   - Lines 50-54 from dict-errata.lisp errata-conj-description-hook (lines 1168-1179)
export const CONJ_DESCRIPTION: Record<number, string> = {
  1: "Non-past",
  2: "Past (~ta)",
  3: "Conjunctive (~te)",
  4: "Provisional (~eba)",
  5: "Potential",
  6: "Passive",
  7: "Causative",
  8: "Causative-Passive",
  9: "Volitional",
  10: "Imperative",
  11: "Conditional (~tara)",
  12: "Alternative (~tari)",
  13: "Continuative (~i)",
  // Errata additions from dict-errata.lisp errata-conj-description-hook
  50: "Adverbial",           // +conj-adverbial+
  51: "Adjective Stem",      // +conj-adjective-stem+
  52: "Negative Stem",       // +conj-negative-stem+
  53: "Causative (~su)",     // +conj-causative-su+
  54: "Old/literary form"    // +conj-adjective-literary+
};

// dict-load.lisp line 257-260: csv-hash *conj-description* creates get-conj-description accessor
// This function replicates the Lisp (get-conj-description conj-type) behavior
export function getConjDescription(conjType: number): string {
  return CONJ_DESCRIPTION[conjType] || String(conjType);
}