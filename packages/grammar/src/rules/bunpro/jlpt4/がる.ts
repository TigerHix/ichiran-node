import { V, node, lemmaOneOf, posOneOf } from '../../../engine/dsl.js';
import type { RuleSpec } from '../../../engine/dsl.js';

// がる - third-person desire/emotion suffix
// Attached to adjective stems to show how someone else appears to feel
//
// Examples:
// - い-adjectives (remove い): 強い → 強がる, 怖い → 怖がる, 寒い → 寒がる, 暑い → 暑がる
// - な-adjectives (no な): 嫌 → 嫌がる
// - 欲しい: 欲しい → 欲しがる
// - 面白い → 面白がる, 恥ずかしい → 恥ずかしがる
//
// GiNZA parsing is complex:
// - Full dictionary forms (強がる, つよがる): lemma is the full word
// - Conjugated stems (がっ, やがっ): lemma includes stem (がる, やがる)
// - Various POS: ADJ, VERB, NOUN, PART, AUX depending on form
//
// Note: Noun forms like 怖がり (scaredy-cat) also match since they share
// the same lemma. This is linguistically correct as they're derived from がる.

const garu: RuleSpec = {
  id: 'がる',
  where: [
    // Match any token that is part of the がる construction
    node(V('garu'), [
      posOneOf(['ADJ', 'VERB', 'NOUN', 'PART', 'AUX', 'SCONJ']),
      lemmaOneOf([
        // Pure がる (from conjugated forms like 強がっている)
        'がる',
        // いやがる variants (GiNZA sometimes analyzes as やがる)
        'いやがる', 'やがる',
        // Full lemmas from common がる verbs (kanji and hiragana)
        '強がる', 'つよがる',
        '怖がる', 'こわがる',
        '寒がる', 'さむがる',
        '暑がる', 'あつがる',
        '嫌がる',
        '欲しがる', 'ほしがる',
        '面白がる', 'おもしろがる',
        '恥ずかしがる', 'はずかしがる',
      ]),
    ]),
  ],
  captures: [
    { kind: 'token', name: 'match', var: V('garu') },
  ],
};

export default garu;
