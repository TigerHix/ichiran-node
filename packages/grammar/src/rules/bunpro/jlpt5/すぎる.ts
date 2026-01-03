import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('すぎる', (r) => {
  // すぎる (sugiru) - auxiliary verb meaning "too much" or "excessively"
  // Attaches to verb stems and adjectives to mean "too [stem]"
  // Examples: 食べすぎる (eat too much), 高すぎる (too expensive), 静かすぎる (too quiet)
  // Also matches conjugated forms: すぎた, すぎない, すぎます, etc.

  // Pattern: Stem + すぎる (where すぎる is lemma=すぎる, any form)
  // The stem is the syntactic head, すぎる is an advcl modifier

  r.either(
    // Branch 1: I-adjective stem + すぎる
    // Stem is VERB with conjugationClass=形容詞 and inflectionForm=語幹-一般
    (b) => {
      const stem = b.tok({
        pos: 'VERB',
        conjugationClass: '形容詞',
        inflectionForm: '語幹-一般',
      }, 'stem');
      const sugiru = b.verb({
        lemma: 'すぎる',
      }, 'sugiru');
      b.headChild(stem, sugiru, 'advcl');
      b.captureSpan('すぎる', stem, sugiru);
    },

    // Branch 2: Verb stem (ren'youkei) + すぎる
    // Stem is VERB with inflectionForm=連用形-一般
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const sugiru = b.verb({
        lemma: 'すぎる',
      }, 'sugiru');
      b.headChild(stem, sugiru, 'advcl');
      b.captureSpan('すぎる', stem, sugiru);
    },

    // Branch 3: Negative i-adjective stem (なさ) + すぎる
    // Stem is ADJ with inflectionForm=語幹-サ and conjugationClass=形容詞
    (b) => {
      const stem = b.adj({
        inflectionForm: '語幹-サ',
        conjugationClass: '形容詞',
      }, 'stem');
      const sugiru = b.verb({
        lemma: 'すぎる',
      }, 'sugiru');
      b.headChild(stem, sugiru, 'advcl');
      b.captureSpan('すぎる', stem, sugiru);
    },

    // Branch 4: Na-adjective + すぎる
    // Stem is ADJ (no inflectionForm)
    (b) => {
      const stem = b.adj({}, 'stem');
      const sugiru = b.verb({
        lemma: 'すぎる',
      }, 'sugiru');
      b.headChild(stem, sugiru, 'advcl');
      b.captureSpan('すぎる', stem, sugiru);
    },

    // Branch 5: Na-adjective stem + すぎる with compound dependency
    // For cases like ねなさすぎた
    (b) => {
      const stem = b.adj({
        inflectionForm: '語幹-サ',
        conjugationClass: '形容詞',
      }, 'stem');
      const sugiru = b.verb({
        lemma: 'すぎる',
      }, 'sugiru');
      b.headChild(stem, sugiru, 'compound');
      b.captureSpan('すぎる', stem, sugiru);
    },

    // Branch 6:  parsed as single ADJ token
    // For cases like 食べすぎる when parsed as one token
    (b) => {
      const compound = b.adj({
        lemma: 'すぎる',
        conjugationClass: '上一段-ガ行',
      }, 'compound');
      b.capture(compound);
    },

    // Branch 7: Shortened form すぎ (without る) as particle
    // Casual form: 可愛すぎ！, 良すぎ, etc.
    (b) => {
      const stem = b.adj({
        inflectionForm: '語幹-一般',
      }, 'stem');
      const sugi = b.particle('すぎ', 'sugi');
      b.headChild(stem, sugi, 'mark');
      b.captureSpan('すぎる', stem, sugi);
    }
  );
});
