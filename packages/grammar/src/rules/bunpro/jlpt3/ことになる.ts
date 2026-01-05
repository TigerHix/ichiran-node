import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことになる - "it is decided that/it turns out that"
 *
 * Matches: verb/adj + こと + に + なる (outcome/arrangement construction)
 * This expresses non-volitional outcomes - things decided by others or circumstances.
 *
 * Contrast with ことにする (volitional decisions made by the speaker).
 *
 * Structure variants:
 * - Verb［連体形］+ ことになる (casual, present)
 * - Verb［連体形］+ ことになった (casual, past)
 * - Verb［連体形］+ ことになります (polite, present)
 * - Verb［連体形］+ ことになりました (polite, past)
 * - Verb［連体形］+ ことになって (te-form)
 * - ［な］Adj［な］+ ことになる (な-adjective variant)
 * - 困ったこと (noun-modified こと)
 * - こんなこと (determiner + こと)
 *
 * GiNZA parse structure (for "入院する" → "入院することになる"):
 * - 入院(NOUN/VERB) --aux--> する(AUX)
 * - する --compound--> こと(NOUN)
 * - こと --fixed--> に(ADP)
 * - こと --fixed--> なる(VERB)
 *
 * For "大変なことになった":
 * - 大変(ADJ) --aux--> な(AUX)
 * - 大変 --compound--> こと(NOUN)
 *
 * For "困ったことになった":
 * - 困っ(VERB) --aux--> た(AUX)
 * - 困っ --compound--> こと(NOUN)
 *
 * For "こんなことになったの？":
 * - こんな (PRON/DET) --nmod--> こと(NOUN)
 * - こと --obl--> なっ(VERB)
 * - なっ --case--> に(ADP)
 * - た --aux--> なっ
 *
 * Key insight: koto.dep can be 'compound' (when modified by verb/adj) or 'obl' (when used as object).
 * The に+なる attach to koto or vice versa depending on parse.
 */
export default bunproLinguisticRule('ことになる', (r) => {
  r.either(
    // Branch 1: Casual past with compound koto (〜ことになった)
    (b) => {
      // Preceding predicate (can be verb, adj, noun - anything that can modify koto)
      const pred = b.tok({}, 'pred');

      // Followed by こと (nominalizer) - dep=compound points to pred
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      // Followed by に (case marker, fixed)
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      // Followed by なっ (連用形)
      const nat = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-促音便' }, 'nat');
      b.inOrder(ni, nat, 1);

      // Followed by た (past) - attaches to pred as aux
      const ta = b.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
      b.auxOf(pred, ta);

      b.captureSpan('ことになる', pred, ta);
    },
    // Branch 2: Casual past with obl koto (〜こんなことになった)
    (b) => {
      // こと (used as object noun)
      const koto = b.noun({ lemma: 'こと', depOneOf: ['obl', 'nmod'] }, 'koto');

      // Optional: preceded by determiner (e.g., こんな, そんな)
      // In "どうしてこんなことになった", the modifier "どうしてして" is an advcl
      // pointing to the ROOT, not to koto directly. So we just look for koto.
      b.optional((b2) => {
        const det = b2.tok({ posOneOf: ['PRON', 'DET'] }, 'det');
        b2.headChild(det, koto, 'nmod');
        b2.inOrder(det, koto, 1);
      });

      // Followed by に (case marker)
      const ni = b.tok({ lemma: 'に' }, 'ni');
      b.inOrder(koto, ni, 1);

      // Followed by なっ (連用形) - the ROOT verb
      const nat = b.verb({ lemma: 'なる', inflectionForm: '連用形-促音便' }, 'nat');
      b.inOrder(ni, nat, 1);

      // Followed by た (past) - attaches to なっ as aux
      const ta = b.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
      b.auxOf(nat, ta);

      // Capture from koto (not from det, since det may not exist)
      b.captureSpan('ことになる', koto, ta);
    },
    // Branch 3: Polite past (〜ことになりました)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      // なり (連用形) - optional dep=fixed
      const nari = b.verb({ lemma: 'なる', inflectionForm: '連用形-一般' }, 'nari');
      b.inOrder(ni, nari, 3);

      // ました (polite past) - can be:
      // - mashita as aux of pred (e.g., 帰ら...ました)
      // - mashita decomposed: まし(aux of pred) + た(aux of pred)
      b.either(
        // 3a: Single ました token
        (b2) => {
          const mashita = b2.aux({ lemma: 'ました' }, 'mashita');
          b2.auxOf(pred, mashita);
          b2.captureSpan('ことになる', pred, mashita);
        },
        // 3b: まし + た decomposed
        (b2) => {
          const mash = b2.aux({ lemma: 'ます', inflectionForm: '連用形-一般' }, 'mash');
          b2.auxOf(pred, mash);

          const ta = b2.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
          b2.auxOf(pred, ta);

          b2.captureSpan('ことになる', pred, ta);
        }
      );
    },
    // Branch 4: Casual present (〜ことになる)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      // Present tense なる (辞書形/終止形) - no dep=fixed required
      const naru = b.verb({
        lemma: 'なる',
        inflectionForm: '終止形-一般',
      }, 'naru');
      b.inOrder(ni, naru, 3);

      b.captureSpan('ことになる', pred, naru);
    },
    // Branch 5: Polite present (〜ことになります)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      // なり (連用形) - optional dep=fixed
      const nari = b.verb({ lemma: 'なる', inflectionForm: '連用形-一般' }, 'nari');
      b.inOrder(ni, nari, 3);

      // ます (polite) - attaches to pred as aux
      const masu = b.aux({ lemma: 'ます', inflectionForm: '終止形-一般' }, 'masu');
      b.auxOf(pred, masu);

      b.captureSpan('ことになる', pred, masu);
    },
    // Branch 6: Te-form (〜ことになって)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      // なっ (連用形-促音便)
      const nat = b.verb({
        lemma: 'なる',
        inflectionForm: '連用形-促音便',
      }, 'nat');
      b.inOrder(ni, nat, 1);

      // て (te-form) - attaches to pred with dep=mark
      const te = b.tok({ lemma: 'て', depOneOf: ['mark', 'fixed'] }, 'te');
      b.headChild(pred, te, 'mark');

      b.captureSpan('ことになる', pred, te);
    }
  );
});
