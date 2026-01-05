import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことにはならない (koto ni wa naranai) - "won't result in, can't possibly be"
 *
 * An expression indicating that (A) does not necessarily lead to (B), or that
 * "just because (A), it doesn't mean that (B)". It emphasizes that fulfilling
 * a condition does not guarantee the expected result.
 *
 * Common structure patterns:
 * - Phrase［て/からといって］+ ことにはならない (even if/just because... doesn't mean)
 * - Phrase + という + ことにはならない (with emphatic quoting)
 * - Verb［た］+ だけでは + ことにはならない (merely doing X doesn't mean Y)
 *
 * Examples:
 * - ５分ノートを見直しただけでは、勉強したことにはならない。
 *   (Just because you reviewed your notes for 5 minutes doesn't mean you studied.)
 * - サンプルだからといって、乱暴に扱ってもいいことにはならない。
 *   (Just because it's a sample doesn't mean it's okay to handle it roughly.)
 * - いいホテルに泊まろうと言ったとしても、必ず一流ホテルに泊まるということにはならない。
 *   (Just because I said let's stay at a nice hotel doesn't mean we'll stay at a first-class hotel.)
 *
 * Key discriminators from similar patterns:
 * - ことになる (JLPT3): decision/outcome without negation (becomes)
 * - ことになっている (JLPT2): ongoing state/arrangement (is arranged that)
 * - ことにはならない (JLPT2): negative, denies necessary outcome (won't result in)
 *
 * GiNZA parse structure (for "勉強したことにはならない"):
 * - 勉強(VERB) --aux--> し(AUX) [preceding predicate]
 * - た(AUX) --aux--> 勉強 [past tense marker on predicate]
 * - こと(NOUN) --compound--> 勉強 [nominalizer attaches to predicate]
 * - に(ADP) --case--> こと [case marker]
 * - は(ADP) --case--> こと [topic marker]
 * - なら(VERB, lemma=なる, inflectionForm=未然形-一般) --fixed/root--> こと or independent
 * - ない(AUX) --aux--> なら [negation attaches to naranai]
 *
 * Parse variations observed:
 * 1. With という (emphatic): ...いう(VERB) + こと(NOUN) + に(ADP) + は(ADP) + なら(VERB) + ない(AUX)
 * 2. Without という: ...predicate + こと(NOUN) + に(ADP) + は(ADP) + なら(VERB) + ない(AUX)
 * 3. こと can have dep=compound (attaches to preceding predicate) or dep=obl (object of root)
 * 4. なら can have dep=fixed (fixed expression with こと) or dep=root (main verb)
 */
export default linguisticRule('ことにはならない', (r) => {
  r.either(
    // Pattern 1: With という (emphatic quoting)
    // Structure: ...preceding predicate + という + ことにはならない
    (b) => {
      // Preceding predicate (verb, noun phrase, or adjective clause)
      const pred = b.tok({}, 'pred');

      // Optional: と (quotative particle)
      const to = b.tok({ lemma: 'と' }, 'to');
      b.inOrder(pred, to, 5);

      // いう (quotative verb in 連体形)
      const iu = b.verb({ lemma: 'いう', inflectionForm: '連体形-一般' }, 'iu');
      b.inOrder(to, iu, 1);

      // こと (nominalizer)
      // Can be compound with iu or obl (object) with root
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.either(
        // 1a: こと compounds with いう
        (b2) => {
          b2.headChild(iu, koto, 'compound');
          b2.inOrder(iu, koto, 1);
        },
        // 1b: こと is object of later root
        (b2) => {
          b2.inOrder(iu, koto, 2);
        }
      );

      // に (case marker)
      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);
      b.caseMarker(koto, ni);

      // は (topic marker)
      const wa = b.particle('は', 'wa');
      b.inOrder(ni, wa, 1);

      // なら (verb なる in 未然形-一般/negative base form)
      // Can be fixed with こと or be the root
      const nara = b.verb({ lemma: 'なる', inflectionForm: '未然形-一般' }, 'nara');
      b.inOrder(wa, nara, 1);
      b.either(
        // 1a: なら is fixed with こと
        (b2) => {
          b2.headChild(koto, nara, 'fixed');
        },
        // 1b: なら is root (main verb)
        (b2) => {
          b2.headChild(pred, nara, 'root');
        }
      );

      // ない (negative auxiliary)
      const nai = b.aux({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');
      b.auxOf(nara, nai);

      b.captureSpan('ことにはならない', pred, nai);
    },

    // Pattern 2: Without いう (direct)
    // Structure: ...preceding predicate + ことにはならない
    (b) => {
      // Preceding predicate
      const pred = b.tok({}, 'pred');

      // こと (nominalizer)
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.either(
        // 2a: こと compounds with predicate
        (b2) => {
          b2.headChild(pred, koto, 'compound');
          b2.inOrder(pred, koto, 5);
        },
        // 2b: こと is within distance (for loose parsing)
        (b2) => {
          b2.inOrder(pred, koto, 5);
        }
      );

      // に (case marker)
      const ni = b.particle('に', 'ni');
      b.inOrder(koto, ni, 1);
      b.caseMarker(koto, ni);

      // は (topic marker)
      const wa = b.particle('は', 'wa');
      b.inOrder(ni, wa, 1);

      // なら (verb なる in 未然形-一般)
      const nara = b.verb({ lemma: 'なる', inflectionForm: '未然形-一般' }, 'nara');
      b.inOrder(wa, nara, 1);
      b.either(
        // 2a: なら is fixed with こと
        (b2) => {
          b2.headChild(koto, nara, 'fixed');
        },
        // 2b: なら is root
        (b2) => {
          b2.headChild(pred, nara, 'root');
        }
      );

      // ない (negative auxiliary)
      const nai = b.aux({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');
      b.auxOf(nara, nai);

      b.captureSpan('ことにはならない', pred, nai);
    }
  );
});
