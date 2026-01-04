import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことにはならない (koto-niwa-naranai) - "it doesn't amount to, it doesn't lead to"
 *
 * Matches: verb-dictionary form + (optional: という) + こと + には + ならない
 *
 * This expression is used at the end of sentences to emphasize that something said
 * earlier is not necessarily the case. It indicates that one condition doesn't
 * guarantee a result, often translated as "just because (A), it doesn't mean (B)".
 *
 * The structure typically appears after:
 * - からといって (kara-to-itte) - "just because"
 * - ても (temo) - "even if"
 *
 * Structure variants:
 * - Verb［る］+ ことにはならない (casual, negative)
 * - Verb［る］+ ことにはなりません (polite, negative)
 * - Verb［る］+ ことにはならないです (polite, negative, softer)
 * - Verb［る］+ (という) + ことにはならない (with という for emphasis)
 *
 * Examples:
 * - 勉強したからといって、合格することにはならない。
 *   (Just because you studied doesn't mean you'll pass.)
 * - いくら上司でも、仕事を全部部下たちに押し付けてもいいことにはならない。
 *   (Just because you're the boss doesn't mean you can force all work on subordinates.)
 * - 殴られたからと言って、殴り返してもいいということにはならない。
 *   (Just because someone hit you doesn't mean it's okay to hit back.)
 * - ５分ノートを見直しただけでは、勉強したことにはならない。
 *   (Just because you reviewed notes for 5 minutes doesn't mean you've studied.)
 *
 * GiNZA parse structure (for "できる" → "できることにはならない"):
 * - できる(VERB) --compound--> こと(NOUN)
 * - こと --fixed--> に(ADP)
 * - こと --fixed--> は(ADP)
 * - なる(VERB) [inflectionForm=終止形-一般] or なら(連用形)
 * - ない/ません(AUX) [negative]
 *
 * For patterns with という:
 * - ...ということにはならない: pred --compound--> と(PART) --compound--> いう(VERB)
 *   --compound--> こと(NOUN) --fixed--> に(ADP) --fixed--> は(ADP)
 *
 * Key discriminators from similar patterns:
 * - ことになる (JLPT3): "it will turn out that, it is decided that" (affirmative outcome)
 * - ことになっている (JLPT2): "it is arranged that" (ongoing state, has ている)
 * - ことにはならない: "it doesn't amount to, doesn't mean that" (negative, has には+ならない)
 *
 * The "には" particle combination is emphatic, contrasting with simple "に" in other patterns.
 */
export default linguisticRule('ことにはならない', (r) => {
  r.either(
    // Branch 1: Casual negative (〜ことにはならない)
    (b) => {
      // Preceding predicate (verb in dictionary form, noun, adjective)
      const pred = b.tok({}, 'pred');

      // Followed by こと (nominalizer) - dep=compound points to pred
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      // Followed by に (case marker)
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      // Followed by は (topic marker)
      const wa = b.tok({ lemma: 'は', dep: 'fixed' }, 'wa');
      b.inOrder(koto, wa, 1);

      // Followed by なら or なる (連用形 or 終止形)
      b.either(
        // 1a: なら (連用形 of なる) + ない
        (b2) => {
          const nara = b2.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-一般' }, 'nara');
          b2.inOrder(wa, nara, 1);

          // Followed by ない (negative auxiliary)
          const nai = b2.aux({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');
          b2.auxOf(pred, nai);

          b2.captureSpan('ことにはならない', pred, nai);
        },
        // 1b: なる (終止形, for 帰ることにはならない where "ならない" is one token)
        (b2) => {
          const naranai = b2.verb({ lemma: 'なる', inflectionForm: '終止形-一般' }, 'naranai');
          b2.inOrder(wa, naranai, 3);

          // Check if followed by negative auxiliary (in some parses)
          b2.optional((b3) => {
            const nai = b3.aux({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');
            b3.auxOf(pred, nai);
            b3.captureSpan('ことにはならない', pred, nai);
          });

          // If no negative aux found, capture at naranai
          if (!b2.captureSpanCalled) {
            b2.captureSpan('ことにはならない', pred, naranai);
          }
        }
      );
    },

    // Branch 2: Casual negative with という (〜ということにはならない)
    (b) => {
      // Preceding predicate (verb in dictionary form, noun, adjective)
      const pred = b.tok({}, 'pred');

      // Followed by と (quotation particle)
      const to = b.particle('と', 'to');
      b.inOrder(pred, to, 1);

      // Followed by いう (verb "to say")
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      b.inOrder(to, iu, 1);

      // Followed by こと (nominalizer) - can have various dependencies
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(iu, koto, 1);

      // Followed by に (case marker)
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      // Followed by は (topic marker)
      const wa = b.tok({ lemma: 'は', dep: 'fixed' }, 'wa');
      b.inOrder(koto, wa, 1);

      // Followed by なら or なる
      b.either(
        // 2a: なら (連用形 of なる) + ない
        (b2) => {
          const nara = b2.verb({ lemma: 'なる', dep: 'fixed' }, 'nara');
          b2.inOrder(wa, nara, 1);

          // Followed by ない (negative auxiliary)
          const nai = b2.aux({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');
          b2.auxOf(pred, nai);

          b2.captureSpan('ことにはならない', pred, nai);
        },
        // 2b: なる (終止形) - when "ならない" is parsed as one token
        (b2) => {
          const naranai = b2.verb({ lemma: 'なる', inflectionForm: '終止形-一般' }, 'naranai');
          b2.inOrder(wa, naranai, 3);

          // Check if followed by negative auxiliary
          b2.optional((b3) => {
            const nai = b3.aux({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');
            b3.auxOf(pred, nai);
            b3.captureSpan('ことにはならない', pred, nai);
          });

          if (!b2.captureSpanCalled) {
            b2.captureSpan('ことにはならない', pred, naranai);
          }
        },
        // 2c: なら (without strict inflectionForm check)
        (b2) => {
          const nara = b2.tok({ text: 'なら', dep: 'fixed' }, 'nara');
          b2.inOrder(wa, nara, 1);

          // Followed by ない (negative auxiliary)
          const nai = b2.aux({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');
          b2.auxOf(pred, nai);

          b2.captureSpan('ことにはならない', pred, nai);
        },
        // 2d: Direct to nai when naranai has text=ならない
        (b2) => {
          // When GiNZA parses "ならない" as a single token with text=ならない, lemma=なる
          const nai = b2.aux({
            lemma: 'ない',
            inflectionForm: '終止形-一般',
            text: 'ならない'
          }, 'nai');
          b2.inOrder(wa, nai, 3);
          b2.auxOf(pred, nai);

          b2.captureSpan('ことにはならない', pred, nai);
        }
      );
    },

    // Branch 3: Polite negative (〜ことにはなりません)
    (b) => {
      const pred = b.tok({}, 'pred');

      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const wa = b.tok({ lemma: 'は', dep: 'fixed' }, 'wa');
      b.inOrder(koto, wa, 1);

      // なり (連用形 of なる)
      const nari = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-一般' }, 'nari');
      b.inOrder(wa, nari, 1);

      // ません (polite negative) - attaches to pred as aux
      const masen = b.aux({ lemma: 'ません', inflectionForm: '終止形-一般' }, 'masen');
      b.auxOf(pred, masen);

      b.captureSpan('ことにはならない', pred, masen);
    },

    // Branch 4: Polite negative with という (〜ということにはなりません)
    (b) => {
      const pred = b.tok({}, 'pred');

      const to = b.particle('と', 'to');
      b.inOrder(pred, to, 1);

      const iu = b.verb({ lemma: 'いう' }, 'iu');
      b.inOrder(to, iu, 1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(iu, koto, 1);

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const wa = b.tok({ lemma: 'は', dep: 'fixed' }, 'wa');
      b.inOrder(koto, wa, 1);

      const nari = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-一般' }, 'nari');
      b.inOrder(wa, nari, 1);

      // ません (polite negative) - attaches to pred as aux
      const masen = b.aux({ lemma: 'ません', inflectionForm: '終止形-一般' }, 'masen');
      b.auxOf(pred, masen);

      b.captureSpan('ことにはならない', pred, masen);
    },

    // Branch 5: Polite negative softer (〜ことにはならないです)
    (b) => {
      const pred = b.tok({}, 'pred');

      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const wa = b.tok({ lemma: 'は', dep: 'fixed' }, 'wa');
      b.inOrder(koto, wa, 1);

      // Followed by なら (連用形 of なる)
      const nara = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-一般' }, 'nara');
      b.inOrder(wa, nara, 1);

      // ない (negative)
      const nai = b.aux({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');
      b.auxOf(pred, nai);

      // です (polite copula) - attaches to pred as aux
      const desu = b.aux({ lemma: 'です', inflectionForm: '終止形-一般' }, 'desu');
      b.auxOf(pred, desu);

      b.captureSpan('ことにはならない', pred, desu);
    },

    // Branch 6: Polite negative softer with という (〜ということにはならないです)
    (b) => {
      const pred = b.tok({}, 'pred');

      const to = b.particle('と', 'to');
      b.inOrder(pred, to, 1);

      const iu = b.verb({ lemma: 'いう' }, 'iu');
      b.inOrder(to, iu, 1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(iu, koto, 1);

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const wa = b.tok({ lemma: 'は', dep: 'fixed' }, 'wa');
      b.inOrder(koto, wa, 1);

      const nara = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-一般' }, 'nara');
      b.inOrder(wa, nara, 1);

      // ない (negative)
      const nai = b.aux({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');
      b.auxOf(pred, nai);

      // です (polite copula) - attaches to pred as aux
      const desu = b.aux({ lemma: 'です', inflectionForm: '終止形-一般' }, 'desu');
      b.auxOf(pred, desu);

      b.captureSpan('ことにはならない', pred, desu);
    }
  );
});
