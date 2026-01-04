import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('というのは', (r) => {
  // というのは (to iu no wa) - "the thing known as... is", "what we call... is"
  // Meaning: Presents something (A) as a topic to be defined or explained further
  //
  // Patterns:
  // 1. Noun/Phrase + という + の + は (full form)
  // 2. Noun/Phrase + という + の + も (with も for emphasis/reason)
  // 3. Noun/Phrase + と + は (abbreviated, no いう)
  // 4. Noun/Phrase + っ + て (casual speech)
  //
  // Examples:
  // - 人生というのは儚いものだ (Life, the thing known as life, is fleeting)
  // - 雪というのは、自然現象です (Snow, as we know it, is a natural phenomenon)
  // - おかずとはなんですか (What are side dishes?)
  // - 夢って簡単に諦められない (Dreams are hard to give up)
  //
  // Key discriminators:
  // - Different from という (JLPT3) which is: Noun + いう + Noun (no のは/は follows)
  // - Different from かというと (JLPT3) which requires question particle か
  // - Different from ということ (JLPT4) which ends in こと (definitional nominalizer)
  //
  // GiNZA parsing notes:
  // - と is ADP with dep=case (quotational particle)
  // - いう is VERB with lemma=いう
  // - の can be NOUN, PROPN, or PART depending on context
  // - は/も are ADP particles (topic markers)

  r.either(
    // Pattern 1: というのは (full form with topic marker は)
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const no = b.tok({ text: 'の' }, 'no');
      const wa = b.particle('は', 'wa');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, no, 1);
      b.inOrder(no, wa, 1);

      b.captureSpan('というのは', to, wa);
    },

    // Pattern 2: というのも (with も for "also is / the reason is")
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const no = b.tok({ text: 'の' }, 'no');
      const mo = b.particle('も', 'mo');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, no, 1);
      b.inOrder(no, mo, 1);

      b.captureSpan('というのは', to, mo);
    },

    // Pattern 3: とは (abbreviated form - quotational と + topic は, no いう)
    // Note: Must ensure it's not just a simple quotational phrase
    (b) => {
      const to = b.particle('と', 'to');
      const wa = b.particle('は', 'wa');

      b.inOrder(to, wa, 1);

      // Don't match if there's いう between と and は (that's pattern 1)
      // We check this by requiring nothing with lemma=いう between them
      b.captureSpan('というのは', to, wa);
    },

    // Pattern 4: って (casual speech - quotational marker)
    (b) => {
      const tte = b.tok({ text: 'って' }, 'tte');

      b.captureSpan('というのは', tte, tte);
    }
  );
});
