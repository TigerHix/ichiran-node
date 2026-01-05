import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('かというと2', (r) => {
  // Pattern: question word + (の/な) + か + と(いう) + と/いえば
  // Meaning: "if I were to say" / "if I had to say"
  //
  // This grammar point qualifies a personal opinion in response to an implied question
  // Example: 何で日本に引っ越して来たかというと、私の家族が日本に住んでいるからです。
  //         (If I were to say why I moved to Japan, it's because my family lives here.)
  //
  // Variations:
  // - かというと (formal, conditional)
  // - かといえば (conditional form)
  // - かっていうと (casual speech)
  // - With nominalizer: のかというと / なのかというと
  //
  // Key constraint: Follows question words (何, 誰, どの, etc.) optionally with の/な
  //
  // Note: This is similar to かというと1 but used with question words instead of
  // answering "why/why not" questions. かというと1 introduces reasoning, while
  // かというと2 qualifies a personal opinion.
  //
  // Note: GiNZA tokenizes いえば inconsistently:
  // - Sometimes as single token "いえば" with lemma="いう"
  // - Sometimes as separate tokens "いえ" + "ば"
  // We use b.tok() instead of b.aux() for "ば" to match both cases

  const ka = r.particle('か', 'ka');

  r.either(
    // Pattern 1: かというと (formal "if I were to say")
    (b) => {
      const to = b.particle('と', 'to1');
      // Must be "いう" not "いえ" (いえ is part of いえば)
      const iu = b.verb({ text: 'いう' }, 'iu');
      const to2 = b.particle('と', 'to2');

      b.inOrder(ka, to, 1);
      b.inOrder(to, iu, 1);
      b.inOrder(iu, to2, 1);
      b.captureSpan('かというと', ka, to2);
    },
    // Pattern 2a: かといえば (いえば as single token)
    (b) => {
      const to = b.tok({ text: 'と' });
      const ieba = b.tok({ text: 'いえば', lemma: 'いう' });

      b.inOrder(ka, to, 1);
      b.inOrder(to, ieba, 1);
      b.captureSpan('かといえば', ka, ieba);
    },
    // Pattern 2b: かといえば (いえ + ば as separate tokens)
    (b) => {
      const to = b.tok({ text: 'と' });
      const ie = b.tok({ text: 'いえ' });
      const ba = b.tok({ text: 'ば' });

      b.inOrder(ka, to, 1);
      b.inOrder(to, ie, 1);
      b.inOrder(ie, ba, 1);
      b.captureSpan('かといえば', ka, ba);
    },
    // Pattern 3: かっていうと (casual "if I were to say")
    (b) => {
      const tte = b.tok({ text: 'って' });
      const iu = b.verb({ lemma: 'いう' });
      const to = b.particle('と');

      b.inOrder(ka, tte, 1);
      b.inOrder(tte, iu, 1);
      b.inOrder(iu, to, 1);
      b.captureSpan('かっていうと', ka, to);
    },
    // Pattern 4a: かっていえば (いえば as single token)
    (b) => {
      const tte = b.tok({ text: 'って' });
      const ieba = b.tok({ text: 'いえば', lemma: 'いう' });

      b.inOrder(ka, tte, 1);
      b.inOrder(tte, ieba, 1);
      b.captureSpan('かっていえば', ka, ieba);
    },
    // Pattern 4b: かっていえば (いえ + ば as separate tokens)
    (b) => {
      const tte = b.tok({ text: 'って' });
      const ie = b.tok({ text: 'いえ' });
      const ba = b.tok({ text: 'ば' });

      b.inOrder(ka, tte, 1);
      b.inOrder(tte, ie, 1);
      b.inOrder(ie, ba, 1);
      b.captureSpan('かっていえば', ka, ba);
    }
  );
});
