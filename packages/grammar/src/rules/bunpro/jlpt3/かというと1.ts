import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('かというと1', (r) => {
  // Pattern: question word + か + と(いう) + と/いえば
  // Meaning: "if (we were to) ask" / "speaking of"
  //
  // This grammar point introduces reasoning or a cause mid-sentence by echoing a question
  // Example: なぜ行かなかったかというと (If you're asking why I didn't go...)
  //
  // Variations:
  // - かというと (formal, conditional)
  // - かといえば (conditional form)
  // - かっていうと (casual speech)
  //
  // Key constraint: Must follow a question clause (marked by の/ん or ending in question form)
  //
  // Note: GiNZA tokenizes いえば inconsistently:
  // - Sometimes as single token "いえば" with lemma="いう"
  // - Sometimes as separate tokens "いえ" + "ば"
  // We use b.tok() instead of b.aux() for "ば" to match both cases

  const ka = r.particle('か', 'ka');

  r.either(
    // Pattern 1: かというと (formal "if we ask")
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
    // Pattern 3: かっていうと (casual "if we ask")
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
      const ba = b.tok({ text: 'ば' });  // Use b.tok() not b.aux() for consistency

      b.inOrder(ka, tte, 1);
      b.inOrder(tte, ie, 1);
      b.inOrder(ie, ba, 1);
      b.captureSpan('かっていえば', ka, ba);
    }
  );
});
