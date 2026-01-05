import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('かは-によって違う', (r) => {
  // Pattern: question + かは + によって + 違う/違います
  // Meaning: "(A) depends on (B)" or "(A) differs depending on (B)"
  //
  // This grammar point expresses that an uncertain outcome (A) will differ
  // or change depending on factor (B).
  //
  // Variations:
  // - かは〜によって違う (casual "differs depending on")
  // - かは〜によって違います (polite "differs depending on")
  // - かは〜による (short form, "depends on" - 違う omitted)
  // - かは〜によります (polite short form)
  //
  // Key constraint: Must follow a question clause (ending in かは)
  // Note: GiNZA tokenizes かは inconsistently:
  //   - Sometimes as single token: かは (PART/ADP)
  //   - Sometimes as separate: か (PART) + は (ADP)
  //
  // Examples:
  // - 好きか嫌いかは人によって違う (Whether they like it differs by person)
  // - 高いかどうかは季節によって違う (Whether it's expensive differs by season)
  // - かは人による (depends on the person - 違う omitted)

  r.either(
    // Pattern 1a: かは as single token + によって + 違う (casual)
    (b) => {
      const kaha = b.tok({ text: 'かは', posOneOf: ['PART', 'ADP'] }, 'kaha');
      const ni = b.particle('に', 'ni');
      const yotte = b.verb({ text: 'よって', lemma: 'よる' }, 'yotte');
      const chigau = b.verb({ lemma: '違う' }, 'chigau');

      b.inOrder(kaha, ni, 2);
      b.inOrder(ni, yotte, 1);
      b.inOrder(yotte, chigau, 3);
      b.captureSpan('かは-によって違う', kaha, chigau);
    },

    // Pattern 1b: か + は as separate tokens + によって + 違う (casual)
    (b) => {
      const ka = b.tok({ text: 'か', posOneOf: ['PART', 'ADP'] }, 'ka');
      const wa = b.particle('は', 'wa');
      const ni = b.particle('に', 'ni');
      const yotte = b.verb({ text: 'よって', lemma: 'よる' }, 'yotte');
      const chigau = b.verb({ lemma: '違う' }, 'chigau');

      b.inOrder(ka, wa, 5);  // Allow かどうかは pattern (up to 5 tokens)
      b.inOrder(wa, ni, 5);  // Allow words like "人それぞれ" between は and に
      b.inOrder(ni, yotte, 1);
      b.inOrder(yotte, chigau, 3);
      b.captureSpan('かは-によって違う', ka, chigau);
    },

    // Pattern 2a: かは as single token + によって + 違います (polite)
    (b) => {
      const kaha = b.tok({ text: 'かは', posOneOf: ['PART', 'ADP'] }, 'kaha');
      const ni = b.particle('に', 'ni');
      const yotte = b.verb({ text: 'よって', lemma: 'よる' }, 'yotte');
      const chigaimasu = b.verb({ lemma: '違う' }, 'chigaimasu');

      b.inOrder(kaha, ni, 5);  // Allow words between かは and に
      b.inOrder(ni, yotte, 1);
      b.inOrder(yotte, chigaimasu, 3);
      b.captureSpan('かは-によって違います', kaha, chigaimasu);
    },

    // Pattern 2b: か + は as separate tokens + によって + 違います (polite)
    (b) => {
      const ka = b.tok({ text: 'か', posOneOf: ['PART', 'ADP'] }, 'ka');
      const wa = b.particle('は', 'wa');
      const ni = b.particle('に', 'ni');
      const yotte = b.verb({ text: 'よって', lemma: 'よる' }, 'yotte');
      const chigaimasu = b.verb({ lemma: '違う' }, 'chigaimasu');

      b.inOrder(ka, wa, 5);  // Allow かどうかは pattern (up to 5 tokens)
      b.inOrder(wa, ni, 5);  // Allow words between は and に
      b.inOrder(ni, yotte, 1);
      b.inOrder(yotte, chigaimasu, 3);
      b.captureSpan('かは-によって違います', ka, chigaimasu);
    },

    // Pattern 3a: かは as single token + による (short form)
    (b) => {
      const kaha = b.tok({ text: 'かは', posOneOf: ['PART', 'ADP'] }, 'kaha');
      const ni = b.particle('に', 'ni');
      const yoru = b.verb({ text: 'よる', lemma: 'よる' }, 'yoru');

      b.inOrder(kaha, ni, 5);  // Allow words between かは and に
      b.inOrder(ni, yoru, 1);
      b.captureSpan('かは-による', kaha, yoru);
    },

    // Pattern 3b: か + は as separate tokens + による (short form)
    (b) => {
      const ka = b.tok({ text: 'か', posOneOf: ['PART', 'ADP'] }, 'ka');
      const wa = b.particle('は', 'wa');
      const ni = b.particle('に', 'ni');
      const yoru = b.verb({ text: 'よる', lemma: 'よる' }, 'yoru');

      b.inOrder(ka, wa, 5);  // Allow かどうかは pattern (up to 5 tokens)
      b.inOrder(wa, ni, 5);  // Allow words between は and に
      b.inOrder(ni, yoru, 1);
      b.captureSpan('かは-による', ka, yoru);
    },

    // Pattern 4a: かは as single token + よります (polite short form)
    (b) => {
      const kaha = b.tok({ text: 'かは', posOneOf: ['PART', 'ADP'] }, 'kaha');
      const ni = b.particle('に', 'ni');
      const yorimasu = b.verb({ lemma: 'よる' }, 'yorimasu');

      b.inOrder(kaha, ni, 5);  // Allow words between かは and に
      b.inOrder(ni, yorimasu, 1);
      b.captureSpan('かは-によります', kaha, yorimasu);
    },

    // Pattern 4b: か + は as separate tokens + よります (polite short form)
    (b) => {
      const ka = b.tok({ text: 'か', posOneOf: ['PART', 'ADP'] }, 'ka');
      const wa = b.particle('は', 'wa');
      const ni = b.particle('に', 'ni');
      const yorimasu = b.verb({ lemma: 'よる' }, 'yorimasu');

      b.inOrder(ka, wa, 5);  // Allow かどうかは pattern (up to 5 tokens)
      b.inOrder(wa, ni, 5);  // Allow words between は and に
      b.inOrder(ni, yorimasu, 1);
      b.captureSpan('かは-によります', ka, yorimasu);
    }
  );
});
