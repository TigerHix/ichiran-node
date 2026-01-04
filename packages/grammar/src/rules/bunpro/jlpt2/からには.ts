import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('からには', (r) => {
  r.either(
    // Pattern 1: Noun/PROPN/PRON + である + からには (e.g., 教師であるからには)
    (r1) => {
      const noun = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const dearu = r1.aux({ lemma: 'である' }, 'dearu');
      const kara = r1.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r1.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case'] }, 'ni');
      const wa = r1.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case', 'mark'] }, 'wa');

      r1.inOrder(noun, dearu, 1);
      r1.inOrder(dearu, kara, 1);
      r1.inOrder(kara, ni, 1);
      r1.inOrder(ni, wa, 1);
      r1.headChild(noun, kara);
      r1.headChild(noun, ni);
      r1.headChild(noun, wa);

      r1.captureSpan('からには', noun, wa);
    },
    // Pattern 2: な-Adjective + である + からには (e.g., 有名であるからには)
    (r2) => {
      const naAdj = r2.adj({}, 'naAdj');
      const dearu = r2.aux({ lemma: 'である' }, 'dearu');
      const kara = r2.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r2.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case'] }, 'ni');
      const wa = r2.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case', 'mark'] }, 'wa');

      r2.inOrder(naAdj, dearu, 1);
      r2.inOrder(dearu, kara, 1);
      r2.inOrder(kara, ni, 1);
      r2.inOrder(ni, wa, 1);
      r2.headChild(naAdj, kara);
      r2.headChild(naAdj, ni);
      r2.headChild(naAdj, wa);

      r2.captureSpan('からには', naAdj, wa);
    },
    // Pattern 3: い-Adjective + からには (e.g., 高いからには)
    (r3) => {
      const iAdj = r3.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const kara = r3.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r3.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case'] }, 'ni');
      const wa = r3.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case', 'mark'] }, 'wa');

      r3.inOrder(iAdj, kara, 1);
      r3.inOrder(kara, ni, 1);
      r3.inOrder(ni, wa, 1);
      r3.headChild(iAdj, kara);
      r3.headChild(iAdj, ni);
      r3.headChild(iAdj, wa);

      r3.captureSpan('からには', iAdj, wa);
    },
    // Pattern 4: Verb + (auxiliaries) + た + からには (e.g., 来たからには, 買ったからには)
    // Allow up to 4 tokens between verb and た to handle complex auxiliary chains
    (r4) => {
      const verb = r4.verb({}, 'verb');
      const ta = r4.aux({ lemma: 'た' }, 'ta');
      const kara = r4.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r4.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case'] }, 'ni');
      const wa = r4.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case', 'mark'] }, 'wa');

      r4.inOrder(verb, ta, 4);
      r4.inOrder(ta, kara, 1);
      r4.inOrder(kara, ni, 1);
      r4.inOrder(ni, wa, 1);
      r4.headChild(verb, kara);
      r4.headChild(verb, ni);
      r4.headChild(verb, wa);

      r4.captureSpan('からには', verb, wa);
    },
    // Pattern 4b: Verb + (auxiliaries) + た + からには (alternative dependency pattern)
    // Allow up to 4 tokens between verb and た to handle complex auxiliary chains
    // に and は depend on から instead of verb
    (r4b) => {
      const verb = r4b.verb({}, 'verb');
      const ta = r4b.aux({ lemma: 'た' }, 'ta');
      const kara = r4b.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r4b.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], dep: 'fixed' }, 'ni');
      const wa = r4b.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], dep: 'fixed' }, 'wa');

      r4b.inOrder(verb, ta, 4);
      r4b.inOrder(ta, kara, 1);
      r4b.inOrder(kara, ni, 1);
      r4b.inOrder(ni, wa, 1);
      r4b.headChild(verb, kara);
      r4b.headChild(kara, ni);
      r4b.headChild(kara, wa);

      r4b.captureSpan('からには', verb, wa);
    },
    // Pattern 5: Verb (dictionary form) + からには (e.g., やるからには, 行くからには)
    // All particles depend on verb
    (r5) => {
      const verb = r5.verb({}, 'verb');
      const kara = r5.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r5.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case'] }, 'ni');
      const wa = r5.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case', 'mark'] }, 'wa');

      r5.inOrder(verb, kara, 2);  // Allow 1 AUX between verb and kara (for suru-verbs)
      r5.inOrder(kara, ni, 1);
      r5.inOrder(ni, wa, 1);
      r5.headChild(verb, kara);
      r5.headChild(verb, ni);
      r5.headChild(verb, wa);

      r5.captureSpan('からには', verb, wa);
    },
    // Pattern 5b: Verb (dictionary form) + からには (alternative dependency pattern)
    // に and は depend on から instead of verb
    (r5b) => {
      const verb = r5b.verb({}, 'verb');
      const kara = r5b.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r5b.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], dep: 'fixed' }, 'ni');
      const wa = r5b.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], dep: 'fixed' }, 'wa');

      r5b.inOrder(verb, kara, 2);  // Allow 1 AUX between verb and kara (for suru-verbs)
      r5b.inOrder(kara, ni, 1);
      r5b.inOrder(ni, wa, 1);
      r5b.headChild(verb, kara);
      r5b.headChild(kara, ni);
      r5b.headChild(kara, wa);

      r5b.captureSpan('からには', verb, wa);
    },
    // Pattern 5c: Verb (dictionary form) + からには (mixed dependency pattern)
    // に depends on から, but は depends on verb
    (r5c) => {
      const verb = r5c.verb({}, 'verb');
      const kara = r5c.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r5c.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], dep: 'fixed' }, 'ni');
      const wa = r5c.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['case', 'fixed', 'mark'] }, 'wa');

      r5c.inOrder(verb, kara, 2);  // Allow 1 AUX between verb and kara (for suru-verbs)
      r5c.inOrder(kara, ni, 1);
      r5c.inOrder(ni, wa, 1);
      r5c.headChild(verb, kara);
      r5c.headChild(kara, ni);
      r5c.headChild(verb, wa);

      r5c.captureSpan('からには', verb, wa);
    },
    // Pattern 6: Verb + て + いる/ある + からには (e.g., やっているからには)
    // All particles depend on verb
    (r6) => {
      const verb = r6.verb({}, 'verb');
      const te = r6.tok({ lemma: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = r6.tok({ lemmaOneOf: ['いる', 'ある'], dep: 'fixed' }, 'iru');
      const kara = r6.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r6.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case'] }, 'ni');
      const wa = r6.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['fixed', 'case', 'mark'] }, 'wa');

      r6.inOrder(verb, te, 2);
      r6.inOrder(te, iru, 1);
      r6.inOrder(iru, kara, 1);
      r6.inOrder(kara, ni, 1);
      r6.inOrder(ni, wa, 1);
      r6.headChild(verb, kara);
      r6.headChild(verb, ni);
      r6.headChild(verb, wa);

      r6.captureSpan('からには', verb, wa);
    },
    // Pattern 6b: Verb + て + いる/ある + からには (alternative dependency pattern)
    // に and は depend on から instead of verb
    (r6b) => {
      const verb = r6b.verb({}, 'verb');
      const te = r6b.tok({ lemma: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = r6b.tok({ lemmaOneOf: ['いる', 'ある'], dep: 'fixed' }, 'iru');
      const kara = r6b.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'], depOneOf: ['mark', 'case'] }, 'kara');
      const ni = r6b.tok({ text: 'に', posOneOf: ['SCONJ', 'ADP'], dep: 'fixed' }, 'ni');
      const wa = r6b.tok({ text: 'は', posOneOf: ['SCONJ', 'ADP'], dep: 'fixed' }, 'wa');

      r6b.inOrder(verb, te, 2);
      r6b.inOrder(te, iru, 1);
      r6b.inOrder(iru, kara, 1);
      r6b.inOrder(kara, ni, 1);
      r6b.inOrder(ni, wa, 1);
      r6b.headChild(verb, kara);
      r6b.headChild(kara, ni);
      r6b.headChild(kara, wa);

      r6b.captureSpan('からには', verb, wa);
    },
    // Pattern 7: からは (shorter form, more formal/written) - Noun + である + からは
    (r7) => {
      const noun = r7.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const dearu = r7.aux({ lemma: 'である' }, 'dearu');
      const kara = r7.tok({ text: 'から', pos: 'ADP', dep: 'case' }, 'kara');
      const wa = r7.tok({ text: 'は', pos: 'ADP', dep: 'case' }, 'wa');

      r7.inOrder(noun, dearu, 1);
      r7.inOrder(dearu, kara, 1);
      r7.inOrder(kara, wa, 1);
      r7.headChild(noun, kara);
      r7.headChild(noun, wa);

      r7.captureSpan('からには', noun, wa);
    },
    // Pattern 8: からは (shorter form) - Verb + た/る + からは
    (r8) => {
      const verb = r8.verb({}, 'verb');
      const kara = r8.tok({ text: 'から', pos: 'ADP', dep: 'case' }, 'kara');
      const wa = r8.tok({ text: 'は', pos: 'ADP', dep: 'case' }, 'wa');

      r8.inOrder(verb, kara, 1);
      r8.inOrder(kara, wa, 1);
      r8.headChild(verb, kara);
      r8.headChild(verb, wa);

      r8.captureSpan('からには', verb, wa);
    }
  );
});
