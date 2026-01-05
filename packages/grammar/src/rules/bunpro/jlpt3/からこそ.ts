import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('からこそ', (r) => {
  r.either(
    // Pattern 1: Noun/PROPN/PRON + だ + から + こそ (e.g., 君だからこそ, 操作が簡単だからこそ)
    // こそ is ADP with dep=case or dep=mark (GiNZA inconsistency)
    (r1) => {
      const noun = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = r1.tok({ text: 'だ', pos: 'AUX', depOneOf: ['cop', 'aux'] }, 'da');
      const kara = r1.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });
      const koso = r1.tok({ text: 'こそ', pos: 'ADP', depOneOf: ['case', 'mark'] }, 'koso');

      r1.inOrder(noun, da, 1);
      r1.inOrder(da, kara, 1);
      r1.inOrder(kara, koso, 1);
      r1.headChild(noun, da);
      r1.headChild(noun, kara);
      r1.headChild(noun, koso);

      r1.captureSpan('からこそ', noun, koso);
    },
    // Pattern 2: な-Adjective + だ + から + こそ (e.g., 綺麗な川だからこそ)
    (r2) => {
      const naAdj = r2.adj({}, 'naAdj');
      const da = r2.tok({ text: 'だ', pos: 'AUX' }, 'da');
      const kara = r2.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });
      const koso = r2.tok({ text: 'こそ', pos: 'ADP', depOneOf: ['case', 'mark'] }, 'koso');

      r2.inOrder(naAdj, da, 1);
      r2.inOrder(da, kara, 1);
      r2.inOrder(kara, koso, 1);
      r2.headChild(naAdj, da);
      r2.headChild(naAdj, kara);
      r2.headChild(naAdj, koso);

      r2.captureSpan('からこそ', naAdj, koso);
    },
    // Pattern 3: い-Adjective + から + こそ (e.g., 難しいからこそ)
    (r3) => {
      const iAdj = r3.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const kara = r3.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });
      const koso = r3.tok({ text: 'こそ', pos: 'ADP', depOneOf: ['case', 'mark'] }, 'koso');

      r3.inOrder(iAdj, kara, 1);
      r3.inOrder(kara, koso, 1);
      r3.headChild(iAdj, kara);
      r3.headChild(iAdj, koso);

      r3.captureSpan('からこそ', iAdj, koso);
    },
    // Pattern 4: Verb + (auxiliaries) + た + から + こそ (e.g., 努力したからこそ, 教えて貰ったからこそ)
    // Allow up to 4 tokens between verb and た to handle complex auxiliary chains
    (r4) => {
      const verb = r4.verb({}, 'verb');
      const ta = r4.aux({ lemma: 'た' }, 'ta');
      const kara = r4.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });
      const koso = r4.tok({ text: 'こそ', pos: 'ADP', depOneOf: ['case', 'mark'] }, 'koso');

      r4.inOrder(verb, ta, 4);
      r4.inOrder(ta, kara, 1);
      r4.inOrder(kara, koso, 1);
      r4.headChild(verb, kara);
      r4.headChild(verb, koso);

      r4.captureSpan('からこそ', verb, koso);
    },
    // Pattern 5: Verb + から + こそ (e.g., いるからこそ)
    (r5) => {
      const verb = r5.verb({}, 'verb');
      const kara = r5.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });
      const koso = r5.tok({ text: 'こそ', pos: 'ADP', depOneOf: ['case', 'mark'] }, 'koso');

      r5.inOrder(verb, kara, 1);
      r5.inOrder(kara, koso, 1);
      r5.headChild(verb, kara);
      r5.headChild(verb, koso);

      r5.captureSpan('からこそ', verb, koso);
    },
    // Pattern 6: Verb + て + いる/ある + から + こそ (e.g., 働いているからこそ, しているからこそ)
    // Note: て is SCONJ with dep=mark, いる is VERB with dep=fixed
    (r6) => {
      const verb = r6.verb({}, 'verb');
      const te = r6.tok({ lemma: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = r6.tok({ lemmaOneOf: ['いる', 'ある'], dep: 'fixed' }, 'iru');
      const kara = r6.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });
      const koso = r6.tok({ text: 'こそ', pos: 'ADP', depOneOf: ['case', 'mark'] }, 'koso');

      r6.inOrder(verb, te, 2);
      r6.inOrder(te, iru, 1);
      r6.inOrder(iru, kara, 1);
      r6.inOrder(kara, koso, 1);
      r6.headChild(verb, kara);
      r6.headChild(verb, koso);

      r6.captureSpan('からこそ', verb, koso);
    }
  );
});
