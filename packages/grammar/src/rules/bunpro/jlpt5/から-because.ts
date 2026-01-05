import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('から-because', (r) => {
  r.either(
    // Pattern 1: Noun + だ + から (e.g., 会社だから, 冬だから, お菓子だから)
    (r1) => {
      const noun = r1.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const da = r1.tok({ text: 'だ', pos: 'AUX', depOneOf: ['cop', 'aux'] }, 'da');
      const kara = r1.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });

      r1.inOrder(noun, da, 1);
      r1.inOrder(da, kara, 1);
      r1.headChild(noun, da);
      r1.headChild(noun, kara);

      r1.captureSpan('から-because', noun, kara);
    },
    // Pattern 2: な-Adjective + だ/です + から (e.g., 便利だから, 好きですから)
    (r2) => {
      const naAdj = r2.adj({}, 'naAdj');
      const desu = r2.tok({ textOneOf: ['だ', 'です'], pos: 'AUX' }, 'desu');
      const kara = r2.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });

      r2.inOrder(naAdj, desu, 1);
      r2.inOrder(desu, kara, 1);
      r2.headChild(naAdj, desu);
      r2.headChild(naAdj, kara);

      r2.captureSpan('から-because', naAdj, kara);
    },
    // Pattern 3: い-Adjective + から (e.g., 優しいから, 辛いから)
    (r3) => {
      const iAdj = r3.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const kara = r3.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });

      r3.inOrder(iAdj, kara, 1);
      r3.headChild(iAdj, kara);

      r3.captureSpan('から-because', iAdj, kara);
    },
    // Pattern 4: Verb + (た/し)? + から (e.g., 食べたから, 行くから, 勉強したから, いるから)
    // Auxiliary た or し can appear between verb and から (optional)
    (r4) => {
      const verb = r4.verb({}, 'verb');
      const aux = r4.tok({ lemmaOneOf: ['た', 'し'], pos: 'AUX' }, 'aux');
      const kara = r4.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });

      r4.inOrder(verb, aux, 2);
      r4.inOrder(aux, kara, 1);
      r4.auxOf(verb, aux);
      r4.headChild(verb, kara);

      r4.captureSpan('から-because', verb, kara);
    },
    // Pattern 5: Verb + から (without aux, e.g., いるから, 行くから)
    (r5) => {
      const verb = r5.verb({}, 'verb');
      const kara = r5.particle('から', 'kara', { pos: 'SCONJ', dep: 'mark' });

      r5.inOrder(verb, kara, 1);
      r5.headChild(verb, kara);

      r5.captureSpan('から-because', verb, kara);
    }
  );
});
