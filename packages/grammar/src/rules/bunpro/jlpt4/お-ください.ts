import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('お-ください', (r) => {
  // Honorific prefix お or ご
  const o = r.tok({ textOneOf: ['お', 'ご'], pos: 'NOUN', dep: 'compound' }, 'o');

  // Verb stem (can be various inflection forms: 連用形 for simple stems, 未然形 for causative, etc.)
  const verbStem = r.verb({ posOneOf: ['VERB', 'NOUN'] }, 'verbStem');

  // ください (imperative form of くださる)
  const kudasai = r.aux({ lemma: 'くださる', inflectionForm: '命令形' }, 'kudasai');

  // Structural constraints: お + verb-stem + ください in order (close together)
  r.inOrder(o, verbStem, 1);
  r.inOrder(verbStem, kudasai, 3); // Allow up to 3 tokens for verb + auxiliaries

  // Dependency: ください attaches as aux to the verb
  r.auxOf(verbStem, kudasai);

  // Ensure we don't match te-form + kudasai (e.g., 書いてください)
  r.not((nr) => {
    const teForm = nr.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' });
    nr.inOrder(verbStem, teForm, 3);
  });

  // Ensure we don't match してください form (humble form with suru verb)
  r.not((nr) => {
    const shi = nr.aux({ lemma: 'する', inflectionForm: '連用形-一般' });
    const teForm = nr.tok({ text: 'て', pos: 'SCONJ' });
    nr.inOrder(verbStem, shi, 2);
    nr.inOrder(shi, teForm, 1);
  });

  // Capture the full span from お to ください
  r.captureSpan('お-ください', o, kudasai);
});
