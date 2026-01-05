import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('てください', (r) => {
  // Polite request form: verb te-form + ください (please do)
  // Examples: 書いてください (please write), 待ってください (please wait)
  // Handles both て and で forms

  // The te/de particle (te-form marker)
  // Note: GiNZA assigns different lemmas for て vs で
  const te = r.tok(
    {
      textOneOf: ['て', 'で'],
      lemmaOneOf: ['て', 'で'],
      pos: 'SCONJ',
      dep: 'mark',
    },
    'te'
  );

  // ください as auxiliary (polite request marker)
  // Key discriminator: lemma=くださる, pos=AUX (not VERB), dep=fixed
  // When ください is used independently (e.g., 水をください), it's pos=VERB, dep=root
  const kudasai = r.aux(
    {
      text: 'ください',
      lemma: 'くださる',
      dep: 'fixed',
      inflectionForm: '命令形',
    },
    'kudasai'
  );

  // kudasai attaches to the te/de form as a 'fixed' dependency
  // GiNZA: kudasai.head points to te (kudasai --fixed--> te)
  r.headChild(te, kudasai, 'fixed');

  // Capture from te to kudasai (includes compound te-forms like 待っていてください)
  r.captureSpan('てください', te, kudasai);
});
