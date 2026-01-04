import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ないでください', (r) => {
  // Polite negative request form: verb nai-form + で + ください (please don't)
  // Examples: 行かないでください (please don't go), 食べないでください (please don't eat)
  //
  // Structure: Verb[未然形] + ない + で + ください
  // The verb is in mizenkei (irrealis form) followed by auxiliary ない
  // Then で (te-form connector in negative context)
  // Finally ください (polite request marker)

  // The で particle (connective form in negative context)
  // In negative requests, GiNZA assigns lemma=で, pos=SCONJ, dep=mark
  const de = r.tok(
    {
      text: 'で',
      lemma: 'で',
      pos: 'SCONJ',
      dep: 'mark',
    },
    'de'
  );

  // ください as auxiliary (polite request marker)
  // Key discriminators:
  // - lemma=くださる (dictionary form)
  // - pos=AUX (not VERB - when used independently as "give me", it's pos=VERB)
  // - dep=fixed (attached to the preceding で)
  // - inflectionForm=命令形 (imperative form)
  const kudasai = r.aux(
    {
      text: 'ください',
      lemma: 'くださる',
      dep: 'fixed',
      inflectionForm: '命令形',
    },
    'kudasai'
  );

  // kudasai attaches to the で form as a 'fixed' dependency
  // GiNZA: kudasai.head points to で (kudasai --fixed--> de)
  r.headChild(de, kudasai, 'fixed');

  // The ない auxiliary (negative marker)
  // This attaches to the verb with dep=aux
  // We don't need to explicitly match the verb - the pattern de+kudasai
  // is sufficient to identify this grammar structure
  const nai = r.aux(
    {
      text: 'ない',
      lemma: 'ない',
    },
    'nai'
  );

  // で comes after ない (inOrder constraint)
  r.inOrder(nai, de, 1);

  // Capture from ない to kudasai (the full negative request pattern)
  r.captureSpan('ないでください', nai, kudasai);
});
