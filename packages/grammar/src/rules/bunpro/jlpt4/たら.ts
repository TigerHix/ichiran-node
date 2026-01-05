import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: たら (conditional / when)
 *
 * Matches verb/adj/noun + たら (conditional form)
 *
 * GiNZA tokenizes たら forms as:
 * - 勉強したら → 勉強(VERB) + し(AUX) + たら(AUX, lemma=た, inflectionForm=仮定形-一般)
 * - 暇だったら → 暇(ADJ) + だっ(AUX) + たら(AUX, lemma=た, inflectionForm=仮定形-一般)
 * - 週末だったら → 週末(NOUN) + だっ(AUX) + たら(AUX, lemma=た, inflectionForm=仮定形-一般)
 *
 * The key is matching the らら token with inflectionForm=仮定形-一般.
 */
export default bunproLinguisticRule('たら', (r) => {
  // Match らら as a conditional auxiliary
  const tara = r.aux({
    textOneOf: ['たら', 'ったら', 'だら', 'なら'],
    lemma: 'た',
    inflectionForm: '仮定形-一般',
  }, 'tara');

  // The らら must come after a verb, auxiliary, or noun
  // It attaches to the preceding word as part of the conditional construction
  r.capture(tara);
});
