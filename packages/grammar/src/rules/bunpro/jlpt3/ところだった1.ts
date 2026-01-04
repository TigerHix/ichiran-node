import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ところだった1 - "was just about to", "almost did", "on the verge of"
 *
 * Matches verb dictionary form + ところ + だった to express that something
 * was about to happen but didn't actually occur.
 *
 * Structure:
 * - Verb［る］+ ところ + だった
 * - Verb［ない］+ ところ + だった
 *
 * This grammar point emphasizes that the action was "about to" happen but
 * never actually eventuated (or another more important event happened at that moment).
 * Can be translated as "was about to", "almost did", "nearly did", or "on the verge of".
 *
 * Examples:
 * - 電車に傘を忘れるところだった (I was about to forget my umbrella on the train)
 * - もう少しで遅刻するところだった (I was on the verge of being late)
 * - 店は閉店するところだった (The shop was just about to close)
 * - やばい、大事な書類を捨てるところだった (Oh shoot, I was just about to throw away an important document)
 *
 * GiNZA parse structure:
 * - Verb (VERB) - dictionary form or negative form
 * - ところ (NOUN or SCONJ) - nominal suffix meaning "place/situation"
 * - だった (AUX) - past copula indicating past tense
 *
 * Key discriminators:
 * - Must follow verb dictionary form (る-form) or negative form (ない-form)
 * - Different from たところだ (just finished) - this uses dictionary form
 * - Different from ところだ (about to do now) - this uses past copula だった
 */
export default linguisticRule('ところだった1', (r) => {
  // Verb in dictionary form (る-form) or negative form (ない-form)
  const verb = r.verb({}, 'verb');

  // ところ - nominal suffix meaning "place/situation"
  // GiNZA parses as NOUN with dep=root when sentence-final
  const tokoro = r.noun({
    lemma: 'ところ',
  }, 'tokoro');

  // ところ must immediately follow verb
  r.inOrder(verb, tokoro, 3);

  // GiNZA parses だった as TWO tokens: だっ (cop) + た (aux)
  // だっ - copula stem
  const dattsu = r.aux({
    lemma: 'だ',
    inflectionForm: '連用形-促音便',
  }, 'dattsu');
  r.copulaOf(tokoro, dattsu);

  // た - past tense auxiliary
  const ta = r.aux({
    lemma: 'た',
  }, 'ta');
  r.auxOf(tokoro, ta);

  // Capture from verb to た
  r.captureSpan('ところだった1', verb, ta);
});
