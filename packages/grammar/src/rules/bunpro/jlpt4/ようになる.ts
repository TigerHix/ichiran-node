import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ようになる - To reach the point that, To come to be that, To turn into
 *
 * Matches patterns where verb + ようになる expresses a natural change
 * or development of state, meaning "becomes able to" or "comes to be".
 *
 * Structure:
 * - Verb (potential/negative/dictionary) + よう + に + なる
 *
 * Examples:
 * - 泳げるようになった (became able to swim)
 * - 話せるようになる (will become able to speak)
 * - 使えないようになる (will become unable to use)
 * - できるようになりたい (want to become able to do)
 *
 * Key discriminators:
 * - よう and に are both AUX tokens with dep=aux, attached to the preceding verb
 * - なる is a VERB with lemma=なる (can be various conjugations)
 * - Different from ように (purpose) and ようにする (effort)
 * - Must not match plain なる without ように
 *
 * Common verb forms:
 * - Potential: 泳げる, 話せる, できる
 * - Negative: 使えない, 食べない
 * - Dictionary: 勉強する (less common but possible)
 *
 * GiNZA parsing notes:
 * - よう is AUX, lemma=よう, dep=aux, points to verb
 * - に is AUX, lemma=だ, inflectionForm=連用形-ニ, dep=aux, points to verb
 * - なる is VERB, lemma=なる (can be various forms: なる, なった, になり, etc.)
 */
export default linguisticRule('ようになる', (r) => {
  // Match verb + よう + に + なる pattern
  const verb = r.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
  const you = r.aux({ lemma: 'よう' }, 'you');
  const ni = r.aux({ lemma: 'だ', inflectionForm: '連用形-ニ' }, 'ni');
  const naru = r.verb({ lemma: 'なる' }, 'naru');

  // Require aux dependencies from よう and に to verb
  r.auxOf(verb, you);
  r.auxOf(verb, ni);

  // Require ordering: verb -> you -> ni -> naru
  r.inOrder(verb, you, 5);
  r.inOrder(you, ni, 1);
  r.inOrder(ni, naru, 5);

  // Capture the full pattern
  r.captureSpan('ようになる', verb, naru);
});
