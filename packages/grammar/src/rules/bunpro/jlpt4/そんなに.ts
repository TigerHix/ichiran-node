import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: そんなに - So (much), That much, Like that
 *
 * Adverbial expression indicating degree or extent.
 * Used to express "that much" or "to that extent".
 *
 * Structures:
 * - そんなに + Verb
 * - そんなに + ［い］Adjective
 * - そんなに + ［な］Adjective
 *
 * Examples:
 * - そんなに食べたらお腹を壊すよ (If you eat that much, you'll get a stomach ache)
 * - このケーキはそんなに甘いんですか (Is this cake really that sweet?)
 * - そんなに痛いなら、病院に行ったほうがいい (If it hurts that much, you should go to the hospital)
 * - そんなに頑張っても、彼みたいに出来ない (Even if I try that hard, I can't do it like him)
 *
 * GiNZA parse structure:
 * - そんな (lemma=そんな, pos=ADJ) + に (lemma=だ, pos=AUX, inflectionForm=連用形-ニ)
 * - The に is an auxiliary verb (copula だ in conjunctive form), not a particle
 *
 * Key discriminators:
 * - そんな is an adjective (ADJ, tag=形状詞-一般)
 * - に is an auxiliary (AUX) with lemma=だ and inflectionForm=連用形-ニ
 * - に has dep=aux attached to そんな
 *
 * Note: This rule matches そんなに specifically. Variants like こんなに, あんなに, どんなに
 * are grammatically identical but are separate words (kosoado words) and could be handled
 * separately if needed.
 */
export default bunproLinguisticRule('そんなに', (r) => {
  // そんな + に (copula in conjunctive form)
  // GiNZA parses as two tokens: そんな (ADJ) + に (AUX with lemma=だ)
  const sonna = r.tok({
    text: 'そんな',
    lemma: 'そんな',
    pos: 'ADJ',
  }, 'sonna');

  const ni = r.aux({
    text: 'に',
    lemma: 'だ',
    inflectionForm: '連用形-ニ',
  }, 'ni');

  r.auxOf(sonna, ni);
  r.captureSpan('そんなに', sonna, ni);
});
