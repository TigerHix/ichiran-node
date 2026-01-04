import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('だろう', (r) => {
  // だろう is the casual conjecture form (volitional form of だ)
  // Attaches to: verbs, i-adjectives, nouns, na-adjectives, adverbs
  // Key discriminators: lemma=だ, inflectionForm=意志推量形

  // Match either だろう or だろ (contracted form)
  const darou = r.aux({
    lemma: 'だ',
    inflectionForm: '意志推量形',
    textOneOf: ['だろう', 'だろ']
  }, 'darou');

  // だろう can attach to various heads:
  // 1. Verbs (dep=aux) - 明日は晴れるだろう
  // 2. I-adjectives (dep=aux) - 忙しいだろう
  // 3. Nouns/pronouns (dep=cop) - あなただろう
  // 4. Adverbs (dep=aux) - 初めてだろう
  // Note: GiNZA uses dep='cop' for nominal heads, dep='aux' for others

  r.capture(darou);
});
