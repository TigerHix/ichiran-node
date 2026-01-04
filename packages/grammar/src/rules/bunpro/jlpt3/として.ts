import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('として', (r) => {
  // として (toshite) - "as" / "in the role of"
  // Pattern: Noun + として (Noun + として + Noun also valid)
  //
  // と (quotational particle) + して (te-form of する)
  // Functions as an adverbial particle indicating capacity/role
  //
  // GiNZA parsing notes:
  // - と is ADP with lemma=と and dep=case (quotational)
  // - して can be:
  //   1. Single token with lemma=する (pos can be VERB or AUX)
  //   2. Split into し (pos=VERB or AUX, lemma=する) + て (pos=SCONJ or AUX, lemma=て)
  // - The noun before と has various dependencies (nmod, obl, etc.)
  //
  // Examples:
  // - 先生としての彼 (him as a teacher)
  // - 友達としては最高だ (as a friend, [she's] the best)
  // - 会社としての目標 (goals as a company)
  // - DVDプレイヤーとしても使えます (can also use as a DVD player)

  const to = r.particle('と', 'to');

  r.either(
    // Pattern 1: して as single token (VERB or AUX)
    // 最も難しいところとして (parsed as single token)
    (b) => {
      const shite = b.tok({
        text: 'して',
        lemma: 'する',
        posOneOf: ['VERB', 'AUX'],
      }, 'shite');
      b.inOrder(to, shite, 1);
      b.captureSpan('として', to, shite);
    },

    // Pattern 2: して split into し + て (most common)
    // 友達として, 先生として, 会社として
    // GiNZA inconsistency: し can be VERB or AUX
    (b) => {
      const shi = b.tok({
        text: 'し',
        lemma: 'する',
        posOneOf: ['VERB', 'AUX'],
      }, 'shi');
      const te = b.tok({
        text: 'て',
        lemma: 'て',
        posOneOf: ['SCONJ', 'AUX'],
      }, 'te');
      b.inOrder(to, shi, 1);
      b.inOrder(shi, te, 1);
      b.captureSpan('として', to, te);
    }
  );
});
