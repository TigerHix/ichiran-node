import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('な-adjective-noun', (r) => {
  // Match na-adjective + な + noun
  // na-adjectives require な when modifying a noun
  // e.g., "きれいな人" (clean person), "静かな部屋" (quiet room)
  //
  // Examples from Bunpro:
  // - 綺麗なカーテンです (beautiful curtain)
  // - 静かな街です (quiet town)
  // - 親切な人だ (kind person)
  // - 便利なツールです (convenient tool)
  // - 好きな色 (favorite color)
  //
  // Grammar structure:
  // - Adjective (ADJ/NOUN) with dep=acl/advcl (attributive use)
  // - Followed by auxiliary な (lemma=だ, pos=AUX, inflectionForm=連体形-一般)
  // - Followed by noun (NOUN/PROPN) that is being modified
  //
  // The key discriminator from prohibitive な is:
  // - Prohibitive な: pos=PART, dep=mark (sentence-ending particle)
  // - Adjective な: pos=AUX, lemma=だ, inflectionForm=連体形-一般 (copula form)
  //
  // GiNZA inconsistencies:
  // - Some na-adjectives are parsed as NOUN instead of ADJ (e.g., かんたん)
  // - Some na-adjectives use dep=advcl instead of acl (e.g., しずか in compound)
  // - Some nouns are PROPN instead of NOUN (e.g., ベン)

  r.either(
    // Branch 1: Standard case - ADJ with dep=acl
    (branch) => {
      const adj = branch.tok({
        pos: 'ADJ',
        dep: 'acl',
      }, 'adj');
      const na = branch.aux({
        text: 'な',
        lemma: 'だ',
        inflectionForm: '連体形-一般',
      }, 'na');
      const noun = branch.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');

      branch.inOrder(adj, na, 1);
      branch.inOrder(na, noun, 1);
      branch.captureSpan('な-adjective-noun', adj, noun);
    },
    // Branch 2: NOUN parsed as ADJ (e.g., かんたん)
    (branch) => {
      const adj = branch.tok({
        pos: 'NOUN',
        dep: 'acl',
      }, 'adj');
      const na = branch.aux({
        text: 'な',
        lemma: 'だ',
        inflectionForm: '連体形-一般',
      }, 'na');
      const noun = branch.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');

      branch.inOrder(adj, na, 1);
      branch.inOrder(na, noun, 1);
      branch.captureSpan('な-adjective-noun', adj, noun);
    },
    // Branch 3: ADJ with dep=advcl (compound modifier)
    (branch) => {
      const adj = branch.tok({
        pos: 'ADJ',
        dep: 'advcl',
      }, 'adj');
      const na = branch.aux({
        text: 'な',
        lemma: 'だ',
        inflectionForm: '連体形-一般',
      }, 'na');
      const noun = branch.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');

      branch.inOrder(adj, na, 1);
      branch.inOrder(na, noun, 1);
      branch.captureSpan('な-adjective-noun', adj, noun);
    }
  );
});
