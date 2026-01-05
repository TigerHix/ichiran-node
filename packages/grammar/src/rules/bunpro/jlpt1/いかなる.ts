import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('いかなる', (r) => {
  // いかなる (ikanaru) - formal/literary "what kind of", "any", "no matter what"
  // Pre-nominal adjectival (連体詞 - pre-noun adjective)
  // Formal version of どんな (casual "what kind of")
  //
  // Structure: いかなる + Noun
  // Examples:
  // - いかなる時 (no matter what time/at any time)
  // - いかなる状況 (no matter what situation)
  // - いかなること (no matter what thing)
  //
  // GiNZA parses いかなる as:
  // - pos=ADJ (adjective/adjectival)
  // - dep=amod or acl (adjectival modifier)
  // - lemma=いかなる (or 如何なる in kanji form)
  //
  // Key discriminators from どんな:
  // - どんな: pos=PRON, dep=nmod
  // - いかなる: pos=ADJ, dep=amod/acl

  const ikanaru = r.adj({
    lemmaOneOf: ['いかなる', '如何なる'],
    depOneOf: ['amod', 'acl'],
  }, 'ikanaru');

  // いかなる must be followed by a noun (within 5 tokens to allow adjectives)
  const noun = r.tok({
    posOneOf: ['NOUN', 'PROPN', 'PRON'],
  }, 'noun');

  r.inOrder(ikanaru, noun, 5);
  r.capture(ikanaru);
});
