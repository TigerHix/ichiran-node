import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('adjective-て-b', (r) => {
  r.either(
    // Pattern 1: i-adjective + くて (e.g., 大きくて, 明るくて, 悲しくて)
    // Note: GiNZA parses some i-adjectives in conjunctive form as VERB, not ADJ
    (r1) => {
      // Match i-adjective in conjunctive form
      // Use conjugationClass to distinguish: adjectives have class=形容詞
      const iAdj = r1.tok(
        {
          conjugationClass: '形容詞',
        },
        'iAdj'
      );

      const te = r1.tok(
        {
          text: 'て',
          pos: 'SCONJ',
        },
        'te'
      );

      r1.headChild(iAdj, te, 'mark');
      r1.inOrder(iAdj, te, 1);

      r1.captureSpan('て-form', iAdj, te);
    },
    // Pattern 2a: na-adjective + で with AUX (e.g., 綺麗で, シンプルで)
    // GiNZA correctly parses で as AUX with dep=aux for some na-adjectives
    (r2) => {
      const naAdj = r2.tok(
        {
          pos: 'ADJ',
          depOneOf: ['advcl', 'acl'],
        },
        'naAdj'
      );

      const de = r2.tok(
        {
          text: 'で',
          pos: 'AUX',
          dep: 'aux',
        },
        'de'
      );

      r2.headChild(naAdj, de);
      r2.inOrder(naAdj, de, 1);

      r2.captureSpan('で-form', naAdj, de);
    },
    // Pattern 2b: na-adjective + で with ADP (e.g., しずかで, しんせつで)
    // GiNZA inconsistently parses で as ADP/case for hiragana na-adjectives
    // Discriminator: preceding ADJ has dep=advcl/obl AND で has lemma=だ
    (r3) => {
      const naAdj = r3.tok(
        {
          pos: 'ADJ',
          depOneOf: ['advcl', 'obl'],
        },
        'naAdj'
      );

      const de = r3.tok(
        {
          text: 'で',
          pos: 'ADP',
          lemma: 'だ', // Discriminator: copula で has lemma=だ, locative で has lemma=で
        },
        'de'
      );

      r3.headChild(naAdj, de, 'case');
      r3.inOrder(naAdj, de, 1);

      r3.captureSpan('で-form', naAdj, de);
    },
    // Pattern 3: Noun + で conjunction (e.g., 漫画家で, 映画監督で)
    // When GiNZA parses で with lemma=だ (copula te-form), it's a conjunction
    // Note: GiNZA is inconsistent - sometimes lemma=だ, sometimes lemma=で for same pattern
    (r4) => {
      const noun = r4.tok(
        {
          posOneOf: ['NOUN', 'PROPN'],
        },
        'noun'
      );

      const de = r4.tok(
        {
          text: 'で',
          pos: 'ADP',
          lemma: 'だ', // Key discriminator: copula で (conjunction) vs locative で
        },
        'de'
      );

      r4.headChild(noun, de, 'case');
      r4.inOrder(noun, de, 1);

      r4.captureSpan('で-form', noun, de);
    }
  );
});
