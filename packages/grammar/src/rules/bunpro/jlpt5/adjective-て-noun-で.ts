import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('adjective-て-noun-で', (r) => {
  // Adjective + て・Noun + で - "And..." (Conjunctive)
  //
  // Matches the conjunctive form used to connect adjectives and nouns,
  // similar to "and" in English.
  //
  // Three patterns:
  // 1. い-adjectives: remove い, add くて (e.g., 大きい → 大きくて)
  //    - Exception: いい → よくて (not いくて)
  // 2. な-adjectives: add で (e.g., 静か → 静かで)
  // 3. Nouns: add で (e.g., 仕事 → 仕事で)
  //
  // Note: で for な-adjectives and nouns is a form of the copula だ,
  // while くて for い-adjectives uses the conjunction particle て.

  r.either(
    // Pattern 1: い-adjectives in くて form
    // e.g., 新しくて, 大きくて, 寒くて, よくて (exception for いい)
    // GiNZA parses these as i-adjective (conjugationClass=形容詞) + て (SCONJ)
    (branch1) => {
      const iAdj = branch1.tok({
        conjugationClass: '形容詞',
      }, 'iAdj');

      const te = branch1.tok({
        text: 'て',
        pos: 'SCONJ',
      }, 'te');

      branch1.headChild(iAdj, te, 'mark');
      branch1.inOrder(iAdj, te, 1);
      branch1.captureSpan('adjective-て-noun-で', iAdj, te);
    },

    // Pattern 2a: な-adjectives + で (AUX form)
    // e.g., 綺麗で, きれいで, 大きらいで
    // GiNZA correctly parses で as AUX with dep=aux for some na-adjectives
    (branch2) => {
      const naAdj = branch2.tok({
        pos: 'ADJ',
        depOneOf: ['advcl', 'acl'],
      }, 'naAdj');

      const de = branch2.aux({
        text: 'で',
        dep: 'aux',
      }, 'de');

      branch2.headChild(naAdj, de);
      branch2.inOrder(naAdj, de, 1);
      branch2.captureSpan('adjective-て-noun-で', naAdj, de);
    },

    // Pattern 2b: な-adjectives + で (ADP form)
    // e.g., 静かで, 嫌いで, 好きで
    // GiNZA inconsistently parses で as ADP/case for hiragana na-adjectives
    // Discriminator: preceding ADJ has dep=advcl/obl AND で has lemma=だ
    (branch3) => {
      const naAdj = branch3.tok({
        pos: 'ADJ',
        depOneOf: ['advcl', 'obl'],
      }, 'naAdj');

      const de = branch3.tok({
        text: 'で',
        pos: 'ADP',
        lemma: 'だ', // Discriminator: copula で has lemma=だ
      }, 'de');

      branch3.headChild(naAdj, de, 'case');
      branch3.inOrder(naAdj, de, 1);
      branch3.captureSpan('adjective-て-noun-で', naAdj, de);
    },

    // Pattern 3: Nouns + で (conjunction)
    // e.g., 仕事で, 暇で, 病気で
    // When GiNZA parses で with lemma=だ (copula te-form), it's a conjunction
    (branch4) => {
      const noun = branch4.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');

      const de = branch4.tok({
        text: 'で',
        pos: 'ADP',
        lemma: 'だ', // Key discriminator: copula で vs locative で
      }, 'de');

      branch4.headChild(noun, de, 'case');
      branch4.inOrder(noun, de, 1);
      branch4.captureSpan('adjective-て-noun-で', noun, de);
    }
  );
});
