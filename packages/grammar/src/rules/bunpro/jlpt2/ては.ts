import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ては', (r) => {
  r.either(
    // Pattern 1: Verb-te form + は (e.g., 入れては, 回しては, 解放しては, していては)
    // Conditional: the verb must have dep=advcl (adverbial clause)
    (b1) => {
      const verb = b1.tok({
        posOneOf: ['VERB', 'AUX'],  // Include AUX for verbs like し (する)
        // Note: inflectionForm may not be set for verb stems like 解放
        dep: 'advcl',  // Conditional clause, not sequential action
      }, 'verb');
      const te = b1.tok({ text: 'て', pos: 'SCONJ' }, 'te');
      const wa = b1.tok({ text: 'は', pos: 'ADP', dep: 'case' }, 'wa');

      b1.inOrder(verb, te, 10);  // Allow more distance for て-form + aux + ては
      b1.inOrder(te, wa, 10);  // Allow more distance for いては patterns

      // The は should point to the verb (forming the conditional clause)
      b1.headChild(verb, wa, 'case');

      b1.captureSpan('ては', verb, wa);
    },

    // Pattern 2: Verb-de + は (e.g., 飲んでは - some verbs use で instead of て)
    // Conditional: the verb must have dep=advcl (adverbial clause)
    (b2) => {
      const verb = b2.verb({
        // Note: inflectionForm may not be set for verb stems
        dep: 'advcl',
      }, 'verb');
      const de = b2.tok({ text: 'で', posOneOf: ['SCONJ', 'ADP'] }, 'de');
      const wa = b2.tok({ text: 'は', pos: 'ADP', dep: 'case' }, 'wa');

      b2.inOrder(verb, de, 10);
      b2.inOrder(de, wa, 10);

      // The は should point to the verb (forming the conditional clause)
      b2.headChild(verb, wa, 'case');

      b2.captureSpan('ては', verb, wa);
    },

    // Pattern 3: I-adj-te form + は (e.g., 長くては, 塩辛くては, 高くては, 多くては)
    // Note: Some i-adjs are parsed as VERB (e.g., 塩辛く)
    // Conditional: the adj must have dep=advcl (adverbial clause)
    // Also accept adj at end of sentence (dep=root) for sentence-final conditionals
    (b3) => {
      const adj = b3.tok({
        posOneOf: ['ADJ', 'VERB'],  // Some i-adj te-forms are parsed as VERB
        // Note: inflectionForm may not be set for some adjs
        depOneOf: ['advcl', 'root'],  // Can be advcl or root (sentence-final)
      }, 'adj');
      const te = b3.tok({ text: 'て', pos: 'SCONJ' }, 'te');
      const wa = b3.tok({ text: 'は', pos: 'ADP', dep: 'case' }, 'wa');

      b3.inOrder(adj, te, 10);
      b3.inOrder(te, wa, 10);

      // The は should point to the adj (forming the conditional clause)
      b3.headChild(adj, wa, 'case');

      b3.captureSpan('ては', adj, wa);
    },

    // Pattern 4: Noun + では where で is ADP/AUX (e.g., 几帳面では, 給与では, 結果次第では)
    // This creates a conditional from a noun phrase
    // This pattern only matches when で has pos=ADP/AUX (not SCONJ)
    // SCONJ + で is handled by pattern 2 (verb-de)
    (b4) => {
      const noun = b4.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      const de = b4.tok({ text: 'で', posOneOf: ['ADP', 'AUX'], dep: 'case' }, 'de');
      const wa = b4.tok({ text: 'は', pos: 'ADP', dep: 'case' }, 'wa');

      b4.inOrder(noun, de, 3);
      b4.inOrder(de, wa, 2);

      // Both で and は should point to the noun
      b4.headChild(noun, de, 'case');
      b4.headChild(noun, wa, 'case');

      b4.captureSpan('ては', noun, wa);
    },

    // Pattern 5: Noun + じゃ (e.g., あの先生じゃ)
    // Contraction of では, with dep=cop
    (b5) => {
      const noun = b5.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      const ja = b5.tok({ text: 'じゃ', pos: 'AUX', dep: 'cop' }, 'ja');

      b5.inOrder(noun, ja, 3);
      b5.headChild(noun, ja, 'cop');

      b5.captureSpan('ては', noun, ja);
    },

    // Pattern 6: Single token ては (e.g., 多くては when tokenized together)
    // GiNZA sometimes tokenizes ては as a single SCONJ token
    (b6) => {
      const tewa = b6.tok({ text: 'ては', pos: 'SCONJ' }, 'tewa');
      b6.captureSpan('ては', tewa, tewa);
    },

    // Pattern 7: Verb/adj + ちゃ (contraction of ては, e.g., 食べなくちゃ, 塩辛くちゃ)
    // This is the casual contracted form
    (b7) => {
      const word = b7.tok({
        posOneOf: ['VERB', 'ADJ', 'AUX'],
        inflectionForm: /連用形/,
      }, 'word');
      const cha = b7.tok({
        text: 'ちゃ',
        posOneOf: ['AUX', 'PART'],
      }, 'cha');

      b7.inOrder(word, cha, 5);
      b7.captureSpan('ては', word, cha);
    },
  );
});
