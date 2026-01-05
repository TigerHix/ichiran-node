import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことは-が (koto wa ga) - "while it's true that X, (but)..."
 *
 * A concessive construction where the same word or phrase is repeated before
 * both "ことは" and "が/けど". It acknowledges that X is true, but contrasts
 * it with some other fact or limitation. Translates to "it is true that X, but..."
 * or "while X is true, (but)..."
 *
 * Structure:
 * - Verb + ことは + Verb + が/けど (same verb, possibly different forms)
 * - い-adj + ことは + い-adj + が/けど (same adjective, possibly different forms)
 * - な-adj + (な)ことは + な-adj + (だ)が/だけど
 * - Noun + (な)ことは + Noun + (だ)が/だけど
 *
 * Examples:
 * - 漢字は読めることは読めるが、簡単な漢字しか読めないです。
 *   (It is true that I can read kanji, but I can only read easy ones.)
 * - 新しい家は広いことは広いけど、家具が多いから狭く見える。
 *   (It is true that my new house is spacious, but because there is a lot of furniture, it looks small.)
 * - 雨に濡れたことは濡れたが、大したことはなかった。
 *   (It is true that it got soaked by the rain, but it wasn't a big deal.)
 * - ここは道路なことは道路だけど、狭すぎて車が通れない。
 *   (It is true that this is a road, but it's too narrow for cars to pass.)
 *
 * Key discriminators:
 * - こと is a noun (NOUN) meaning "thing, matter"
 * - は is a topic particle (ADP/PART)
 * - が/けど is a conjunction particle (ADP/CCONJ) meaning "but, however"
 * - Concessive meaning: acknowledges X but contrasts with Y
 * - The repetition creates emphasis on the truth of X
 *
 * GiNZA parse structure:
 * - Verb/Adj + こと(NOUN) + は(PART) ... Verb/Adj + が/けど(PART/CCONJ)
 * - For な-adj/Noun: Adj/Noun + な(PART) + こと(NOUN) + は(PART)
 * - Dependencies: compound, mark, advcl relationships
 *
 * Different from:
 * - ことは-が with different lemmas (not this grammar)
 * - が alone (simple "but" conjunction)
 * - けど alone (informal "but" conjunction)
 * - けれど/けれども (formal "but" conjunction)
 * - ことは-の (koto wa no - topic + nominalization)
 */
export default bunproLinguisticRule('ことは-が', (r) => {
  // The pattern is: X + ことは + X + が/けど
  // where X is the same verb/adj/noun (possibly different inflection forms)
  //
  // Important notes:
  // - The two X's can be in different forms (e.g., 外れる vs 外れた, 分かる vs 分かります)
  // - GiNZA may parse verbs in 連用形 as ADJ
  // - We use looser matching on POS to handle GiNZA's variations

  r.either(
    // Pattern 1: Verb + ことは + Verb + が/けど
    // Example: 漢字は読めることは読めるが、簡単な漢字しか読めないです。
    // Note: The two verbs can be in different forms (e.g., 外れる vs 外れた)
    (b1) => {
      const tok1 = b1.tok({ posOneOf: ['VERB', 'ADJ'] }, 'tok1');
      const koto1 = b1.noun({ text: 'こと', lemma: 'こと' }, 'koto1');
      const wa = b1.particle('は', 'wa');
      const tok2 = b1.tok({ posOneOf: ['VERB', 'ADJ'] }, 'tok2');
      const ga = b1.tok({
        textOneOf: ['が', 'けど', 'けども', 'けれど', 'けれども'],
        posOneOf: ['PART', 'CCONJ', 'SCONJ'],
      }, 'ga');

      b1.inOrder(tok1, koto1, 1);
      b1.inOrder(koto1, wa, 1);
      b1.inOrder(wa, tok2, 10);  // Allow more distance for varied sentence structures
      b1.inOrder(tok2, ga, 1);

      b1.captureSpan('ことは-が', tok1, ga);
    },

    // Pattern 2: Adj + ことは + Adj + が/けど
    // Example: 新しい家は広いことは広いけど、家具が多いから狭く見える。
    (b2) => {
      const adj1 = b2.adj({}, 'adj1');
      const koto2 = b2.noun({ text: 'こと', lemma: 'こと' }, 'koto2');
      const wa2 = b2.particle('は', 'wa2');
      const adj2 = b2.tok({ posOneOf: ['ADJ', 'VERB'] }, 'adj2');  // Also VERB for GiNZA variations
      const ga2 = b2.tok({
        textOneOf: ['が', 'けど', 'けども', 'けれど', 'けれども'],
        posOneOf: ['PART', 'CCONJ', 'SCONJ'],
      }, 'ga2');

      b2.inOrder(adj1, koto2, 1);
      b2.inOrder(koto2, wa2, 1);
      b2.inOrder(wa2, adj2, 10);
      b2.inOrder(adj2, ga2, 1);

      b2.captureSpan('ことは-が', adj1, ga2);
    },

    // Pattern 3: な-adjective + な + ことは + な-adjective + だ/である + が/けど
    // Example: このスマホは便利であることは便利であるけど...
    (b3) => {
      const adj1 = b3.adj({}, 'adj1');
      const na1 = b3.tok({ textOneOf: ['な', 'である'] }, 'na1');
      const koto3 = b3.noun({ text: 'こと', lemma: 'こと' }, 'koto3');
      const wa3 = b3.particle('は', 'wa3');
      const adj2 = b3.adj({}, 'adj2');
      const da = b3.tok({ textOneOf: ['だ', 'である'] }, 'da');
      const ga3 = b3.tok({
        textOneOf: ['が', 'けど', 'けども', 'けれど', 'けれども'],
        posOneOf: ['PART', 'CCONJ', 'SCONJ'],
      }, 'ga3');

      b3.inOrder(adj1, na1, 1);
      b3.inOrder(na1, koto3, 1);
      b3.inOrder(koto3, wa3, 1);
      b3.inOrder(wa3, adj2, 10);
      b3.inOrder(adj2, da, 1);
      b3.inOrder(da, ga3, 1);

      b3.captureSpan('ことは-が', adj1, ga3);
    },

    // Pattern 4: Noun + な + ことは + Noun + だ/である + が/けど
    // Example: ここは道路なことは道路だけど、狭すぎて車が通れない。
    (b4) => {
      const noun1 = b4.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun1');
      const na2 = b4.particle('な', 'na2');
      const koto4 = b4.noun({ text: 'こと', lemma: 'こと' }, 'koto4');
      const wa4 = b4.particle('は', 'wa4');
      const noun2 = b4.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun2');
      const da2 = b4.tok({ textOneOf: ['だ', 'である'] }, 'da2');
      const ga4 = b4.tok({
        textOneOf: ['が', 'けど', 'けども', 'けれど', 'けれども'],
        posOneOf: ['PART', 'CCONJ', 'SCONJ'],
      }, 'ga4');

      b4.inOrder(noun1, na2, 1);
      b4.inOrder(na2, koto4, 1);
      b4.inOrder(koto4, wa4, 1);
      b4.inOrder(wa4, noun2, 10);
      b4.inOrder(noun2, da2, 1);
      b4.inOrder(da2, ga4, 1);

      b4.captureSpan('ことは-が', noun1, ga4);
    },

    // Pattern 5: Verb (any token) + ことは + Verb (any token) + が/けど
    // This is a catch-all pattern for various verb forms
    // Example: 雨に濡れたことは濡れたが、大したことはなかった。
    (b5) => {
      const tok1 = b5.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'tok1');
      const koto5 = b5.noun({ text: 'こと', lemma: 'こと' }, 'koto5');
      const wa5 = b5.particle('は', 'wa5');
      const tok2 = b5.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'tok2');
      const ga5 = b5.tok({
        textOneOf: ['が', 'けど', 'けども', 'けれど', 'けれども'],
        posOneOf: ['PART', 'CCONJ', 'SCONJ'],
      }, 'ga5');

      b5.inOrder(tok1, koto5, 1);
      b5.inOrder(koto5, wa5, 1);
      b5.inOrder(wa5, tok2, 10);
      b5.inOrder(tok2, ga5, 1);

      b5.captureSpan('ことは-が', tok1, ga5);
    },

    // Pattern 6: Looser pattern - any POS before ことは, any POS after
    // For sentences with unexpected GiNZA parses
    (b6) => {
      const tok1 = b6.tok({}, 'tok1');
      const koto6 = b6.noun({ text: 'こと', lemma: 'こと' }, 'koto6');
      const wa6 = b6.particle('は', 'wa6');
      const tok2 = b6.tok({}, 'tok2');
      const ga6 = b6.tok({
        textOneOf: ['が', 'けど', 'けども', 'けれど', 'けれども'],
        posOneOf: ['PART', 'CCONJ', 'SCONJ'],
      }, 'ga6');

      b6.inOrder(tok1, koto6, 1);
      b6.inOrder(koto6, wa6, 1);
      b6.inOrder(wa6, tok2, 10);
      b6.inOrder(tok2, ga6, 1);

      b6.captureSpan('ことは-が', tok1, ga6);
    }
  );
});
