import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことは-が (koto-wa-ga) - "It is true...but, although...but"
 *
 * A set expression where the same verb or adjective appears before and after ことは,
 * followed by が/けど(も). It concedes that (A) is true while contrasting it with (B).
 *
 * Structure:
 * - Verb + ことは + (same verb) + が/けど(も)
 * - い-adjective + ことは + (same い-adjective) + が/けど(も)
 * - な-adjective + な/だ + ことは + (same な-adjective) + だ + が/けど(も)
 * - Noun + な/だ + ことは + (same noun) + だ + が/けど(も)
 *
 * Examples:
 * - 漢字は読めることは読めるが、簡単な漢字しか読めないです。
 *   (It is true that I can read kanji, but I can only read easy ones.)
 * - 新しい家は広いことは広いけど、家具が多いから狭く見える。
 *   (It is true that my new house is spacious, but because there is a lot of furniture, it looks small.)
 * - 雨に濡れたことは濡れたが、大したことはなかった。
 *   (It is true that it got soaked by the rain, but it wasn't a big deal.)
 * - このスマホは便利であることは便利であるけど、本体がデカすぎて片手では操作できない。
 *   (It is true that this smartphone is useful, but because it is big, I can't use it with just one hand.)
 *
 * Key discriminators:
 * - Same word (lemma) appears before and after ことは
 * - ことは is a fixed pattern: こと(NOUN) + は(PART)
 * - Ends with が/けど/けども (conjunction particles)
 * - Expresses concession: "A is true, but B"
 *
 * GiNZA parse structure:
 * - Verb/Adj + こと(NOUN) + は(PART) + [same lemma Verb/Adj] + が/けど(PART)
 *
 * Different from:
 * - ことだ (simple assertion)
 * - ことにする (decide to)
 * - ことになる (it has been decided that)
 * - ことだから (given that)
 */
export default linguisticRule('ことは-が', (r) => {
  r.either(
    // Pattern 1: Verb + ことは + Verb + が/けど
    // Example: 読めることは読めるが、簡単な漢字しか読めないです。
    // Example: 雨に濡れたことは濡れたが、大したことはなかった。
    (b1) => {
      const verb1 = b1.verb({}, 'verb1');
      const koto = b1.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b1.particle('は', 'wa');
      const verb2 = b1.verb({}, 'verb2');
      const ga = b1.particle({ textOneOf: ['が', 'けど', 'けども'] }, 'ga');

      b1.inOrder(verb1, koto, 3);
      b1.inOrder(koto, wa, 1);
      b1.inOrder(wa, verb2, 10);
      b1.inOrder(verb2, ga, 1);

      b1.captureSpan('ことは-が', verb1, ga);
    },

    // Pattern 2: い-adjective + ことは + い-adjective + が/けど
    // Example: 新しい家は広いことは広いけど、家具が多いから狭く見える。
    // Example: 優しいことは優しいが、自分の家族に対してだけ。
    (b2) => {
      const adj1 = b2.adj({ pos: 'ADJ' }, 'adj1');
      const koto = b2.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b2.particle('は', 'wa');
      const adj2 = b2.adj({ pos: 'ADJ' }, 'adj2');
      const ga = b2.particle({ textOneOf: ['が', 'けど', 'けども'] }, 'ga');

      b2.inOrder(adj1, koto, 3);
      b2.inOrder(koto, wa, 1);
      b2.inOrder(wa, adj2, 10);
      b2.inOrder(adj2, ga, 1);

      b2.captureSpan('ことは-が', adj1, ga);
    },

    // Pattern 3: な-adjective + な + ことは + な-adjective + だ/である + が
    // Example: このスマホは便利であることは便利であるけど、...
    (b3) => {
      const adj1 = b3.adj({ pos: 'ADJ' }, 'adj1');
      const na = b3.particle('な', 'na');
      const koto = b3.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b3.particle('は', 'wa');
      const adj2 = b3.adj({ pos: 'ADJ' }, 'adj2');
      const da = b3.aux({ textOneOf: ['だ', 'である'] }, 'da');
      const ga = b3.particle({ textOneOf: ['が', 'けど', 'けども'] }, 'ga');

      b3.inOrder(adj1, na, 1);
      b3.inOrder(na, koto, 1);
      b3.inOrder(koto, wa, 1);
      b3.inOrder(wa, adj2, 10);
      b3.inOrder(adj2, da, 1);
      b3.inOrder(da, ga, 1);

      b3.captureSpan('ことは-が', adj1, ga);
    },

    // Pattern 4: Noun + な + ことは + Noun + だ + が
    // Example: ここは道路なことは道路だけど、狭すぎて車が通れない。
    (b4) => {
      const noun1 = b4.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun1');
      const na = b4.particle('な', 'na');
      const koto = b4.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b4.particle('は', 'wa');
      const noun2 = b4.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun2');
      const da = b4.aux({ textOneOf: ['だ', 'である'] }, 'da');
      const ga = b4.particle({ textOneOf: ['が', 'けど', 'けども'] }, 'ga');

      b4.inOrder(noun1, na, 1);
      b4.inOrder(na, koto, 1);
      b4.inOrder(koto, wa, 1);
      b4.inOrder(wa, noun2, 10);
      b4.inOrder(noun2, da, 1);
      b4.inOrder(da, ga, 1);

      b4.captureSpan('ことは-が', noun1, ga);
    },

    // Pattern 5: Verb + ことは + Aux (for cases like 外れることは外れたが)
    // where the second occurrence might be split into verb + aux
    (b5) => {
      const verb1 = b5.verb({}, 'verb1');
      const koto = b5.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b5.particle('は', 'wa');
      const verb2 = b5.verb({}, 'verb2');
      const aux = b5.aux({}, 'aux');
      const ga = b5.particle({ textOneOf: ['が', 'けど', 'けども'] }, 'ga');

      b5.inOrder(verb1, koto, 3);
      b5.inOrder(koto, wa, 1);
      b5.inOrder(wa, verb2, 10);
      b5.inOrder(verb2, aux, 1);
      b5.inOrder(aux, ga, 1);

      b5.captureSpan('ことは-が', verb1, ga);
    },

    // Pattern 6: Looser pattern - catch-all for variations
    (b6) => {
      const word1 = b6.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN'] }, 'word1');
      const koto = b6.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b6.particle('は', 'wa');
      const word2 = b6.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'AUX'] }, 'word2');
      const ga = b6.particle({ textOneOf: ['が', 'けど', 'けども'] }, 'ga');

      b6.inOrder(word1, koto, 5);
      b6.inOrder(koto, wa, 1);
      b6.inOrder(wa, word2, 15);
      b6.inOrder(word2, ga, 3);

      b6.captureSpan('ことは-が', word1, ga);
    }
  );
});
