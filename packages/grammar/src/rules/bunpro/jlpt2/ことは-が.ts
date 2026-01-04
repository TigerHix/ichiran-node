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
 * - な-adjective + な + ことは + (same な-adjective) + だ + が/けど(も)
 * - Noun + な + ことは + (same noun) + だ + が/けど(も)
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
 * - Same lemma appears before and after ことは (with potentially different inflection)
 * - ことは is a fixed pattern: こと(NOUN) + は(PART)
 * - Ends with が/けど/けども/がも (conjunction particles)
 * - Expresses concession: "A is true, but B"
 *
 * GiNZA parse structure:
 * - Verb/Adj + こと(NOUN) + は(PART) + [same lemma Verb/Adj] + が/けど(PART)
 * - Dependencies typically show compound/fixed relations
 *
 * Different from:
 * - ことだ (simple assertion)
 * - ことにする (decide to)
 * - ことになる (it has been decided that)
 */
export default linguisticRule('ことは-が', (r) => {
  r.either(
    // Pattern 1: Verb + ことは + (same verb) + が
    // Example: 読めることは読めるが、簡単な漢字しか読めないです。
    (b1) => {
      const verb1 = b1.verb({}, 'verb1');
      const koto = b1.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b1.particle('は', 'wa');
      const verb2 = b1.verb({}, 'verb2');
      const ga = b1.particle({ textOneOf: ['が', 'けど', 'けども', 'がも'] }, 'ga');

      b1.inOrder(verb1, koto, 1);
      b1.inOrder(koto, wa, 1);
      b1.inOrder(wa, verb2, 10);
      b1.inOrder(verb2, ga, 1);

      // Same lemma (allowing different inflection)
      b1.constraint('same_lemma', verb1, verb2, (v1, v2) => v1.lemma === v2.lemma);

      b1.captureSpan('ことは-が', verb1, ga);
    },

    // Pattern 2: い-adjective + ことは + (same adj) + が
    // Example: 新しい家は広いことは広いけど、家具が多いから狭く見える。
    (b2) => {
      const adj1 = b2.adj({ pos: 'ADJ' }, 'adj1');
      const koto = b2.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b2.particle('は', 'wa');
      const adj2 = b2.adj({ pos: 'ADJ' }, 'adj2');
      const ga = b2.particle({ textOneOf: ['が', 'けど', 'けども', 'がも'] }, 'ga');

      b2.inOrder(adj1, koto, 1);
      b2.inOrder(koto, wa, 1);
      b2.inOrder(wa, adj2, 10);
      b2.inOrder(adj2, ga, 1);

      // Same lemma (allowing different inflection)
      b2.constraint('same_lemma', adj1, adj2, (a1, a2) => a1.lemma === a2.lemma);

      b2.captureSpan('ことは-が', adj1, ga);
    },

    // Pattern 3: な-adjective + な + ことは + (same adj) + だ + が
    // Example: このスマホは便利であることは便利であるけど、...
    (b3) => {
      const adj1 = b3.adj({ pos: 'ADJ' }, 'adj1');
      const na = b3.particle('な', 'na');
      const koto = b3.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b3.particle('は', 'wa');
      const adj2 = b3.adj({ pos: 'ADJ' }, 'adj2');
      const da = b3.aux({ textOneOf: ['だ', 'である'] }, 'da');
      const ga = b3.particle({ textOneOf: ['が', 'けど', 'けども', 'がも'] }, 'ga');

      b3.inOrder(adj1, na, 1);
      b3.inOrder(na, koto, 1);
      b3.inOrder(koto, wa, 1);
      b3.inOrder(wa, adj2, 10);
      b3.inOrder(adj2, da, 1);
      b3.inOrder(da, ga, 1);

      // Same lemma
      b3.constraint('same_lemma', adj1, adj2, (a1, a2) => a1.lemma === a2.lemma);

      b3.captureSpan('ことは-が', adj1, ga);
    },

    // Pattern 4: な-adjective + だ + ことは + (same adj) + だ + が
    // Example: （less common, but である form)
    (b4) => {
      const adj1 = b4.adj({ pos: 'ADJ' }, 'adj1');
      const da1 = b4.aux({ textOneOf: ['だ', 'である'] }, 'da1');
      const koto = b4.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b4.particle('は', 'wa');
      const adj2 = b4.adj({ pos: 'ADJ' }, 'adj2');
      const da2 = b4.aux({ textOneOf: ['だ', 'である'] }, 'da2');
      const ga = b4.particle({ textOneOf: ['が', 'けど', 'けども', 'がも'] }, 'ga');

      b4.inOrder(adj1, da1, 1);
      b4.inOrder(da1, koto, 1);
      b4.inOrder(koto, wa, 1);
      b4.inOrder(wa, adj2, 10);
      b4.inOrder(adj2, da2, 1);
      b4.inOrder(da2, ga, 1);

      // Same lemma
      b4.constraint('same_lemma', adj1, adj2, (a1, a2) => a1.lemma === a2.lemma);

      b4.captureSpan('ことは-が', adj1, ga);
    },

    // Pattern 5: Noun + な + ことは + (same noun) + だ + が
    // Example: ここは道路なことは道路だけど、狭すぎて車が通れない。
    (b5) => {
      const noun1 = b5.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun1');
      const na = b5.particle('な', 'na');
      const koto = b5.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b5.particle('は', 'wa');
      const noun2 = b5.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun2');
      const da = b5.aux({ textOneOf: ['だ', 'である'] }, 'da');
      const ga = b5.particle({ textOneOf: ['が', 'けど', 'けども', 'がも'] }, 'ga');

      b5.inOrder(noun1, na, 1);
      b5.inOrder(na, koto, 1);
      b5.inOrder(koto, wa, 1);
      b5.inOrder(wa, noun2, 10);
      b5.inOrder(noun2, da, 1);
      b5.inOrder(da, ga, 1);

      // Same lemma
      b5.constraint('same_lemma', noun1, noun2, (n1, n2) => n1.lemma === n2.lemma);

      b5.captureSpan('ことは-が', noun1, ga);
    },

    // Pattern 6: Noun + だ + ことは + (same noun) + だ + が
    (b6) => {
      const noun1 = b6.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun1');
      const da1 = b6.aux({ textOneOf: ['だ', 'である'] }, 'da1');
      const koto = b6.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b6.particle('は', 'wa');
      const noun2 = b6.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun2');
      const da2 = b6.aux({ textOneOf: ['だ', 'である'] }, 'da2');
      const ga = b6.particle({ textOneOf: ['が', 'けど', 'けども', 'がも'] }, 'ga');

      b6.inOrder(noun1, da1, 1);
      b6.inOrder(da1, koto, 1);
      b6.inOrder(koto, wa, 1);
      b6.inOrder(wa, noun2, 10);
      b6.inOrder(noun2, da2, 1);
      b6.inOrder(da2, ga, 1);

      // Same lemma
      b6.constraint('same_lemma', noun1, noun2, (n1, n2) => n1.lemma === n2.lemma);

      b6.captureSpan('ことは-が', noun1, ga);
    },

    // Pattern 7: Verb + ことは + (same verb without aux) + が
    // For cases where second verb might have different auxiliaries
    // Example: ボルトは外れることは外れたが、...
    (b7) => {
      const verb1 = b7.verb({}, 'verb1');
      const koto = b7.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b7.particle('は', 'wa');
      const verb2 = b7.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb2');
      const ga = b7.particle({ textOneOf: ['が', 'けど', 'けども', 'がも'] }, 'ga');

      b7.inOrder(verb1, koto, 1);
      b7.inOrder(koto, wa, 1);
      b7.inOrder(wa, verb2, 10);
      b7.inOrder(verb2, ga, 1);

      // Same lemma for verb root
      b7.constraint('same_lemma', verb1, verb2, (v1, v2) => v1.lemma === v2.lemma);

      b7.captureSpan('ことは-が', verb1, ga);
    },

    // Pattern 8: Looser pattern - any word + ことは + same word + が
    // Catch-all for unexpected GiNZA parsings
    (b8) => {
      const word1 = b8.tok({}, 'word1');
      const koto = b8.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const wa = b8.particle('は', 'wa');
      const word2 = b8.tok({}, 'word2');
      const ga = b8.particle({ textOneOf: ['が', 'けど', 'けども', 'がも'] }, 'ga');

      b8.inOrder(word1, koto, 5);
      b8.inOrder(koto, wa, 1);
      b8.inOrder(wa, word2, 15);
      b8.inOrder(word2, ga, 5);

      // Same lemma
      b8.constraint('same_lemma', word1, word2, (w1, w2) => w1.lemma === w2.lemma);

      b8.captureSpan('ことは-が', word1, ga);
    }
  );
});
