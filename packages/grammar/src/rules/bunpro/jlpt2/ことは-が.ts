import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ことは-が', (r) => {
  // ことは～が (koto wa ~ ga) - "it is true that X, but Y"
  // Pattern: Word + ことは + same word (possibly conjugated) + が/けど/etc
  //
  // This construction emphasizes that while X is true, there's a contrasting
  // factor Y. The same word appears before ことは and before the conjunction.
  //
  // Examples:
  // - 読めることは読めるが、簡単な漢字しか読めない (I CAN read kanji, but only simple ones)
  // - 広いことは広いけど、家具が多いから狭く見える (It IS spacious, but looks small)
  // - 便利であることは便利であるけど、本体がデカすぎる (It IS useful, but it's too big)
  // - 道路なことは道路だけど、狭すぎて車が通れない (It IS a road, but it's too narrow)
  //
  // GiNZA parsing notes:
  // - こと is NOUN with lemma=こと
  // - は is ADP with lemma=は
  // - The conjunction (が/けど/けれど/けれども) is typically PART or SCONJ
  // - Predicate before ことは and after it share the same lemma but may differ in form

  const koto = r.tok({
    text: 'こと',
    pos: 'NOUN',
  }, 'koto');

  const wa = r.particle('は', 'wa');

  r.either(
    // Pattern 1: Verb + ことは + same verb (any conjugation) + が/けど
    // 読めることは読めるが, 分かることは分かりますが, 行ったことは行ったけど
    // 雨に濡れたことは濡れたが, 締切を延ばせたことは延ばせたが
    (b) => {
      const verb1 = b.verb({}, 'verb1');
      const verb2 = b.verb({}, 'verb2');
      const conj = b.tok({
        textOneOf: ['が', 'けど', 'けれど', 'けれども'],
      }, 'conj');

      b.inOrder(verb1, koto, 5);
      b.inOrder(koto, wa, 1);
      b.inOrder(wa, verb2, 5);
      b.inOrder(verb2, conj, 2);

      b.captureSpan('ことは-が', verb1, conj);
    },

    // Pattern 2: い-adj + ことは + same い-adj + が/けど
    // 広いことは広いけど, 優しいことは優しいが
    (b) => {
      const adj1 = b.adj({}, 'adj1');
      const adj2 = b.adj({}, 'adj2');
      const conj = b.tok({
        textOneOf: ['が', 'けど', 'けれど', 'けれども'],
      }, 'conj');

      b.inOrder(adj1, koto, 3);
      b.inOrder(koto, wa, 1);
      b.inOrder(wa, adj2, 2);
      b.inOrder(adj2, conj, 2);

      b.captureSpan('ことは-が', adj1, conj);
    },

    // Pattern 3: な-adj (〜な/〜である) + ことは + same な-adj + だ/である + が/けど
    // 便利であることは便利であるけど
    (b) => {
      const adj1 = b.adj({
        posOneOf: ['NOUN', 'ADJ'],
      }, 'adj1');
      const dearu1 = b.aux({
        lemma: 'だ',
        inflectionForm: '連体形-ダ',
      }, 'dearu1');

      const adj2 = b.adj({
        posOneOf: ['NOUN', 'ADJ'],
      }, 'adj2');
      const dearu2 = b.aux({
        lemmaOneOf: ['だ', 'である'],
      }, 'dearu2');

      const conj = b.tok({
        textOneOf: ['が', 'けど', 'けれど', 'けれども'],
      }, 'conj');

      b.inOrder(adj1, dearu1, 2);
      b.inOrder(dearu1, koto, 1);
      b.inOrder(koto, wa, 1);
      b.inOrder(wa, adj2, 10);
      b.inOrder(adj2, dearu2, 2);
      b.inOrder(dearu2, conj, 2);

      b.captureSpan('ことは-が', adj1, conj);
    },

    // Pattern 4: Noun + なことは + noun + だ + が/けど
    // 道路なことは道路だけど
    (b) => {
      const noun1 = b.noun({}, 'noun1');
      const na1 = b.tok({
        text: 'な',
      }, 'na1');

      const noun2 = b.noun({}, 'noun2');
      const da = b.aux({
        lemma: 'だ',
      }, 'da');

      const conj = b.tok({
        textOneOf: ['が', 'けど', 'けれど', 'けれども'],
      }, 'conj');

      b.inOrder(noun1, na1, 1);
      b.inOrder(na1, koto, 1);
      b.inOrder(koto, wa, 1);
      b.inOrder(wa, noun2, 10);
      b.inOrder(noun2, da, 2);
      b.inOrder(da, conj, 2);

      b.captureSpan('ことは-が', noun1, conj);
    }
  );
});
