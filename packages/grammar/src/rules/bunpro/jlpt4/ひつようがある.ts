import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ひつようがある (need to/it is necessary to)
 *
 * Pattern: Verb (dictionary form) + 必要 + が + ある/ある/ない/ありません
 *
 * Expresses necessity or requirement to do something. Literally "there is a need to do X".
 *
 * This attaches to verbs in dictionary form (辞書形), not nouns. For nouns, use が必要.
 *
 * Structures:
 * - Casual positive: Verb + 必要がある (need to do)
 * - Polite positive: Verb + 必要があります (need to do)
 * - Casual negative: Verb + 必要がない (don't need to do)
 * - Polite negative: Verb + 必要がありません (don't need to do)
 *
 * Examples:
 * - 勉強する必要がある (need to study)
 * - 行く必要があります (need to go)
 * - 買う必要がない (don't need to buy)
 * - 翻訳する必要がありません (don't need to translate)
 *
 * Note: 必要 can be written in kanji (必要) or hiragana (ひつよう).
 * The test data uses both forms, so we match both.
 *
 * Related grammar points:
 * - が必要 (nouns) vs 必要がある (verbs)
 * - ことはない (there's no need to - JLPT3)
 */
export default bunproLinguisticRule('ひつようがある', (r) => {
  // Match any verb in dictionary form
  const verb = r.verb({}, 'verb');

  // Match both kanji (必要) and hiragana (ひつよう) forms
  // GiNZA parses these inconsistently: sometimes NOUN, sometimes ADJ, sometimes VERB
  const hitsuyou = r.tok({
    posOneOf: ['NOUN', 'ADJ', 'VERB'],
    textOneOf: ['必要', 'ひつよう']
  }, 'hitsuyou');

  const ga = r.particle('が', 'ga');

  r.either(
    // Branch 1: Positive casual - Verb + 必要がある
    (b1) => {
      const aru = b1.tok({ lemma: 'ある' }, 'aru');
      b1.inOrder(verb, hitsuyou);
      b1.inOrder(hitsuyou, ga, 1);
      b1.inOrder(ga, aru, 1);
      b1.captureSpan('必要がある', verb, aru);
    },
    // Branch 2: Positive polite - Verb + 必要があります
    (b2) => {
      const arimasu = b2.tok({ lemma: 'ある', text: 'あります' }, 'arimasu');
      b2.inOrder(verb, hitsuyou);
      b2.inOrder(hitsuyou, ga, 1);
      b2.inOrder(ga, arimasu, 1);
      b2.captureSpan('必要があります', verb, arimasu);
    },
    // Branch 3: Negative casual - Verb + 必要がない
    (b3) => {
      const nai = b3.tok({ lemma: 'ない' }, 'nai');
      b3.inOrder(verb, hitsuyou);
      b3.inOrder(hitsuyou, ga, 1);
      b3.inOrder(ga, nai, 1);
      b3.captureSpan('必要がない', verb, nai);
    },
    // Branch 4: Negative polite - Verb + 必要がありません
    (b4) => {
      const arimasen = b4.tok({ lemma: 'ある', text: 'ありません' }, 'arimasen');
      b4.inOrder(verb, hitsuyou);
      b4.inOrder(hitsuyou, ga, 1);
      b4.inOrder(ga, arimasen, 1);
      b4.captureSpan('必要がありません', verb, arimasen);
    }
  );
});
