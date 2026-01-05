import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: てしょうがない (te shouganai) - Can't help but..., unbearable
 *
 * Expresses that something is so extreme that "it can't be helped" or "there's no way
 * to avoid it." Used to emphasize uncontrollable feelings, desires, or states.
 *
 * Structure:
 * - Verb[て-form] + しょうがない / 仕方がない
 * - I-adjective[て-form] + しょうがない / 仕方がない
 * - Na-adjective + で + しょうがない / 仕方がない
 *
 * Examples:
 * - 日本語が上手になりたくてしょうがない。
 *   (I can't help but want to become good at Japanese.)
 * - 部屋が暑くてしょうがない。
 *   (My room is extremely hot.)
 * - スイーツが好きでしょうがない。
 *   (I love sweets so much that it can't be helped.)
 *
 * Key discriminators:
 * - Follows te-form of verbs/adjectives or de-form of na-adjectives
 * - しょう is a contraction of 仕方 (shikata - method/way)
 * - Both しょうがない and 仕方がない are used (former is more colloquial)
 * - Literally means "there's no way/method to deal with (X)"
 *
 * Different from:
 * - てたまらない (unbearable/can't stand - focuses on degree)
 * - てならない (can't help but - more formal/written)
 * - ざるを得ない (have no choice but - external compulsion)
 *
 * GiNZA parse structure:
 * - Verb-te: verb[連用形] + て(SCONJ)
 * - Adj-te: adj[連用形] + て(SCONJ)
 * - Na-adj-de: adj + で(AUX,lemma=だ) or で(SCONJ)
 * - しょうがない: can be split or combined tokens
 * - 仕方がない: can be split or combined tokens
 */
export default linguisticRule('てしょうがない', (r) => {
  r.either(
    // Pattern 1: Verb[て-form] + しょうがない (split tokens)
    // e.g., なりたくてしょうがない, 食べたくて仕方がない
    (b1) => {
      const verb = b1.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const te = b1.tok({ textOneOf: ['て', 'で'] }, 'te');
      const shou = b1.tok({ textOneOf: ['しょう', '仕方'] }, 'shou');
      const ga = b1.tok({ text: 'が' }, 'ga');
      const nai = b1.tok({ textOneOf: ['ない', 'ありません', 'ないです'] }, 'nai');

      b1.inOrder(verb, te, 3);
      b1.inOrder(te, shou, 5);
      b1.inOrder(shou, ga, 1);
      b1.inOrder(ga, nai, 1);

      b1.captureSpan('てしょうがない', verb, nai);
    },

    // Pattern 1b: Verb with inflection form + しょうがない
    (b1b) => {
      const verb = b1b.verb({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
        ],
      }, 'verb');
      const te = b1b.tok({ textOneOf: ['て', 'で'] }, 'te');
      const shou = b1b.tok({ textOneOf: ['しょう', '仕方'] }, 'shou');
      const ga = b1b.tok({ text: 'が' }, 'ga');
      const nai = b1b.tok({ textOneOf: ['ない', 'ありません', 'ないです'] }, 'nai');

      b1b.inOrder(verb, te, 2);
      b1b.inOrder(te, shou, 5);
      b1b.inOrder(shou, ga, 1);
      b1b.inOrder(ga, nai, 1);

      b1b.captureSpan('てしょうがない', verb, nai);
    },

    // Pattern 2: Verb[て-form] + 仕方がない (full kanji form)
    // e.g., 食べたくて仕方がない, 忘れて仕方がない
    (b2) => {
      const verb = b2.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const te = b2.tok({ textOneOf: ['て', 'で'] }, 'te');
      const shikata = b2.tok({ text: '仕方' }, 'shikata');
      const ga = b2.tok({ text: 'が' }, 'ga');
      const nai = b2.tok({ textOneOf: ['ない', 'なかった', 'ありません', 'ないです'] }, 'nai');

      b2.inOrder(verb, te, 3);
      b2.inOrder(te, shikata, 5);
      b2.inOrder(shikata, ga, 1);
      b2.inOrder(ga, nai, 1);

      b2.captureSpan('てしょうがない', verb, nai);
    },

    // Pattern 2b: Verb with inflection form + 仕方がない
    (b2b) => {
      const verb = b2b.verb({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
        ],
      }, 'verb');
      const te = b2b.tok({ textOneOf: ['て', 'で'] }, 'te');
      const shikata = b2b.tok({ text: '仕方' }, 'shikata');
      const ga = b2b.tok({ text: 'が' }, 'ga');
      const nai = b2b.tok({ textOneOf: ['ない', 'なかった', 'ありません', 'ないです'] }, 'nai');

      b2b.inOrder(verb, te, 2);
      b2b.inOrder(te, shikata, 5);
      b2b.inOrder(shikata, ga, 1);
      b2b.inOrder(ga, nai, 1);

      b2b.captureSpan('てしょうがない', verb, nai);
    },

    // Pattern 3: I-adjective[て-form] + しょうがない
    // e.g., 暑くてしょうがない, 楽しくてしょうがない
    (b3) => {
      const adj = b3.tok({ pos: 'ADJ' }, 'adj');
      const te = b3.tok({ text: 'て' }, 'te');
      const shou = b3.tok({ textOneOf: ['しょう', '仕方'] }, 'shou');
      const ga = b3.tok({ text: 'が' }, 'ga');
      const nai = b3.tok({ textOneOf: ['ない', 'ありません', 'ないです'] }, 'nai');

      b3.inOrder(adj, te, 3);
      b3.inOrder(te, shou, 5);
      b3.inOrder(shou, ga, 1);
      b3.inOrder(ga, nai, 1);

      b3.captureSpan('てしょうがない', adj, nai);
    },

    // Pattern 3b: I-adjective with inflection form + しょうがない
    (b3b) => {
      const adj = b3b.adj({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
        ],
      }, 'adj');
      const te = b3b.tok({ text: 'て' }, 'te');
      const shou = b3b.tok({ textOneOf: ['しょう', '仕方'] }, 'shou');
      const ga = b3b.tok({ text: 'が' }, 'ga');
      const nai = b3b.tok({ textOneOf: ['ない', 'ありません', 'ないです'] }, 'nai');

      b3b.inOrder(adj, te, 2);
      b3b.inOrder(te, shou, 5);
      b3b.inOrder(shou, ga, 1);
      b3b.inOrder(ga, nai, 1);

      b3b.captureSpan('てしょうがない', adj, nai);
    },

    // Pattern 4: I-adjective[て-form] + 仕方がない
    // e.g., 暑くて仕方がない, 眠くて仕方がなかった
    (b4) => {
      const adj = b4.tok({ pos: 'ADJ' }, 'adj');
      const te = b4.tok({ text: 'て' }, 'te');
      const shikata = b4.tok({ text: '仕方' }, 'shikata');
      const ga = b4.tok({ text: 'が' }, 'ga');
      const nai = b4.tok({ textOneOf: ['ない', 'なかった', 'ありません', 'ないです'] }, 'nai');

      b4.inOrder(adj, te, 3);
      b4.inOrder(te, shikata, 5);
      b4.inOrder(shikata, ga, 1);
      b4.inOrder(ga, nai, 1);

      b4.captureSpan('てしょうがない', adj, nai);
    },

    // Pattern 4b: I-adjective with inflection form + 仕方がない
    (b4b) => {
      const adj = b4b.adj({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
        ],
      }, 'adj');
      const te = b4b.tok({ text: 'て' }, 'te');
      const shikata = b4b.tok({ text: '仕方' }, 'shikata');
      const ga = b4b.tok({ text: 'が' }, 'ga');
      const nai = b4b.tok({ textOneOf: ['ない', 'なかった', 'ありません', 'ないです'] }, 'nai');

      b4b.inOrder(adj, te, 2);
      b4b.inOrder(te, shikata, 5);
      b4b.inOrder(shikata, ga, 1);
      b4b.inOrder(ga, nai, 1);

      b4b.captureSpan('てしょうがない', adj, nai);
    },

    // Pattern 5: Na-adjective + で + しょうがない
    // e.g., 好きでしょうがない, 便利で仕方がない
    (b5) => {
      const adj = b5.adj({}, 'adj');
      const de = b5.tok({ text: 'で' }, 'de');
      const shou = b5.tok({ textOneOf: ['しょう', '仕方'] }, 'shou');
      const ga = b5.tok({ text: 'が' }, 'ga');
      const nai = b5.tok({ textOneOf: ['ない', 'ありません', 'ないです'] }, 'nai');

      b5.inOrder(adj, de, 2);
      b5.inOrder(de, shou, 5);
      b5.inOrder(shou, ga, 1);
      b5.inOrder(ga, nai, 1);

      b5.captureSpan('てしょうがない', adj, nai);
    },

    // Pattern 6: Na-adjective + で + 仕方がない
    // e.g., 好きで仕方がない, 不便で仕方がない
    (b6) => {
      const adj = b6.adj({}, 'adj');
      const de = b6.tok({ text: 'で' }, 'de');
      const shikata = b6.tok({ text: '仕方' }, 'shikata');
      const ga = b6.tok({ text: 'が' }, 'ga');
      const nai = b6.tok({ textOneOf: ['ない', 'なかった', 'ありません', 'ないです'] }, 'nai');

      b6.inOrder(adj, de, 2);
      b6.inOrder(de, shikata, 5);
      b6.inOrder(shikata, ga, 1);
      b6.inOrder(ga, nai, 1);

      b6.captureSpan('てしょうがない', adj, nai);
    },

    // Pattern 7: Combined token forms (しょうがない as single token)
    // Handles cases where GiNZA parses the expression as one unit
    (b7) => {
      const verbOrAdj = b7.tok({ posOneOf: ['VERB', 'ADJ'] }, 'verbOrAdj');
      const te = b7.tok({ textOneOf: ['て', 'で'] }, 'te');
      const shouganai = b7.tok({ textOneOf: ['しょうがない', '仕方がない'] }, 'shouganai');

      b7.inOrder(verbOrAdj, te, 2);
      b7.inOrder(te, shouganai, 5);

      b7.captureSpan('てしょうがない', verbOrAdj, shouganai);
    },

    // Pattern 8: Very permissive - match any predicate + te/de + しょう/仕方 + が + nai
    // Handles edge cases in GiNZA tokenization
    (b8) => {
      const predicate = b8.tok({ posOneOf: ['VERB', 'ADJ'] }, 'predicate');
      const teDe = b8.tok({ textOneOf: ['て', 'で'] }, 'teDe');
      const shou = b8.tok({ textOneOf: ['しょう', '仕方'] }, 'shou');
      const gaNai = b8.tok({ textOneOf: ['がない', 'がありません', 'がないです'] }, 'gaNai');

      b8.inOrder(predicate, teDe, 3);
      b8.inOrder(teDe, shou, 5);
      b8.inOrder(shou, gaNai, 2);

      b8.captureSpan('てしょうがない', predicate, gaNai);
    },

    // Pattern 9: Ultra-permissive - match predicate + te/de + combined ending
    (b9) => {
      const predicate = b9.tok({ posOneOf: ['VERB', 'ADJ'] }, 'predicate');
      const teDe = b9.tok({ textOneOf: ['て', 'で'] }, 'teDe');
      const ending = b9.tok({
        textOneOf: [
          'しょうがない',
          '仕方がない',
          'しょうがありません',
          '仕方がありません',
          'しょうがないです',
          '仕方がないです',
        ],
      }, 'ending');

      b9.inOrder(predicate, teDe, 3);
      b9.inOrder(teDe, ending, 5);

      b9.captureSpan('てしょうがない', predicate, ending);
    }
  );
});
