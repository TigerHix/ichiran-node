import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ば (conditional: if/when)
 *
 * Matches verb/adj/noun + ば (conditional form)
 *
 * Verbs:
 * - る-verbs: ～れば (e.g., 見れば, 食べれば)
 * - う-verbs: ～えば (e.g., 行けば, 読めば, 話せば, 死ねば)
 * - Irregular: すれば, くれば
 *
 * I-adjectives:
 * - ～ければ (e.g., 高ければ, よければ)
 *
 * Na-adjectives/Nouns:
 * - なら(ば) (e.g., 先生なら(ば), 便利なら(ば))
 *
 * Negative forms:
 * - Verb[nai] + なければ (e.g., なければ, しなければ)
 * - I-adj[nai] + ければ (e.g., なければ, よくなければ)
 * - Na-adj/Noun + で(は)なければ (e.g., でなければ, じゃなければ)
 *
 * GiNZA parsing:
 * - Verbs: 見れ[AUX,lemma=られる,infl=仮定形-一般] + ば[SCONJ]
 *          行け[VERB,lemma=行く,infl=仮定形-一般] + ば[SCONJ]
 * - I-adj:  なけれ[AUX,lemma=ない,infl=仮定形-一般] + ば[SCONJ]
 * - Na-adj: 好き[ADJ] + なら[SCONJ,lemma=だ] + ば[SCONJ]
 */
export default bunproLinguisticRule('ば', (r) => {
  r.either(
    // Pattern 1a: Verbs/I-adj with explicit inflectionForm attribute
    // e.g., なけれ + ば (for nai verbs), 見れ + ば (for potential verbs)
    (b) => {
      const stem = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ'],
        inflectionForm: '仮定形-一般',
      }, 'stem');

      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      b.inOrder(stem, ba, 1);
      b.captureSpan('ば', stem, ba);
    },

    // Pattern 1b: Verbs/I-adj followed by ば (conditional forms)
    // e.g., 行け + ば, 読め + ば, 曲がれ + ば, 止め + ば
    // Match by position: any VERB/AUX/ADJ immediately before SCONJ ば
    (b) => {
      const stem = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ'],
        // Exclude forms that are NOT conditional (e.g., dictionary forms ending in ru/u)
        // by checking that text doesn't end with these typical non-conditional endings
        // and is followed by the conditional particle ば
      }, 'stem');

      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      // Must be immediately adjacent: stem + ば
      b.inOrder(stem, ba, 1);
      b.captureSpan('ば', stem, ba);
    },

    // Pattern 2: Na-adjectives and Nouns + なら(ば)
    // e.g., 嫌いならば, 先生ならば, バイクならば
    (b) => {
      // Noun or na-adjective
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');

      // Conditional form: なら (from だ)
      const nara = b.tok({
        text: 'なら',
        lemmaOneOf: ['だ', 'なる'],
        pos: 'SCONJ',
      }, 'nara');

      // Must be adjacent: noun + なら
      b.inOrder(noun, nara, 1);

      // Optional ば particle
      b.optional((optBranch) => {
        const ba = optBranch.tok({
          text: 'ば',
          pos: 'SCONJ',
        }, 'ba');
        optBranch.inOrder(nara, ba, 1);
        optBranch.captureSpan('ば', noun, ba);
      });

      // If ば is not present, still capture noun + なら
      b.captureSpan('ば', noun, nara);
    }
  );
});
