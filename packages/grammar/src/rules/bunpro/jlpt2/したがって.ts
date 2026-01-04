import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: したがって (shitagatte) - Therefore, Thus, As a result
 *
 * A formal conjunction used to indicate that (B) follows logically from (A).
 * "In compliance with (A), (B)."
 *
 * Structure:
 * - (Cause) + したがって + (Result)
 *
 * Can be written in hiragana (したがって) or kanji (従って)
 *
 * Examples:
 * - 彼は長男です。したがって、次期社長はおそらく彼でしょう。
 *   (He is the oldest son. Therefore, he will soon be the CEO, right?)
 * - 雨が降っている。したがって、試合は中止だ。
 *   (It is raining. Therefore, the game is cancelled.)
 * - 本番直前です。したがって、彼は極度に緊張しています。
 *   (It is right before the big game. Therefore, he is at maximum nervousness.)
 *
 * Key discriminators:
 * - したがって is a conjunction (接続詞)
 * - Appears at the beginning of a sentence
 * - More formal than だから, それで, etc.
 * - Can be written as 従って (kanji) or したがって (hiragana)
 * - GiNZA parses したがって as two tokens: したがっ(CCONJ) + て(SCONJ)
 * - 従って may be parsed as single token or two tokens
 *
 * GiNZA parse structure:
 * - したがって: したがっ(CCONJ,lemma=したがう) + て(SCONJ,dep=fixed)
 * - 従って: may be 従っ(CCONJ,lemma=従う) + て(SCONJ,dep=fixed) or single token
 */
export default linguisticRule('したがって', (r) => {
  // Match したがって or 従って as a conjunction
  // GiNZA typically parses this as two tokens:
  // - したがっ/従っ (CCONJ with lemma=したがう/従う) + て (SCONJ with dep=fixed)
  r.either(
    // Pattern 1: Two tokens - したがっ + て or 従っ + て
    (b1) => {
      const shitagau_te = b1.tok({
        lemma: 'したがう',
        pos: 'CCONJ',
      }, 'shitagau_te');
      const te = b1.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'fixed',
      }, 'te');

      b1.inOrder(shitagau_te, te, 1);
      b1.captureSpan('したがって', shitagau_te, te);
    },

    // Pattern 2: Two tokens - 従っ + て (kanji form)
    (b2) => {
      const shitagau_te = b2.tok({
        lemma: '従う',
        pos: 'CCONJ',
      }, 'shitagau_te');
      const te = b2.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'fixed',
      }, 'te');

      b2.inOrder(shitagau_te, te, 1);
      b2.captureSpan('したがって', shitagau_te, te);
    },

    // Pattern 3: Single token したがって (less common)
    (b3) => {
      const shitagatte = b3.tok({
        textOneOf: ['したがって', '従って'],
        posOneOf: ['CONJ', 'ADV', 'SCONJ', 'CCONJ'],
      }, 'shitagatte');

      b3.captureSpan('したがって', shitagatte, shitagatte);
    }
  );
});
