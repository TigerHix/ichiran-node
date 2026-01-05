import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: したがって (shitagatte) - "therefore, thus, consequently"
 *
 * A formal conjunction expressing logical consequence or result.
 * From the verb 従う (shitagau - to comply/follow), in te-form acting as conjunction.
 * Meaning: "following from (A), therefore (B)" or "as a result of (A), (B)".
 *
 * Kanji: 従って
 *
 * Structures:
 * - [Cause/Reason]. したがって、[Result].
 * - したがって、[Result]. (at beginning of sentence)
 *
 * Examples:
 * - 彼は長男です。したがって、次期社長はおそらく彼でしょう。
 *   (He is the oldest son. Therefore, he will likely be the next CEO.)
 * - 電車が脱線してしまった。したがって、今日中の到着は無理だと思われます。
 *   (The train derailed. Therefore, arrival today is impossible.)
 * - 公害が増大している。したがって、工場は閉鎖する方向になります。
 *   (Pollution is increasing. Therefore, the factory will be closed.)
 *
 * Key characteristics:
 * - Formal/written style conjunction
 * - Expresses logical cause-effect relationship
 * - Can be replaced by だから in all cases, but not vice versa
 * - Typically followed by comma (、) in Japanese
 * - Appears at beginning of sentence or clause
 *
 * Similar conjunctions (should NOT match):
 * - だから (dagara) - casual "therefore"
 * - ですから (desukara) - polite "therefore"
 * - なので (nanode) - formal "because/therefore"
 * - それで (sorede) - "so/then" (temporal sequence)
 * - そこで (sokode) - "accordingly/therefore" (action-oriented)
 * - その結果 (sono kekka) - "as a result"
 * - ゆえに (yue ni) - "due to/therefore" (more formal)
 * - 従う (shitagau) - verb "to follow/comply" (not conjunction)
 *
 * GiNZA parse structure:
 * - したがって is typically parsed as two tokens:
 *   - したがっ (CCONJ, lemma=したがう, dep=cc, inflectionForm=連用形-促音便)
 *   - て (AUX/SCONJ/PART, text=て)
 * - Sometimes as single token: したがって (CCONJ/ADV)
 * - The dep=cc indicates conjunction usage
 */
export default linguisticRule('したがって', (r) => {
  // したがって is the te-form of 従う (shitagau - to follow/comply)
  // When used as a conjunction, it means "therefore, thus, consequently"
  //
  // GiNZA parses this as two tokens:
  // - したがっ (pos=CCONJ, lemma=したがう, dep=cc, inflectionForm=連用形-促音便)
  // - て (text=て, pos=AUX/SCONJ/PART)
  //
  // The conjunction appears at sentence/clause beginning to show logical consequence
  //
  // Kanji variant: 従って (same reading, same meaning)

  r.either(
    // Pattern 1: Two-token form (most common)
    // したがっ (CCONJ) + て (AUX/SCONJ/PART)
    (b) => {
      const shitagatsu = b.tok({
        textOneOf: ['したがっ', '従っ'],
        pos: 'CCONJ',
        dep: 'cc',
      }, 'shitagatsu');
      const te = b.tok({
        text: 'て',
        posOneOf: ['AUX', 'SCONJ', 'PART'],
      }, 'te');
      b.inOrder(shitagatsu, te, 1);
      b.captureSpan('したがって', shitagatsu, te);
    },
    // Pattern 2: Single conjunction token (hiragana or kanji)
    // Less common but possible
    (b) => {
      const shitagatte = b.tok({
        textOneOf: ['したがって', '従って'],
        posOneOf: ['CCONJ', 'ADV', 'SCONJ'],
        dep: 'cc',
      }, 'shitagatte');
      b.capture(shitagatte);
    }
  );
});
