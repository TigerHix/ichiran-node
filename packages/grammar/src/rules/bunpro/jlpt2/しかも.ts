import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('しかも', (r) => {
  // しかも (shikamo) - conjunction meaning "moreover, furthermore, and what's more"
  // A somewhat formal expression indicating that furthermore to the surprising (A),
  // (B) is also in unison, and should be considered together.
  // Used for objective opinions/observable facts rather than subjective opinions.
  //
  // Structure: Phrase (A) + しかも + Phrase (B)
  // Can appear:
  // - Between two sentences
  // - At the beginning of a sentence (referring back to previous)
  // - Between clauses
  //
  // Kanji forms: 然も, 而も (rare)
  //
  // Examples:
  // - 彼はハンサムだ。しかも、金持ちだ。
  // - 安い。しかも、美味しい。
  // - このテレビは画質がめちゃくちゃいい。しかも、受信機がついていない。
  // - 彼女はとても頭がいいし性格もいい。しかも、美人だからもてないわけがない。
  // - しかも、舞台の途中に足が絡んで、転けた。

  const shikamo = r.tok({
    lemmaOneOf: ['しかも', '然も', '而も'],
    pos: 'CCONJ',
  }, 'shikamo');

  r.capture(shikamo);
});
