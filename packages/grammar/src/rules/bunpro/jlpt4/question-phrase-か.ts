import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('question-phrase-か', (r) => {
  // Question-phrase + か: Embedding questions within sentences
  // Pattern: (Question word OR Verb) + か + verb (of knowing/deciding)
  // Examples:
  //   - どこかわかる (know where)
  //   - 何か知っている (know what)
  //   - 来るか分かる (know if [someone] comes)
  //   - 足りるかわかる (know if it's enough)
  //
  // This is the adverbial particle か that marks embedded questions/uncertainty, different from:
  // - Sentence-ending question particle か (行きますか？)
  // - Indefinite pronoun か (どこか = somewhere, JLPT5 pattern)
  // - Alternative particle か (リンゴかバナナ = apple or banana)
  // - かどうか pattern (whether or not) - handled by separate rule

  r.either(
    // Branch 1: Question word + か (どこか, 何か, いつか, etc.)
    (r1) => {
      // Question words: どこ, 何, いつ, 誰, だれ, なに, なん, etc.
      // GiNZA tags these as PRON, ADV, DET, or ADJ depending on usage
      // NOTE: "どう" is excluded to avoid false positive on "かどうか" pattern
      const whWord = r1.tok({
        // Common question words (not exhaustive - GiNZA should tag these appropriately)
        textOneOf: [
          'どこ',  // where
          '何', 'なに', 'なん',  // what
          'いつ',  // when
          '誰', 'だれ',  // who
          'どの',  // which
          'いくつ',  // how many
          'なぜ',  // why
          'どうして',  // why
          'どんな',  // what kind of
          '何時', 'なんじ',  // what time
          '何で', 'なんで',  // why/how
        ],
      }, 'whWord');

      // The particle か that marks the embedded question
      // GiNZA tags this as either PART (particle) or ADP (adposition)
      // We accept both - the key is the context (question word + か + verb of knowing)
      const ka = r1.tok({
        text: 'か',
        posOneOf: ['PART', 'ADP'],
      }, 'ka');

      // Verbs that commonly follow embedded questions
      const verb = r1.verb({
        lemmaOneOf: [
          '分かる', 'わかる',  // to understand
          '知る',  // to know
          '決める',  // to decide
          '覚える',  // to remember
          '確認',  // to confirm
          '聞く',  // to ask
        ],
      }, 'verb');

      // Require the sequence: question word + か + verb
      // Allow large distance between whWord and ka (for embedded clauses)
      r1.inOrder(whWord, ka, 10);
      r1.inOrder(ka, verb, 5);

      // Capture the question phrase (question word + か)
      r1.captureSpan('question-phrase-か', whWord, ka);
    },

    // Branch 2: Verb + か (来るか, 足りるか, etc.) - embedded yes/no question
    (r2) => {
      // Any verb (plain form, dictionary form) before the か particle
      const verb1 = r2.verb({}, 'verb1');

      // The particle か that marks the embedded question
      const ka = r2.tok({
        text: 'か',
        posOneOf: ['PART', 'ADP'],
      }, 'ka');

      // Verbs that commonly follow embedded questions
      const verb2 = r2.verb({
        lemmaOneOf: [
          '分かる', 'わかる',  // to understand
          '知る',  // to know
          '決める',  // to decide
          '覚える',  // to remember
          '確認',  // to confirm
          '聞く',  // to ask
        ],
      }, 'verb2');

      // Require the sequence: verb + か + verb (of knowing)
      r2.inOrder(verb1, ka, 2);
      r2.inOrder(ka, verb2, 5);

      // Capture the question phrase (verb + か)
      r2.captureSpan('question-phrase-か', verb1, ka);
    }
  );
});
