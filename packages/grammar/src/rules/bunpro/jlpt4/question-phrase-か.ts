import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('question-phrase-か', (r) => {
  // Question-phrase + か: Embedding questions within sentences
  // Pattern: Question word + か + verb (of knowing/deciding)
  // Examples: どこかわかる (know where), 何か知っている (know what), いつか決める (decide when)
  //
  // This is the adverbial particle か that marks embedded questions, different from:
  // - Sentence-ending question particle か (行きますか？)
  // - Indefinite pronoun か (どこか = somewhere, JLPT5 pattern)
  // - Alternative particle か (リンゴかバナナ = apple or banana)

  // Question words: どこ, 何, いつ, 誰, だれ, なに, なん, どう, どの, いくつ, なぜ, どうして, etc.
  const whWord = r.tok({
    posOneOf: ['PRON', 'ADV', 'DET'],
    // Common question words (not exhaustive - GiNZA should tag these appropriately)
    textOneOf: [
      'どこ',  // where
      '何', 'なに', 'なん',  // what
      'いつ',  // when
      '誰', 'だれ',  // who
      'どう',  // how
      'どの',  // which
      'いくつ',  // how many
      'なぜ',  // why
      'どうして',  // why
      'どんな',  // what kind of
      '何時', 'なんじ',  // what time
    ],
  }, 'whWord');

  // The particle か that marks the embedded question
  // This should NOT have dep=case (which would make it an indefinite pronoun like どこか)
  const ka = r.tok({ text: 'か', pos: 'PART' }, 'ka');

  // Verbs that commonly follow embedded questions
  // - 知っている (to know)
  // - わかる (to understand)
  // - 決める (to decide)
  // - 覚える (to remember)
  // - 分かる (to understand - kanji form)
  const verb = r.verb({
    lemmaOneOf: [
      '分かる', 'わかる',  // to understand
      '知る', '知っている', 'している',  // to know
      '決める',  // to decide
      '覚える',  // to remember
      '聞く',  // to ask
    ],
  }, 'verb');

  // Require the sequence: question word + か + verb (with reasonable distance)
  r.inOrder(whWord, ka, 3);
  r.inOrder(ka, verb, 5);

  // Capture the question phrase (question word + か)
  r.captureSpan('question-phrase-か', whWord, ka);
});
