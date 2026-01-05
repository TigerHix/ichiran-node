import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('とおり', (r) => {
  // とおり (toori) - "just as, according to, exactly as"
  // Meaning: "in that way", "exactly", "precisely", "just as"
  //
  // This grammar point expresses that something is the same or follows
  // a pattern/plan/instruction exactly.
  //
  // Patterns:
  // 1. Verb (plain/past form) + とおり/とおりに
  //    思ったとおり (just as I thought)
  //    言ったとおり (exactly as said)
  //    おっしゃる通りです (precisely as you say)
  // 2. Noun + の + とおり/とおりに
  //    次のとおりです (as follows)
  //    ご覧のとおり (as you can see)
  //    説明書のとおりに (according to the manual)
  // 3. Noun + どおり/どおりに (voiced variant after certain nouns)
  //    計画どおり (as planned)
  //    契約どおり (according to the contract)
  //    予定どおり (as scheduled)
  // 4. その + とおり (demonstrative: "exactly that")
  //    その通りだ (that's exactly it)
  //    その通りです (that's precisely right)
  //
  // Note: ように (JLPT4) is similar but less emphatic about exactness

  r.either(
    // Pattern 1a: Verb (plain form or past form) + とおり
    // 思ったとおり, 言ったとおり, 報告したとおり
    // おっしゃるとおり, 言うとおり
    // GiNZA parses past tense verbs like "思った" as AUX (助動詞)
    // with tag "助動詞-タ", not VERB
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const toori = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        textOneOf: ['とおり', '通り'],
      }, 'toori');
      b.inOrder(verb, toori, 1);
      b.captureSpan('とおり', verb, toori);
    },

    // Pattern 1b: Verb + とおりに (with に particle)
    // 言うとおりにして, とおりに
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const toori = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        textOneOf: ['とおり', '通り'],
      }, 'toori');
      const ni = b.particle('に', 'ni');
      b.inOrder(verb, toori, 1);
      b.inOrder(toori, ni, 1);
      b.captureSpan('とおり', verb, ni);
    },

    // Pattern 2a: Noun + の + とおり
    // 次のとおり, ご覧のとおり, お察しのとおり, 説明書のとおり
    // Also matches: 僕の思ったとおり (where "僕の" is noun+no, "思った" is verb)
    // This pattern is more flexible to capture noun+no+(anything)+とおり
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const no = b.particle('の', 'no');
      const toori = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        textOneOf: ['とおり', '通り'],
      }, 'toori');
      b.inOrder(noun, no, 1);
      b.inOrder(no, toori, 5); // Allow up to 5 tokens between の and とおり
      b.captureSpan('とおり', noun, toori);
    },

    // Pattern 2b: Noun + の + とおりに
    // 説明書のとおりに
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const no = b.particle('の', 'no');
      const toori = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        textOneOf: ['とおり', '通り'],
      }, 'toori');
      const ni = b.particle('に', 'ni');
      b.inOrder(noun, no, 1);
      b.inOrder(no, toori, 5); // Allow up to 5 tokens between の and とおり
      b.inOrder(toori, ni, 1);
      b.captureSpan('とおり', noun, ni);
    },

    // Pattern 3a: Noun + どおり (voiced variant)
    // 計画どおり, 契約どおり, 予定どおり
    // Note: Must match after verb too (e.g., 思ったどおり in some contexts)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'VERB'],
      }, 'noun');
      const doori = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        textOneOf: ['どおり', '通'],
      }, 'doori');
      b.inOrder(noun, doori, 1);
      b.captureSpan('とおり', noun, doori);
    },

    // Pattern 3b: Noun + どおりに (voiced variant with に)
    // 予定どおりに, 計画どおりに
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'VERB'],
      }, 'noun');
      const doori = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        textOneOf: ['どおり', '通'],
      }, 'doori');
      const ni = b.particle('に', 'ni');
      b.inOrder(noun, doori, 1);
      b.inOrder(doori, ni, 1);
      b.captureSpan('とおり', noun, ni);
    },

    // Pattern 4: その + とおり (demonstrative "exactly that")
    // その通りだ, その通りです, まったくその通りだ
    (b) => {
      const sono = b.tok({
        pos: 'DET',
        text: 'その',
      }, 'sono');
      const toori = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        textOneOf: ['とおり', '通り'],
      }, 'toori');
      b.inOrder(sono, toori, 1);
      b.captureSpan('とおり', sono, toori);
    }
  );
});
