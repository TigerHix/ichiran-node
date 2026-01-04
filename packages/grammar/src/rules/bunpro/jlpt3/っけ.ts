import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('っけ', (r) => {
  // っけ (kke) - Sentence-final particle for confirmation/remembering
  // Meaning: "What is...again?", "Was it...that...?", "Did you say...?"
  //
  // This is a casual sentence-ending particle used when trying to recall
  // or confirm information that the speaker has forgotten.
  //
  // Patterns:
  // 1. Predicate + ん + だ + っけ (nominalization + copula)
  // 2. Word + だっ + た + ん + だ + っけ (past copula + nominalization + copula)
  // 3. Word + だっ + た + っけ (simple past copula)
  // 4. Any predicate + だ/た auxiliary + っけ
  // 5. Verb[る] + っけ (younger generation casual)
  //
  // IMPORTANT: Pattern order matters! More specific patterns must come first.
  //
  // GiNZA parsing notes:
  // - っけ is PART with tag=助詞-終助詞 (sentence-final particle)
  // - dep=mark, points to sentence root
  // - Always appears at sentence end (followed only by punctuation)

  const kke = r.tok({
    text: 'っけ',
    pos: 'PART',
    tag: '助詞-終助詞',
    dep: 'mark',
  }, 'kke');

  r.either(
    // Pattern 1: Predicate + ん + だ + っけ (nominalization + copula)
    // 晴れるんだっけ, 食べれるんだっけ, 納豆食べれるんだっけ
    // 行ったことないんだっけ, 見たことないんだっけ, 食べられないんだっけ
    // 日本語の勉強はいつから始めたんだっけ？
    // Note: "ん" is SCONJ with text=ん
    // "だ" is AUX with lemma=だ
    (b) => {
      const root = b.tok({
        posOneOf: ['VERB', 'ADJ'],
      }, 'root');
      const nom = b.tok({
        text: 'ん',
      }, 'nom');
      const da = b.aux({
        lemma: 'だ',
      }, 'da');
      b.inOrder(root, nom, 15);
      b.inOrder(nom, da, 1);
      b.inOrder(da, kke, 1);
      b.captureSpan('っけ', root, kke);
    },

    // Pattern 2: Word + だっ + た + ん + だ + っけ (past copula + nominalization + copula)
    // この学校に入るのがずっと夢だったんだっけ
    // Note: Both だっ and た are auxiliaries attached to the predicate, not to each other
    (b) => {
      const predicate = b.tok({
        posOneOf: ['NOUN', 'PRON', 'VERB'],
      }, 'predicate');
      const datsu = b.aux({
        lemma: 'だ',
        inflectionForm: '連用形-促音便',
      }, 'datsu');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');
      const nom = b.tok({
        text: 'ん',
      }, 'nom');
      const da = b.aux({
        lemma: 'だ',
      }, 'da');
      b.inOrder(predicate, datsu, 3);
      b.inOrder(datsu, ta, 1);
      b.inOrder(ta, nom, 2);
      b.inOrder(nom, da, 1);
      b.inOrder(da, kke, 1);
      b.captureSpan('っけ', predicate, kke);
    },

    // Pattern 3: Word + だっ + た + っけ (simple past copula)
    // 日本の文化について発表をしたのは誰だったっけ
    // Note: "だっ" is AUX with lemma=だ, inflectionForm=連用形-促音便
    (b) => {
      const predicate = b.tok({
        posOneOf: ['NOUN', 'PRON', 'VERB'],
      }, 'predicate');
      const datsu = b.aux({
        lemma: 'だ',
        inflectionForm: '連用形-促音便',
      }, 'datsu');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');
      b.inOrder(predicate, datsu, 5);
      b.auxOf(datsu, ta);
      b.inOrder(ta, kke, 1);
      b.captureSpan('っけ', predicate, kke);
    },

    // Pattern 4: Any predicate (verb/adj/noun/pronoun) + だ/た auxiliary + っけ
    // 食べたっけ, 渡したっけ, 青かったっけ, 誰だっけ, 終わりだっけ, 土曜日って暇だっけ
    (b) => {
      const predicate = b.tok({
        posOneOf: ['VERB', 'ADJ', 'NOUN', 'PRON'],
      }, 'predicate');
      const ta = b.aux({
        lemmaOneOf: ['た', 'だ'],
      }, 'ta');
      b.inOrder(predicate, ta, 3);
      b.inOrder(ta, kke, 1);
      b.captureSpan('っけ', predicate, kke);
    },

    // Pattern 5: Verb[る] + っけ (younger generation casual)
    // 話せるっけ, 通れるっけ, 連れていけるっけ
    // Note: According to Bunpro, grammatically correct form is んだっけ
    // but younger generation often uses just っけ after verbs
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(verb, kke, 1);
      b.captureSpan('っけ', verb, kke);
    }
  );
});
