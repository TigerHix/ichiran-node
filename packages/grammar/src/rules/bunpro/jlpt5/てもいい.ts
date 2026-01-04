import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('てもいい', (r) => {
  // Pattern: i-adj/na-adj/noun + て/で + も + いい
  // Meaning: "It is okay/fine even if [is adj/noun]"
  // This is the non-verb version of てもいい (verbs are handled by verb-てもいい)
  //
  // i-adjective use て (e.g., 冷たくてもいい - it's okay even if it's cold)
  // na-adjective and noun use で (e.g., 静かでもいい, 水曜日でもいい)
  //
  // GiNZA parsing varies:
  // - i-adj stem (つめたく): pos=VERB, inflectionForm=連用形-一般
  // - で from copula: pos=AUX, lemma=だ
  // - で as case marker: pos=ADP, lemma=で (for bare nouns)

  r.either(
    // Branch 1: i-adjective stem + て + も + いい (casual)
    // GiNZA tags i-adj stems as VERB with inflectionForm=連用形-一般
    (b1) => {
      const adjStem = b1.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'adjStem');

      const te = b1.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      const mo = b1.tok({
        text: 'も',
        pos: 'ADP',
        dep: 'fixed',
      }, 'mo');

      const ii = b1.tok({
        lemmaOneOf: ['いい', 'よい'],
        posOneOf: ['AUX', 'ADJ'],
      }, 'ii');

      b1.inOrder(adjStem, te, 1);
      b1.inOrder(te, mo, 1);
      b1.inOrder(mo, ii, 1);

      b1.captureSpan('てもいい', adjStem, ii);
    },
    // Branch 2: i-adjective stem + て + も + いい + です (polite)
    (b2) => {
      const adjStem = b2.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'adjStem');

      const te = b2.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      const mo = b2.tok({
        text: 'も',
        pos: 'ADP',
        dep: 'fixed',
      }, 'mo');

      const ii = b2.tok({
        lemmaOneOf: ['いい', 'よい'],
        posOneOf: ['AUX', 'ADJ'],
      }, 'ii');

      const desu = b2.aux({
        lemma: 'です',
      }, 'desu');

      b2.inOrder(adjStem, te, 1);
      b2.inOrder(te, mo, 1);
      b2.inOrder(mo, ii, 1);
      b2.inOrder(ii, desu, 2);

      b2.captureSpan('てもいい', adjStem, desu);
    },
    // Branch 3: na-adjective + で(lemma=だ) + も + いい (casual)
    (b3) => {
      const adj = b3.adj({
        pos: 'ADJ',
      }, 'adj');

      const de = b3.tok({
        text: 'で',
        lemma: 'だ',
        pos: 'AUX',
      }, 'de');

      const mo = b3.tok({
        text: 'も',
        pos: 'ADP',
      }, 'mo');

      const ii = b3.adj({
        lemmaOneOf: ['いい', 'よい'],
        pos: 'ADJ',
      }, 'ii');

      b3.inOrder(adj, de, 2);
      b3.inOrder(de, mo, 1);
      b3.inOrder(mo, ii, 2);

      b3.captureSpan('でもいい', adj, ii);
    },
    // Branch 4: noun + で(lemma=で as case marker) + も + いい (casual)
    (b4) => {
      const noun = b4.noun({}, 'noun');

      const de = b4.tok({
        text: 'で',
        lemma: 'で',
        pos: 'ADP',
        dep: 'case',
      }, 'de');

      const mo = b4.tok({
        text: 'も',
        pos: 'ADP',
      }, 'mo');

      const ii = b4.adj({
        lemmaOneOf: ['いい', 'よい'],
        pos: 'ADJ',
      }, 'ii');

      b4.inOrder(noun, de, 2);
      b4.inOrder(de, mo, 1);
      b4.inOrder(mo, ii, 2);

      b4.captureSpan('でもいい', noun, ii);
    },
    // Branch 5: pronoun + で(lemma=で as case marker) + も + いい (casual)
    (b5) => {
      const pronoun = b5.tok({
        pos: 'PRON',
      }, 'pronoun');

      const de = b5.tok({
        text: 'で',
        lemma: 'で',
        pos: 'ADP',
        dep: 'case',
      }, 'de');

      const mo = b5.tok({
        text: 'も',
        pos: 'ADP',
      }, 'mo');

      const ii = b5.adj({
        lemmaOneOf: ['いい', 'よい'],
        pos: 'ADJ',
      }, 'ii');

      b5.inOrder(pronoun, de, 2);
      b5.inOrder(de, mo, 1);
      b5.inOrder(mo, ii, 2);

      b5.captureSpan('でもいい', pronoun, ii);
    },
    // Branch 6: na-adjective + で(lemma=だ) + も + いい + です (polite)
    (b6) => {
      const adj = b6.adj({
        pos: 'ADJ',
      }, 'adj');

      const de = b6.tok({
        text: 'で',
        lemma: 'だ',
        pos: 'AUX',
      }, 'de');

      const mo = b6.tok({
        text: 'も',
        pos: 'ADP',
      }, 'mo');

      const ii = b6.adj({
        lemmaOneOf: ['いい', 'よい'],
        pos: 'ADJ',
      }, 'ii');

      const desu = b6.aux({
        lemma: 'です',
      }, 'desu');

      b6.inOrder(adj, de, 2);
      b6.inOrder(de, mo, 1);
      b6.inOrder(mo, ii, 2);
      b6.inOrder(ii, desu, 2);

      b6.captureSpan('でもいい', adj, desu);
    },
    // Branch 7: noun + で(lemma=で as case marker) + も + いい + です (polite)
    (b7) => {
      const noun = b7.noun({}, 'noun');

      const de = b7.tok({
        text: 'で',
        lemma: 'で',
        pos: 'ADP',
        dep: 'case',
      }, 'de');

      const mo = b7.tok({
        text: 'も',
        pos: 'ADP',
      }, 'mo');

      const ii = b7.adj({
        lemmaOneOf: ['いい', 'よい'],
        pos: 'ADJ',
      }, 'ii');

      const desu = b7.aux({
        lemma: 'です',
      }, 'desu');

      b7.inOrder(noun, de, 2);
      b7.inOrder(de, mo, 1);
      b7.inOrder(mo, ii, 2);
      b7.inOrder(ii, desu, 2);

      b7.captureSpan('でもいい', noun, desu);
    },
    // Branch 8: pronoun + で(lemma=で as case marker) + も + いい + です (polite)
    (b8) => {
      const pronoun = b8.tok({
        pos: 'PRON',
      }, 'pronoun');

      const de = b8.tok({
        text: 'で',
        lemma: 'で',
        pos: 'ADP',
        dep: 'case',
      }, 'de');

      const mo = b8.tok({
        text: 'も',
        pos: 'ADP',
      }, 'mo');

      const ii = b8.adj({
        lemmaOneOf: ['いい', 'よい'],
        pos: 'ADJ',
      }, 'ii');

      const desu = b8.aux({
        lemma: 'です',
      }, 'desu');

      b8.inOrder(pronoun, de, 2);
      b8.inOrder(de, mo, 1);
      b8.inOrder(mo, ii, 2);
      b8.inOrder(ii, desu, 2);

      b8.captureSpan('でもいい', pronoun, desu);
    }
  );
});
