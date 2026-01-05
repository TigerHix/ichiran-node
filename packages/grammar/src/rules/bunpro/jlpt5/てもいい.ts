import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('てもいい', (r) => {
  // Pattern: i-adj/na-adj/noun + て/で + も + いい
  // Meaning: "It is okay/fine even if [is adj/noun]"
  // This is the non-verb version of てもいい (verbs are handled by verb-てもいい)
  //
  // i-adjective use て (e.g., 冷たくてもいい - it's okay even if it's cold)
  // na-adjective and noun use で (e.g., 静かでもいい, 水曜日でもいい)
  //
  // GiNZA parsing varies:
  // - i-adj stem (つめたく): pos=VERB, inflectionForm=連用形-一般, lemma ends in い
  // - で from copula: pos=AUX, lemma=だ
  // - で as case marker: pos=ADP, lemma=で (for bare nouns)
  // - Some nouns are parsed as ADJ (e.g., 焼きそば)

  r.either(
    // Branch 1: i-adjective stem + て + も + いい (casual)
    // GiNZA tags i-adj stems as VERB with inflectionForm=連用形-一般
    // Key: lemma ends in い (i-adj) not る (verb)
    (b1) => {
      const adjStem = b1.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '連用形-一般',
        // Exclude verbs by checking lemma ends in い
        // i-adj lemmas end in い (e.g., 冷たい, 高い, 早い, 狭い)
        lemmaRe: /い$/,
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
        lemmaRe: /い$/,
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
      b2.inOrder(ii, desu, 3);

      b2.captureSpan('てもいい', adjStem, desu);
    },
    // Branch 3: na-adjective + で(lemma=だ) + も + いい (casual)
    (b3) => {
      const adj = b3.tok({
        posOneOf: ['ADJ', 'NOUN'],
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
      }, 'ii');

      b3.inOrder(adj, de, 2);
      b3.inOrder(de, mo, 1);
      b3.inOrder(mo, ii, 2);

      b3.captureSpan('でもいい', adj, ii);
    },
    // Branch 4: noun/pronoun + で(lemma=で as case marker) + も + いい (casual)
    (b4) => {
      const noun = b4.tok({
        posOneOf: ['NOUN', 'PRON', 'ADJ'],
      }, 'noun');

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
      }, 'ii');

      b4.inOrder(noun, de, 2);
      b4.inOrder(de, mo, 1);
      b4.inOrder(mo, ii, 2);

      b4.captureSpan('でもいい', noun, ii);
    },
    // Branch 5: na-adjective + で(lemma=だ) + も + いい + です (polite)
    (b5) => {
      const adj = b5.tok({
        posOneOf: ['ADJ', 'NOUN'],
      }, 'adj');

      const de = b5.tok({
        text: 'で',
        lemma: 'だ',
        pos: 'AUX',
      }, 'de');

      const mo = b5.tok({
        text: 'も',
        pos: 'ADP',
      }, 'mo');

      const ii = b5.adj({
        lemmaOneOf: ['いい', 'よい'],
      }, 'ii');

      const desu = b5.aux({
        lemma: 'です',
      }, 'desu');

      b5.inOrder(adj, de, 2);
      b5.inOrder(de, mo, 1);
      b5.inOrder(mo, ii, 2);
      b5.inOrder(ii, desu, 3);

      b5.captureSpan('でもいい', adj, desu);
    },
    // Branch 6: noun/pronoun + で(lemma=で as case marker) + も + いい + です (polite)
    (b6) => {
      const noun = b6.tok({
        posOneOf: ['NOUN', 'PRON', 'ADJ'],
      }, 'noun');

      const de = b6.tok({
        text: 'で',
        lemma: 'で',
        pos: 'ADP',
        dep: 'case',
      }, 'de');

      const mo = b6.tok({
        text: 'も',
        pos: 'ADP',
      }, 'mo');

      const ii = b6.adj({
        lemmaOneOf: ['いい', 'よい'],
      }, 'ii');

      const desu = b6.aux({
        lemma: 'です',
      }, 'desu');

      b6.inOrder(noun, de, 2);
      b6.inOrder(de, mo, 1);
      b6.inOrder(mo, ii, 2);
      b6.inOrder(ii, desu, 3);

      b6.captureSpan('でもいい', noun, desu);
    }
  );
});
