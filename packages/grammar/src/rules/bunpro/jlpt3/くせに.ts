import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('くせに', (r) => {
  // くせに - "despite/even though" with critical/complaining nuance
  // Patterns:
  // 1. Verb/Adj stem + くせに (three tokens: stem + くせ + に)
  // 2. Noun + のくせに (three tokens: noun + の + くせ + に)
  // 3. な-adjective + な + くせに (four tokens: adj + な + くせ + に)
  // 4. がる + なくせに (special merged case: がる + な + くせ becomes がる + なくせ + に)
  // 5. Single token くせに (GiNZA sometimes tokenizes as one)
  //
  // くせ is a noun (癖) + case marker に, used as a conjunction
  // It attaches to attributive forms and expresses criticism/contempt
  //
  // GiNZA may tokenize くせに as either:
  // - Three tokens: stem/くせ/に (most common)
  // - Two tokens: stem/(くせに) or (stem+くせ)/に
  // - Single token: くせに (ADP/SCONJ)

  r.either(
    // Pattern 1: VERB stem + くせに
    (b) => {
      const verbStem = b.verb({ tag: '動詞-非自立可能' }, 'verbStem');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(verbStem, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', verbStem, ni);
    },
    // Pattern 2: NOUN with verb tag + くせに
    (b) => {
      const verbStem = b.tok({ pos: 'NOUN', tag: '動詞-一般' }, 'verbStem');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(verbStem, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', verbStem, ni);
    },
    // Pattern 3: NOUN general tag + くせに
    (b) => {
      const verbStem = b.tok({ pos: 'NOUN', tag: '名詞-普通名詞-一般' }, 'verbStem');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(verbStem, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', verbStem, ni);
    },
    // Pattern 4: AUX with verb tag + くせに
    (b) => {
      const verbStem = b.aux({ tag: '動詞-非自立可能' }, 'verbStem');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(verbStem, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', verbStem, ni);
    },
    // Pattern 5: AUX with auxiliary verb tag (e.g., た, て) + くせに
    (b) => {
      const verbStem = b.aux({ tag: '助動詞' }, 'verbStem');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(verbStem, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', verbStem, ni);
    },
    // Pattern 6: い-adjective + くせに
    (b) => {
      const adj = b.adj({}, 'adj');
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(adj, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('くせに', adj, ni);
    },
    // Pattern 7: な-adjective + な + くせに
    (b) => {
      const naAdj = b.adj({}, 'naAdj');
      const na = b.aux({ text: 'な' }, 'na');
      b.inOrder(naAdj, na, 5);
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(na, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('なくせに', naAdj, ni);
    },
    // Pattern 8: Noun + のくせに
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);
      const kuse = b.tok({ textOneOf: ['くせ', '癖'] }, 'kuse');
      const ni = b.particle('に', 'ni');
      b.inOrder(no, kuse, 10);
      b.inOrder(kuse, ni, 1);
      b.captureSpan('のくせに', noun, ni);
    },
    // Pattern 9: がる verb suffix + くせ merged as なくせ
    (b) => {
      const garu = b.tok({ lemmaOneOf: ['がる', 'がり'], tag: '接尾辞-動詞的' }, 'garu');
      const nakuse = b.tok({ text: 'なくせ', pos: 'NOUN' }, 'nakuse');
      b.inOrder(garu, nakuse, 10);
      const ni = b.particle('に', 'ni');
      b.inOrder(nakuse, ni, 1);
      b.captureSpan('くせに', garu, ni);
    },
    // Pattern 10: Single token くせに (GiNZA sometimes tokenizes as one)
    (b) => {
      const kuseniTok = b.tok({ text: 'くせに' }, 'kuseniTok');
      const prevTok = b.tok({}, 'prevTok');
      b.inOrder(prevTok, kuseniTok, 10);
      b.captureSpan('くせに', prevTok, kuseniTok);
    },
    // Pattern 11: Single token 癖に (kanji version)
    (b) => {
      const kuseNiTok = b.tok({ text: '癖に' }, 'kuseNiTok');
      const prevTok2 = b.tok({}, 'prevTok2');
      b.inOrder(prevTok2, kuseNiTok, 10);
      b.captureSpan('くせに', prevTok2, kuseNiTok);
    }
  );
});