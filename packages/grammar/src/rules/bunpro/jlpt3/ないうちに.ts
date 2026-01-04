import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ないうちに', (r) => {
  // ないうちに - "while X is not yet" / "before X happens" / "without X"
  // Pattern: Verb[negative form] + うちに
  //
  // Examples from test data:
  // - 忘れないうちに (before I forget)
  // - 暗くならないうちに (before it gets dark)
  // - 知らないうちに (without knowing/before I knew)
  // - 会わないうちに (during the time we haven't met)
  // - 飽きられないうちに (before people lose interest)
  // - 冷めないうちに (before it gets cold)
  // - 眠くならないうちに (before I get sleepy)
  // - 泳がない内に (without swimming)
  // - 親しくならない内に (before we became close)
  // - 読み終わらない内に (before I had finished reading)
  // - 集まらない内に (before people had gathered)
  // - 変わらない内に (before they change their mind)
  // - 気づかない内に (before I could notice)
  //
  // The verb is in negative form (ない or 内 as alternate kanji)
  // うち is a temporal noun, に is the case marker
  //
  // GiNZA parsing variations:
  // - Verb negatives can be single tokens or split (verb + aux)
  // - うちに can be one token or two (うち + に)

  r.either(
    // Pattern 1: Match うちに as a single compound token
    // Some GiNZA parses treat うちに as ADV
    (b) => {
      const uchiNi = b.tok({
        text: 'うちに',
        posOneOf: ['ADV', 'ADP', 'NOUN'],
      }, 'uchiNi');
      const negativeVerb = b.tok({
        posOneOf: ['VERB', 'AUX'],
        textOneOf: ['ない', '内', 'らない', 'わない', 'かない', 'けない', 'げない', 'せない', 'てない', 'めない', 'れない', 'がない'],
      }, 'negativeVerb');
      b.inOrder(negativeVerb, uchiNi, 3);
      b.captureSpan('ないうちに', negativeVerb, uchiNi);
    },

    // Pattern 2: Split うち + に with negative verb before
    (b) => {
      const uchi = r.tok({
        textOneOf: ['うち', '内'],
        lemmaOneOf: ['うち', '内'],
      }, 'uchi');
      const ni = b.tok({
        text: 'に',
      }, 'ni');
      b.inOrder(uchi, ni, 1);

      const negativeVerb = b.tok({
        posOneOf: ['VERB', 'AUX'],
        textOneOf: ['ない', '内', 'らない', 'わない', 'かない', 'けない', 'げない', 'せない', 'てない', 'めない', 'れない', 'がない', 'およがない'],
      }, 'negativeVerb');
      b.inOrder(negativeVerb, uchi, 3);
      b.captureSpan('ないうちに', negativeVerb, ni);
    },

    // Pattern 3: Separate ない auxiliary + うちに
    (b) => {
      const uchi = r.tok({
        lemmaOneOf: ['うち', '内'],
        posOneOf: ['NOUN', 'PROPN', 'ADV'],
      }, 'uchi');
      const ni = b.particle('に', 'ni');
      b.inOrder(uchi, ni, 1);

      const verbStem = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ'],
      }, 'verbStem');
      const nai = b.aux({
        lemma: 'ない',
        textOneOf: ['ない', '内'],
      }, 'nai');
      b.inOrder(verbStem, nai, 3);
      b.inOrder(nai, uchi, 1);
      b.captureSpan('ないうちに', verbStem, ni);
    },

    // Pattern 4: ない as main verb + うちに
    (b) => {
      const uchi = r.tok({
        lemmaOneOf: ['うち', '内'],
        posOneOf: ['NOUN', 'PROPN', 'ADV'],
      }, 'uchi');
      const ni = b.particle('に', 'ni');
      b.inOrder(uchi, ni, 1);

      const naiVerb = b.verb({
        lemmaOneOf: ['ない', 'いる', 'なる'],
        textOneOf: ['ない', '内', 'らない'],
      }, 'naiVerb');
      b.inOrder(naiVerb, uchi, 3);
      b.captureSpan('ないうちに', naiVerb, ni);
    }
  );
});
