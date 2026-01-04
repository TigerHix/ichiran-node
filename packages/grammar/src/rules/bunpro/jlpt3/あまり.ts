import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('あまり', (r) => {
  // あまり (amari) - "so much...that" (excessive degree leading to negative result)
  // This is DIFFERENT from あまり-ない (JLPT4) which means "not very"
  //
  // Patterns:
  // 1. Verb/Aux + あまり: 食べすぎたあまり, 勉強したあまり, 深刻化するあまり
  // 2. Noun/Abstract noun + の + あまり: 驚きのあまり, 暑さのあまり, 悲しみのあまり
  // 3. Na-adjective + な + あまり: 楽しみなあまり, 好きなあまり
  //
  // The result after あまり is typically negative (e.g., ない, できなかった)
  //
  // GiNZA parsing notes:
  // - あまり can be NOUN (名詞-普通名詞-副詞可能) or ADV (副詞)
  // - Noun before の can be NOUN, PROPN, or VERB (for 暑さ-type words)
  // - Na-adj + な: "な" is AUX with lemma=だ

  r.either(
    // Pattern 1: Verb/Aux in 連体形 + あまり
    // 食べすぎたあまり (た is AUX with 連体形-一般)
    // 勉強したあまり (た is AUX with 連体形-一般)
    // 深刻化するあまり (する is AUX with 連体形-一般)
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連体形-一般',
      }, 'verb');
      const amari = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        lemmaOneOf: ['あまり', '余り'],
      }, 'amari');
      b.inOrder(verb, amari, 1);
      b.captureSpan('あまり', verb, amari);
    },

    // Pattern 2a: Noun/Abstract noun + の + あまり
    // 驚きのあまり, 暑さのあまり, 悲しみのあまり, 喜びのあまり
    // Note: 暑さ/悲しみ/etc are NOUN/PROPN with tag 名詞-普通名詞-一般
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'VERB'],
        tagOneOf: [
          '名詞-普通名詞-一般',
          '名詞-普通名詞-サ変可能',
          '名詞-普通名詞-サ変形状',
        ],
      }, 'noun');
      const no = b.particle('の', 'no');
      const amari = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        lemmaOneOf: ['あまり', '余り'],
      }, 'amari');
      b.inOrder(noun, no, 1);
      b.inOrder(no, amari, 1);
      b.captureSpan('あまり', noun, amari);
    },

    // Pattern 2b: I-adjective stem + さ (suffix) + の + あまり
    // 悲しさのあまり, 苦しさのあまり, 怖さのあまり
    // GiNZA parses these as adj (形容詞-一般) + PART さ (接尾辞-名詞的-一般)
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['ADJ', 'VERB'],
        tag: '形容詞-一般',
      }, 'adjStem');
      const sa = b.tok({
        pos: 'PART',
        tag: '接尾辞-名詞的-一般',
        lemma: 'さ',
      }, 'sa');
      const no = b.particle('の', 'no');
      const amari = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        lemmaOneOf: ['あまり', '余り'],
      }, 'amari');
      b.inOrder(adjStem, sa, 1);
      b.inOrder(sa, no, 1);
      b.inOrder(no, amari, 1);
      b.captureSpan('あまり', adjStem, amari);
    },

    // Pattern 3: Na-adjective + な + あまり
    // 楽しみなあまり (楽しみ is ADJ with 名詞-普通名詞-一般)
    // 好きなあまり (好き is ADJ with 形状詞-一般)
    // "な" is AUX with lemma=だ and inflectionForm=連体形-一般
    (b) => {
      const naAdj = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
        tagOneOf: [
          '形状詞-一般',
          '名詞-普通名詞-一般',
          '名詞-普通名詞-形状詞可能',
        ],
      }, 'naAdj');
      const na = b.aux({
        lemma: 'だ',
        text: 'な',
        inflectionForm: '連体形-一般',
      }, 'na');
      const amari = b.tok({
        posOneOf: ['NOUN', 'ADV'],
        lemmaOneOf: ['あまり', '余り'],
      }, 'amari');
      b.inOrder(naAdj, na, 1);
      b.inOrder(na, amari, 1);
      b.captureSpan('あまり', naAdj, amari);
    }
  );
});
