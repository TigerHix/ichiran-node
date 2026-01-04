import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('い-adjective-predicate', (r) => {
  // い-adjectives used as sentence predicates
  // Unlike な-adjectives, い-adjectives can end sentences directly
  // They can also take です for politeness

  r.either(
    // Pattern 1: い-adjective at end of sentence (plain form)
    // e.g., 暑い, かわいい, 楽しい
    (r1) => {
      const adj = r1.tok({
        lemmaOneOf: [
          'さむい', 'あつい', 'たのしい', 'おいしい', 'かわいい',
          'ちかい', 'こわい', 'おとなしい', 'いそがしい', 'むずかしい',
          'おもしろい', 'まるい',
          // Kanji variants
          '熱い', '丸い',
        ],
        tag: '形容詞-一般',
        dep: 'root',
      }, 'adj');
      r1.capture(adj);
    },
    // Pattern 2: い-adjective + です (polite form)
    // e.g., 暑いです, かわいいです, 楽しいです
    (r2) => {
      const adj = r2.adj({
        lemmaOneOf: [
          'さむい', 'あつい', 'たのしい', 'おいしい', 'かわいい',
          'ちかい', 'こわい', 'おとなしい', 'いそがしい', 'むずかしい',
          'おもしろい', 'まるい',
          // Kanji variants
          '熱い', '丸い',
        ],
        tag: '形容詞-一般',
      }, 'adj');
      const desu = r2.aux({
        text: 'です',
        dep: 'root',
      }, 'desu');
      r2.auxOf(adj, desu);
      r2.captureSpan('い-adjective-predicate', adj, desu);
    }
  );
});
