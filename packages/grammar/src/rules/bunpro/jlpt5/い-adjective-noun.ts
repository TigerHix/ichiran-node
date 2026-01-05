import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('い-adjective-noun', (r) => {
  // Match い-adjective directly modifying a noun
  // い-adjectives can directly modify nouns without any particle
  // e.g., "あついひと" (hot person), "おおきいいえ" (big house)
  //
  // Examples from Bunpro:
  // - おいしいピザを食べる。 (tasty pizza)
  // - あたらしいスマホを買う。 (new smartphone)
  // - むずかしい質問をする。 (difficult question)
  // - ちいさい猫はかわいい。 (small cats)
  //
  // Grammar structure:
  // - Adjective immediately precedes the noun it modifies
  // - Adjective must be an i-adjective (pos=ADJ)
  // - Noun follows the adjective (any NOUN token)
  //
  // Note: GiNZA may parse some adjectives inconsistently, so we use
  // r.either() to handle different matching strategies

  r.either(
    // Branch 1: Match by lemma (most cases)
    (branch) => {
      const adj = branch.tok({
        lemmaOneOf: [
          // Hiragana forms
          'さむい', 'あつい', 'たのしい', 'たかい',
          'おいしい', 'かわいい', 'おおきい', 'あたたかい',
          'ちいさい', 'ちかい', 'ふるい', 'あたらしい',
          'むずかしい', 'やすい', 'とおい', 'ながい',
          'はやい', 'こわい', 'おもしろい', 'やさしい',
          'つめたい', 'まずい', 'うつくしい', 'かっこいい',
          // Kanji forms (GiNZA may use these as lemmas)
          '寒い', '暑い', '楽しい', '高い',
          '美味しい', '可愛い', '大きい', '温かい',
          '小さい', '近い', '古い', '新しい',
          '難しい', '安い', '遠い', '長い',
          '早い', '速い', '怖い', '面白い', '優しい',
          '冷たい', '不味い', '美しい',
          // Special forms
          'かっこういい', '格好いい', // Full form of かっこいい
        ],
        posOneOf: ['ADJ', 'NOUN', 'VERB'], // Handle GiNZA inconsistencies
      }, 'adj');
      const noun = branch.noun({}, 'noun');
      branch.inOrder(adj, noun, 1);
      branch.captureSpan('い-adjective-noun', adj, noun);
    },
    // Branch 2: Match by text for problematic words (fallback, no POS constraint)
    (branch) => {
      const adj = branch.tok({
        textOneOf: ['さむい', '寒い'],
      }, 'adj');
      const noun = branch.noun({}, 'noun');
      branch.inOrder(adj, noun, 1);
      branch.captureSpan('い-adjective-noun', adj, noun);
    }
  );
});
