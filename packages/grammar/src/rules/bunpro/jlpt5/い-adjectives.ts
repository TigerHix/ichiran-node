import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('い-adjectives', (r) => {
  const adj = r.tok({
    lemmaOneOf: [
      'さむい', 'あつい', 'たのしい', 'たかい',
      'おいしい', 'かわいい', 'おおきい', 'あたたかい',
      'ちかい', 'ふるい', 'あたらしい', 'むずかしい',
      'やすい', 'とおい', 'ながい', 'はやい',
      'こわい', 'おもしろい', 'おとなしい',
      'つめたい', 'まずい', 'うつくしい', 'い',
      'うつくしい', 'せまい', '温い', '狭い',
    ],
    tag: '形容詞-一般',
  }, 'adj');
  r.capture(adj);
});
