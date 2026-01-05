import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('いわゆる', (r) => {
  // いわゆる (so-called, what is called, generally known)
  // Pre-nominal adjectival verb (連体詞 - pre-noun adjective)
  // Can also appear as 所謂 (kanji form)
  // Structure: いわゆる + Noun
  // Examples:
  // - それはいわゆる「選択」とは何か。
  // - それはいわゆるloveだ。
  // - ハンバーガーやフライドポテトはいわゆるジャンクフードだ。
  // - いわゆる送料ですが、無料でやらせてもらっています。

  const iwayuru = r.tok({
    lemmaOneOf: ['いわゆる', '所謂'],
  }, 'iwayuru');

  // いわゆる must be followed by a noun (within 5 tokens)
  // Use either() to allow different noun types
  const noun = r.tok({
    posOneOf: ['NOUN', 'PROPN', 'PRON'],
  }, 'noun');

  r.inOrder(iwayuru, noun, 5);
  r.capture(iwayuru);
});
