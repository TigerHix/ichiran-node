import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('どの', (r) => {
  // どの is an interrogative pre-nominal adjective (連体詞) meaning "which"
  // It must be followed by a noun (DET + NOUN with dep=det)
  //
  // Examples from Bunpro:
  // - どのスポーツが好きですか。
  // - どのパソコンがいいですか。
  // - どのレストランに行く？

  const dono = r.tok({ lemma: 'どの', pos: 'DET' }, 'dono');
  const noun = r.noun({}, 'noun');
  r.headChild(noun, dono, 'det');
  r.capture(dono);
});
