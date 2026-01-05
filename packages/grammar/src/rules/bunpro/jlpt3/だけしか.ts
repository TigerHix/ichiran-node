import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('だけしか', (r) => {
  // だけしか (dake + shika) - emphatic "only/nothing but"
  // Pattern: Noun + だけ + しか + negative verb
  // This is a double particle construction for strong emphasis
  // Must be followed by negative (ない, ません, etc.)
  //
  // GiNZA parsing notes:
  // - だけ is ADP (助詞-副助詞) with dep=case
  // - しか is ADP (助詞-副助詞) with dep=case
  // - Both attach to the noun
  // - Followed by negative auxiliary (ない, ません, なかった, etc.)
  //
  // Examples:
  // - 医師は一人だけしかいません (Only one doctor is present)
  // - 野菜だけしか食べられません (Can only eat vegetables)
  // - 日本語だけしか話せない (Can only speak Japanese)
  // - 100円だけしかありません (Only have 100 yen)
  //
  // NOTE: We don't explicitly match the negative verb because:
  // 1. GiNZA parses negatives in various ways (いません, ない, ません, etc.)
  // 2. The grammar itself requires negative - overcapture would match sentences with positive verbs
  // 3. We rely on the structural pattern Noun+だけ+しか as the primary discriminator
  //
  // Negative examples prevent matching:
  // - だけ alone (without しか) - different grammar
  // - しか alone (without だけ) - different grammar (JLPT4)
  // - だけ + positive verb - ungrammatical with だけしか
  // - だけで - "just by" (different grammar)

  const noun = r.tok({
    posOneOf: ['NOUN', 'PROPN', 'NUM', 'PRON', 'ADV'],
  }, 'noun');
  const dake = r.particle('だけ', 'dake');
  const shika = r.particle('しか', 'shika');

  // Both particles mark the noun (case marker relationship)
  r.caseMarker(noun, dake);
  r.caseMarker(noun, shika);

  // Strict ordering: noun, then だけ, then しか (adjacent)
  r.inOrder(noun, dake, 1);
  r.inOrder(dake, shika, 1);

  // Capture from noun to しか
  r.captureSpan('だけしか', noun, shika);
});
