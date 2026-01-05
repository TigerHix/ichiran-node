import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('がん-totally', (r) => {
  // がん (gan) - slang prefix meaning "totally/completely"
  // Pattern: ガン/がん + verb stem + optional auxiliaries
  // Examples:
  // - ガン見する (to stare intently/majorly)
  // - ガン無視された (was completely ignored)
  // - ガン切れされた (was furiously snapped at)
  // - ガン詰め (to press/hound intensely)
  // - ガン寝 (to sleep deeply/conk out)
  // - ガン萎え (to be totally bummed/deflated)

  const gan = r.tok({
    lemmaOneOf: ['ガン', 'がん'],
    textOneOf: ['ガン', 'がん'],
    posOneOf: ['NOUN', 'VERB', 'ADV', 'ADJ', 'PART'],
  }, 'gan');

  // The verb stem (masu-stem or noun stem)
  // Can be various verb forms: 見, 無視, 切れ, 詰め, 寝, 萎え, etc.
  // GiNZA parses these in various POS categories
  // Use either() to handle different POS cases while excluding particles
  r.either(
    // Non-particle tokens (NOUN, VERB, ADV, ADJ, PART)
    (b) => {
      const verbStem = b.tok({
        posOneOf: ['NOUN', 'VERB', 'ADV', 'ADJ', 'PART'],
      }, 'verbStem');
      b.inOrder(gan, verbStem, 1);
      b.captureSpan('がん-totally', gan, verbStem);
    },
    // ADP tokens that are NOT particles (exclude case markers)
    (b) => {
      const verbStem = b.tok({
        pos: 'ADP',
        // Exclude common particles that follow がん in "cancer" usage
        // The actual verb stems in がん-X compounds have noun tags
        tagOneOf: ['名詞-普通名詞-助数詞可能', '名詞-普通名詞-一般', '名詞-普通名詞-サ変可能'],
      }, 'verbStem');
      b.inOrder(gan, verbStem, 1);
      b.captureSpan('がん-totally', gan, verbStem);
    }
  );
});
