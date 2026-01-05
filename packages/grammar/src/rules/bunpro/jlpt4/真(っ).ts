import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: 真(っ) (ま, まっ, まん) - Genuine/Real/True/Completely
 *
 * Matches the prefix 真 (ま/まっ/まん) meaning "genuine", "real", "true", or "completely".
 * This prefix emphasizes the intensity or authenticity of the following word.
 *
 * The form changes based on sound changes (gemination and nasal sound):
 * - ま (ma) - basic form before certain sounds
 * - まっ (makkotsu) - with gemination (little tsu) before ka/sa/ta columns
 * - まん (mann) - with 'n' before ma/na columns
 *
 * Common compounds:
 * - まっ赤 (makkaka) - bright red, completely red
 * - まっ暗 (makkura) - pitch dark
 * - まっ黒 (makkuro) - pitch black
 * - まっ青 (massao) - deep blue, pale
 * - まっ白 (masshiro) - pure white
 * - まっ直ぐ (massugu) - straight
 * - まっ先 (massaki) - the very first
 * - まっ最中 (massaichuu) - right in the middle of
 * - まっ二つ (mapputsu) - exactly in two
 * - まっ逆さま (makkasakama) - completely upside down
 * - まっ昼間 (mappiruma) - middle of the day
 * - まっ裸 (mappadaka) - completely naked
 * - まんなか (mannaka) - middle/center
 * - まん丸 (mannmaru) - perfectly round
 * - まん中 (mannnaka) - middle/center
 *
 * Key discriminators:
 * - POS is ADV (adverb) for most compounds
 * - OR forms a compound with NOUN/ADJ
 * - lemma usually contains the full compound
 *
 * Note: This is a prefix that combines with various words to create
 * intensifying expressions meaning "completely", "truly", or "exactly".
 *
 * This rule matches tokens that:
 * 1. Start with まっ or まん (the prefix forms)
 * 2. Have specific lemmas (dictionary forms) of the compounds
 * 3. Are adverbs or nouns
 */
export default bunproLinguisticRule('真(っ)', (r) => {
  // Match various forms of the 真 prefix compounds
  // We match by lemma since GiNZA may use different surface forms (kanji vs kana)
  const maPrefix = r.tok({
    lemmaOneOf: [
      // まっ form (with gemination) - dictionary forms
      'まっか',      // 真っ赤 - bright red
      'まっくら',    // 真っ暗 - pitch dark
      'まっくろ',    // 真っ黒 - pitch black
      'まっさお',    // 真っ青 - deep blue/pale
      'まっしろ',    // 真っ白 - pure white
      'まっすぐ',    // 真っ直ぐ - straight
      'まっさき',    // 真っ先 - very first
      'まっさいちゅう', // 真っ最中 - right in middle
      'まっぷたつ',  // 真っ二つ - exactly in two
      'まっさかさま', // 真っ逆さま - upside down
      'まっぴるま',  // 真っ昼間 - midday
      'まっぱだか',  // 真っ裸 - completely naked
      // まん form (with nasal)
      'まんなか',    // 真ん中 - middle/center
      'まんまる',    // 真ん丸 - perfectly round
      'まんまえ',    // 真ん前 - right in front
    ],
  }, 'maPrefix');

  // Capture the prefix compound
  r.capture(maPrefix);
});
