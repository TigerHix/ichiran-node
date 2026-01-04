import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: および (及び) - Formal conjunction "and, as well as"
 *
 * Matches the formal conjunction および used to connect nouns in written
 * and formal contexts. Similar to と but much more formal, used primarily
 * in documents, articles, and notifications.
 *
 * Structures:
 * - Noun + および + Noun
 *
 * Examples:
 * - 東京および大阪に行く (Going to Tokyo and Osaka)
 * - 英語および日本語を話す (Speaking English and Japanese)
 * - 免許証および印鑑を持ってきてください (Please bring your license and seal)
 * - 夕食および朝食は別料金になります (Dinner and breakfast are charged separately)
 *
 * Key discriminators:
 * - および is a formal conjunction (pos=CCONJ)
 * - Has dep=cc (coordinating conjunction)
 * - Connects nouns in formal/written contexts
 * - Distinguished from verb 及ぶ (oyobu - to reach), which has pos=VERB
 *
 * GiNZA parse structure:
 * - POSITIVE: 東京および大阪に行く
 *   - 東京(NOUN) + および(CCONJ, lemma=および, dep=cc) + 大阪(NOUN)
 * - POSITIVE: 英語および日本語を話す
 *   - 英語(NOUN) + および(CCONJ, lemma=および, dep=cc) + 日本語(NOUN)
 * - NEGATIVE (verb): 騒ぎが庭及び道路に及ぶ (Noise reaches the garden and road)
 *   - 庭(NOUN) + 道路(NOUN) + に(ADP) + 及ぶ(VERB) - verb form, not conjunction
 */
export default linguisticRule('および', (r) => {
  const oyobi = r.tok({
    textOneOf: ['および', '及び'],
    pos: 'CCONJ'
  }, 'oyobi');

  r.capture(oyobi);
});
