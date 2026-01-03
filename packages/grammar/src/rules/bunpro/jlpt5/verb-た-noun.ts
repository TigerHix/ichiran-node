import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: verb-た-noun - Verb + た + Noun (Noun modified by verb)
 *
 * Matches a verb in past tense (た form) or continuous form (ている/でいる)
 * followed by a noun. The verb clause acts as a relative clause modifying
 * the noun.
 *
 * Examples:
 * - 食べたハンバーガー (the hamburger that [someone] ate)
 * - 寝ている犬 (a dog that is sleeping)
 * - 洗った服 (washed clothes)
 * - 住んでいる外国人 (a foreigner living in Japan)
 *
 * The verb must be in plain form (not polite ました/ています form).
 * Per Bunpro: "Only verbs in short (plain) form can modify nouns."
 *
 * GiNZA parse structure:
 * - 食べたハンバーガー: 食べ(verb,dep=acl→ハンバーガー) + た(aux,dep=aux→食べ)
 * - 寝ている犬: 寝(verb,dep=acl→犬) + て(mark,dep=mark→寝) + いる(verb,dep=fixed→て)
 * - 住んでいる外国人: すん(verb,dep=acl→外国人) + で(mark,dep=mark→すん) + いる(verb,dep=fixed→で)
 *
 * Note: The continuous form uses て or で (voiced form after certain sounds).
 */
export default linguisticRule('verb-た-noun', (r) => {
  r.either(
    // Branch 1: Verb + た (past tense) + Noun
    (b) => {
      // Match any verb followed by past tense auxiliary た/だ
      const verb = b.verb({}, 'verb');
      const ta = b.aux({
        lemmaOneOf: ['た', 'だ'],
        conjugationClass: '助動詞-タ',
      }, 'ta');
      b.auxOf(verb, ta);

      // Followed by a noun (NOUN, PROPN, or PRON)
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');

      // The verb stem has dep=acl pointing to the noun (adnominal clause)
      b.headChild(noun, verb, 'acl');

      // Capture from verb to noun
      b.captureSpan('match', verb, noun);
    },
    // Branch 2: Verb + て/で + いる (continuous form) + Noun
    (b) => {
      // Match any verb with て/で form
      const verb = b.verb({}, 'verb');
      const te = b.tok({
        lemmaOneOf: ['て', 'で'],
        pos: 'SCONJ',
        tag: '助詞-接続助詞',
      }, 'te');
      b.headChild(verb, te, 'mark');

      // Followed by いる (verb, not auxiliary in this context)
      const iru = b.tok({
        lemma: 'いる',
        pos: 'VERB',
        tag: '動詞-非自立可能',
      }, 'iru');
      b.headChild(te, iru, 'fixed');

      // Followed by a noun (NOUN, PROPN, or PRON)
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');

      // The verb stem has dep=acl pointing to the noun (adnominal clause)
      b.headChild(noun, verb, 'acl');

      // Capture from verb to noun
      b.captureSpan('match', verb, noun);
    }
  );
});
