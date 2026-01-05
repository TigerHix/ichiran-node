import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: るところだ - About to do / On the verge of doing
 *
 * Matches patterns where a verb in dictionary form is followed by ところ(だ/です)
 * to express "about to do" or "on the verge of doing" something.
 *
 * Structures:
 * - Verb［dictionary form］+ ところ + だ/です (about to do X)
 *
 * Examples:
 * - 今から帰るところです (I'm just about to go home.)
 * - 出かけるところだ (I'm about to go out.)
 * - 友達と会うところです (I'm about to meet my friend.)
 * - 食べるところで、焦げた匂いがした (Just as I was about to eat, I smelled something burning.)
 *
 * Key discriminators:
 * - Must be verb in dictionary form (連体形-一般) + ところ (not other uses of ところ)
 * - Different from たところだ (just finished) - this uses dictionary form
 * - Different from ているところだ (in the middle of) - this uses te-form + いる
 * - GiNZA parses verb as 連体形-一般 (dictionary/attributive form)
 * - GiNZA parses ところ as NOUN with lemma=ところ
 * - Copula (だ/です) is optional in sentence-final position
 *
 * GiNZA parse structure:
 * - 帰る場所: 帰る(verb,inflectionForm=連体形-一般) + ところ(noun,lemma=ところ)
 * - 出かける場所だ: 出かける(verb,inflectionForm=連体形-一般) + ところ(noun) + だ(aux,dep=cop)
 * - 会う場所です: 会う(verb,inflectionForm=連体形-一般) + ところ(noun) + です(aux,dep=cop)
 */
export default bunproLinguisticRule('るところだ', (r) => {
  // Verb in dictionary form (連体形-一般)
  // Key discriminators:
  // - Must have dep=acl or dep=advcl (not dep=fixed which indicates ている pattern)
  // - In ているところだ, the verb いる has dep=fixed pointing to て
  // - Compound verbs like 習い始める have the second verb with dep=advcl
  const verb = r.verb({
    inflectionForm: '連体形-一般',
    depOneOf: ['acl', 'advcl'],
  }, 'verb');

  // Followed by ところ (GiNZA consistently parses as NOUN)
  const tokoro = r.tok({
    lemma: 'ところ',
    pos: 'NOUN',
  }, 'tokoro');
  r.inOrder(verb, tokoro, 3);

  // Followed by optional copula (だ/です)
  r.optional((ob) => {
    const copula = ob.aux({
      lemmaOneOf: ['だ', 'です'],
      dep: 'cop',
    }, 'copula');
    ob.inOrder(tokoro, copula, 2);
  });

  r.captureSpan('るところだ', verb, tokoro);
});
