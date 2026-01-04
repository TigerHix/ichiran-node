import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: たことがある - Have done before (past experience)
 *
 * Matches verb in past tense (た-form) + こと + が + ある/あります (experience)
 * Also matches negative form: verb in past tense + こと + が + ない/ありません
 *
 * Structure:
 * - Verb［た］+ こと + が + ある/あります (casual/polite affirmative)
 * - Verb［た］+ こと + が + ない/ありません (casual/polite negative)
 *
 * Examples:
 * - 日本に行ったことがある (I have been to Japan)
 * - この本を読んだことがあります (I have read this book)
 * - 沖縄に住んだことがない (I have never lived in Okinawa)
 * - ６時前に起きたことがありません (I have never woken up before 6)
 * - 彼女に「早く帰ってください」といったことがあります (I have said to her before...)
 *
 * GiNZA parse structure:
 * - 行ったことがある: 行(verb) + た(aux,dep=aux→行) + こと(noun) + が(particle) + ある(verb)
 * - いったことがあります: いっ(verb,dep=fixed→と) + た(aux,dep=fixed→と) + こと(noun) + が(particle) + あります(verb+aux)
 *
 * Note: The structure of quotative patterns (といった) is complex:
 * - In quotative constructions, both the verb (いう) and the auxiliary (た) may point to
 *   the quotative particle (と) rather than each other. We rely on surface order rather
 *   than dependency structure in these cases.
 */
export default linguisticRule('たことがある', (r) => {
  r.either(
    // Branch 1: Casual affirmative (ある)
    (b) => {
      // Verb (in some conjugated form, often ren'youkei before ta)
      const verb = b.verb({}, 'verb');

      // Followed by ta (past tense auxiliary)
      // Note: maxDistance=2 to allow for intermediate auxiliaries (e.g., 結婚し+た)
      const ta = b.aux({
        lemmaOneOf: ['た', 'だ'],
        conjugationClass: '助動詞-タ',
      }, 'ta');
      b.inOrder(verb, ta, 2);

      // Followed by こと (nominalizer)
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(ta, koto, 1);

      // Followed by particle が
      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      // Followed by ある (existence verb)
      const aru = b.tok({ lemma: 'ある', pos: 'VERB' }, 'aru');
      b.inOrder(ga, aru, 1);

      b.captureSpan('たことがある', verb, aru);
    },
    // Branch 2: Polite affirmative (あります)
    (b) => {
      // Verb (in some conjugated form)
      const verb = b.verb({}, 'verb');

      // Followed by ta (past tense auxiliary)
      // Note: maxDistance=2 to allow for intermediate auxiliaries (e.g., 結婚し+た)
      const ta = b.aux({
        lemmaOneOf: ['た', 'だ'],
        conjugationClass: '助動詞-タ',
      }, 'ta');
      b.inOrder(verb, ta, 2);

      // Followed by こと (nominalizer)
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(ta, koto, 1);

      // Followed by particle が
      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      // Followed by あります (polite form of ある)
      const aru = b.tok({ lemma: 'ある', pos: 'VERB', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.tok({ lemma: 'ます', pos: 'AUX' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(ga, aru, 1);

      b.captureSpan('たことがある', verb, masu);
    },
    // Branch 3: Casual negative (ない)
    (b) => {
      // Verb (in some conjugated form)
      const verb = b.verb({}, 'verb');

      // Followed by ta (past tense auxiliary)
      // Note: maxDistance=2 to allow for intermediate auxiliaries (e.g., 結婚し+た)
      const ta = b.aux({
        lemmaOneOf: ['た', 'だ'],
        conjugationClass: '助動詞-タ',
      }, 'ta');
      b.inOrder(verb, ta, 2);

      // Followed by こと (nominalizer)
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(ta, koto, 1);

      // Followed by particle が
      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      // Followed by ない (negative, parsed as ADJ in GiNZA)
      const nai = b.tok({ lemma: 'ない', pos: 'ADJ' }, 'nai');
      b.inOrder(ga, nai, 1);

      b.captureSpan('たことがある', verb, nai);
    },
    // Branch 4: Polite negative (ありません)
    (b) => {
      // Verb (in some conjugated form)
      const verb = b.verb({}, 'verb');

      // Followed by ta (past tense auxiliary)
      // Note: maxDistance=2 to allow for intermediate auxiliaries (e.g., 結婚し+た)
      const ta = b.aux({
        lemmaOneOf: ['た', 'だ'],
        conjugationClass: '助動詞-タ',
      }, 'ta');
      b.inOrder(verb, ta, 2);

      // Followed by こと (nominalizer)
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(ta, koto, 1);

      // Followed by particle が
      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      // Followed by ありません (polite negative)
      const aru = b.tok({ lemma: 'ある', pos: 'VERB', inflectionForm: '未然形-一般' }, 'aru');
      const masen = b.tok({ lemma: 'ません', pos: 'AUX' }, 'masen');
      b.auxOf(aru, masen);
      b.inOrder(ga, aru, 1);

      b.captureSpan('たことがある', verb, masen);
    }
  );
});
