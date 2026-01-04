import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: たものだ - used to / naturally did
 *
 * Matches verb［た］+ ものだ/ものです (past recollection)
 *
 * Structure:
 * - Verb［past formた］+ ものだ (casual)
 * - Verb［past formた］+ ものです (polite)
 *
 * Expresses recollection of past habits or natural outcomes.
 * Often carries a nostalgic tone when reminiscing about the past.
 *
 * Examples:
 * - 子供の頃はよく海に泳ぎに行ったものだ (I used to go swimming in the ocean a lot when I was a child)
 * - 昔は井戸から水をくんだものだ (People used to take water from wells)
 * - 子供の頃はよく先生に怒られたものだ (I used to get scolded by the teacher a lot when I was a kid)
 * - 若い頃はいろんな苦労をしたものだ (I used to have lots of hardships when I was young)
 *
 * GiNZA parse structure:
 * - いったものだ: いく(verb) + た(aux) + もの(noun) + だ(aux)
 * - 怒られたものだ: 怒る(verb) + れる(aux) + た(aux) + もの(noun) + だ(aux)
 * - です in ものです is AUX with lemma=だ
 *
 * Negative examples to avoid:
 * - たことがある (different grammar: have done before)
 * - たことだ (different grammar: fact/states that)
 * - Simple ものだ without past tense (different grammar)
 */
export default linguisticRule('たものだ', (r) => {
  r.either(
    // Pattern 1: Verb［た］+ ものだ (casual)
    // 過去の verb + た (AUX) + もの (NOUN) + だ (AUX)
    //
    // IMPORTANT: The past tense auxiliary can be either "た" or "だ" depending on verb type:
    // - Ichidan/irregular verbs: 行った (た with lemma=た)
    // - Godan verbs: くんだ, あそんだ, よんだ (だ with lemma=だ, appears as contracted form)
    // - This is because godan verbs contract with た: む+た=んだ, ぶ+た=んだ, etc.
    //
    // IMPORTANT: Verbs in 連用形 (conjunctive form) before past aux are sometimes tagged as ADJ in GiNZA
    (b) => {
      // Match any verb/aux in past form followed by past auxiliary
      // Note: Verbs in 連用形 can be tagged as VERB, AUX, or ADJ in GiNZA
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ'],
      }, 'verb');

      // The past auxiliary can be "た" (ichidan/irregular) or "だ" (godan contracted)
      // Both serve as past tense markers
      const pastAux = b.aux({
        lemmaOneOf: ['た', 'だ'],
      }, 'pastAux');
      b.inOrder(verb, pastAux, 1);

      const mono = b.noun({ lemma: 'もの' }, 'mono');
      b.inOrder(pastAux, mono, 1);

      const da = b.aux({ lemma: 'だ' }, 'da');
      b.inOrder(mono, da, 1);

      b.captureSpan('たものだ', verb, da);
    },

    // Pattern 2: Verb［た］+ ものです (polite)
    // 過去の verb + た (AUX) + もの (NOUN) + です (AUX with lemma=だ)
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ'],
      }, 'verb');

      const pastAux = b.aux({
        lemmaOneOf: ['た', 'だ'],
      }, 'pastAux');
      b.inOrder(verb, pastAux, 1);

      const mono = b.noun({ lemma: 'もの' }, 'mono');
      b.inOrder(pastAux, mono, 1);

      // です is AUX with lemma=だ and text=です
      const desu = b.aux({
        lemma: 'だ',
        text: 'です',
      }, 'desu');
      b.inOrder(mono, desu, 1);

      b.captureSpan('たものだ', verb, desu);
    }
  );
});
