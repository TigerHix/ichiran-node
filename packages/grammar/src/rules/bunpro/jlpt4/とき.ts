import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: とき (時) - When / At the time of
 *
 * Matches patterns where とき is used to express "when" or "at the time of".
 *
 * Structures:
 * - Verb + とき (when [verb])
 * - Verb + ときに (when [verb], emphasizing the timing)
 * - い-adj + とき (when [adj])
 * - な-adj + なとき (when [na-adj])
 * - Noun + のとき (when [noun])
 * - Noun + のときに (when [noun], emphasizing the timing)
 *
 * Examples:
 * - 散歩をするときに音楽を聴く (I listen to music when I take a walk)
 * - あの映画を見たとき、泣いた (When I watched that movie, I cried)
 * - 寒いときは、お風呂に入りたくなる (When it's cold, I want to take a bath)
 * - 大変なときに彼の親は亡くなった (His parents died when he was going through a difficult time)
 * - 授業のときは静かにしなくてはいけない (When you are having class, you have to be quiet)
 * - 雨のときは家でゴロゴロしています (When it rains, I laze around in my house)
 *
 * Key discriminators:
 * - とき is a noun meaning "time/occasion"
 * - Can be preceded by verbs, adjectives, or noun+の
 * - Can be followed by に (ni) or は (wa) particles
 * - Different from ごろ (around/approximate) and あいだ (while/during)
 *
 * GiNZA parse structure:
 * - 散歩をするとき: 散歩(noun) + を(particle) + する(verb) + とき(noun)
 * - 寒いとき: 寒い(adj) + とき(noun)
 * - 大変なとき: 大変(adj) + な(aux) + とき(noun)
 * - 授業のとき: 授業(noun) + の(particle) + とき(noun)
 */
export default bunproLinguisticRule('とき', (r) => {
  r.either(
    // Branch 1: Plain とき (hiragana form)
    (b) => {
      const toki = b.tok({ lemma: 'とき', text: 'とき' }, 'toki');
      b.captureAs('とき', toki);
    },
    // Branch 2: 時 (kanji form - same lemma)
    (b) => {
      const tokiKanji = b.tok({ lemma: 'とき', text: '時' }, 'tokiKanji');
      b.captureAs('とき', tokiKanji);
    }
  );
});
