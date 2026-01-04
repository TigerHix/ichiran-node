import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: あとで - After doing something / Later
 *
 * Matches patterns where あとで (atode) is used to express "after" or "later".
 *
 * Structures:
 * - Verb［た］+ あとで (after doing X)
 * - Noun + の + あとで (after N)
 * - あとで (later, at the beginning of a sentence)
 *
 * Examples:
 * - 勉強したあとで、遊んでもいいよ (You can play after studying)
 * - 仕事の後で、飲み会に行きます (I will go to the party after work)
 * - あとで部屋の掃除をしてください (Please clean your room later)
 * - 食事のあとで、お皿を洗わなくてはいけない (I have to wash dishes after a meal)
 *
 * Key discriminators:
 * - で particle must have dep=case (case marker), not dep=compound
 * - This avoids matching standalone 後 as a noun meaning "behind/remainder"
 *
 * GiNZA parse structure:
 * - 勉強したあとで: 勉強(verb) + し(aux) + た(aux) + あと(noun) + で(particle,dep=case)
 * - 仕事の後で: 仕事(noun) + の(particle) + 後(noun) + で(particle,dep=case)
 * - あとで買い物: あと(noun) + で(particle,dep=case) + 買い物(noun)
 */
export default linguisticRule('あとで', (r) => {
  r.either(
    // Branch 1: Verb-たform + あとで (after doing X)
    (b) => {
      const verb = b.verb({}, 'verb');
      const ta = b.aux({
        lemmaOneOf: ['た', 'だ'],
        conjugationClass: '助動詞-タ',
      }, 'ta');
      b.auxOf(verb, ta);

      // Followed by あと (noun, can be written as あと or 後)
      const ato = b.noun({ lemmaOneOf: ['あと', '後'] }, 'ato');
      b.inOrder(ta, ato, 3);

      // Followed by case marker で
      const de = b.particle('で', 'de', { dep: 'case' });
      b.inOrder(ato, de, 1);

      b.captureSpan('あとで', verb, de);
    },
    // Branch 2: Noun + の + あとで (after N)
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.caseMarker(noun, no);

      // Followed by あと (noun, can be written as あと or 後)
      const ato = b.noun({ lemmaOneOf: ['あと', '後'] }, 'ato');
      b.inOrder(no, ato, 1);

      // Followed by case marker で
      const de = b.particle('で', 'de', { dep: 'case' });
      b.inOrder(ato, de, 1);

      b.captureSpan('あとで', noun, de);
    },
    // Branch 3: Standalone あとで (later, at beginning)
    (b) => {
      // Just あと + で as a temporal adverbial
      const ato = b.noun({ lemmaOneOf: ['あと', '後'] }, 'ato');
      const de = b.particle('で', 'de', { dep: 'case' });
      b.inOrder(ato, de, 1);

      b.captureSpan('あとで', ato, de);
    }
  );
});
