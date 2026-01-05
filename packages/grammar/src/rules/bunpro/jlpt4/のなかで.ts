import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: のなかで - Among/Within/In (a group)
 *
 * Matches patterns where のなかで (nonakade) is used to express "among", "within", or "in"
 * when selecting one or more items from a group.
 *
 * Structures:
 * - Noun + の + 中で (among N)
 * - Noun + の + なかで (among N)
 * - この/その/あの + 中で (among this/that)
 * - この/その/あの + なかで (among this/that)
 * - Standalone 中で/なかで (within/inside)
 *
 * Examples:
 * - フルーツの中で、イチゴが好き (Among fruits, I like strawberries)
 * - この三つの中では、赤色が好き (Among these three, I like red)
 * - 公園の中で子供達が遊んでいた (Children were playing in the park)
 * - 電車の中で大きい声で喋らないでください (Please don't speak loudly in the train)
 * - この中で一つを選んでください (Please choose one from among these)
 *
 * Key discriminators:
 * - 中/なか must be a NOUN
 * - で particle must have dep=case (case marker for location/scope)
 * - This avoids matching standalone 中 as just "middle" without scope
 *
 * GiNZA parse structure:
 * - フルーツの中で: フルーツ(noun) + の(particle) + 中(noun) + で(particle,dep=case)
 * - この中で: この(det) + 中(noun) + で(particle,dep=case)
 * - 中で alone: 中(noun) + で(particle,dep=case)
 */
export default bunproLinguisticRule('のなかで', (r) => {
  r.either(
    // Branch 1: Noun + の + 中/なかで (among N, within N)
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.caseMarker(noun, no);

      // Followed by 中 or なか (noun - inside/middle/among)
      const naka = b.noun({ lemmaOneOf: ['中', 'なか'] }, 'naka');
      b.inOrder(no, naka, 1);

      // Followed by case marker で (location/scope)
      const de = b.particle('で', 'de', { dep: 'case' });
      b.inOrder(naka, de, 1);

      b.captureSpan('のなかで', noun, de);
    },
    // Branch 2: この/その/あの + 中/なかで (among this/that)
    (b) => {
      const kono = b.tok({
        textOneOf: ['この', 'その', 'あの'],
        pos: 'DET'
      }, 'kono');

      // Followed by 中 or なか (noun - inside/middle/among)
      const naka = b.noun({ lemmaOneOf: ['中', 'なか'] }, 'naka');
      b.inOrder(kono, naka, 1);

      // Followed by case marker で (location/scope)
      const de = b.particle('で', 'de', { dep: 'case' });
      b.inOrder(naka, de, 1);

      b.captureSpan('のなかで', kono, de);
    },
    // Branch 3: Standalone 中/なかで (within, inside, in)
    (b) => {
      // Just 中/なか + で as locative/scope
      const naka = b.noun({ lemmaOneOf: ['中', 'なか'] }, 'naka');
      const de = b.particle('で', 'de', { dep: 'case' });
      b.inOrder(naka, de, 1);

      b.captureSpan('のなかで', naka, de);
    }
  );
});
