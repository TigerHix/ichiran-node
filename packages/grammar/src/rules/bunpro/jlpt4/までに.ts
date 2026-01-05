import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: までに (by the time/until)
 *
 * Matches patterns where までに is used to express "by (a certain time)" or "before".
 * This indicates a deadline rather than duration.
 *
 * Structures:
 * - Time + までに (by [time])
 * - Noun + までに (by [noun/time])
 * - Verb + までに (by the time [verb happens])
 *
 * Examples:
 * - １０時までに帰ってくる (come back by 10 o'clock)
 * - 明日までに電話をください (please call by tomorrow)
 * - 冬が終わるまでにスキーをしたい (want to ski by the time winter ends)
 * - 遊びに行くまでに片付けてね (tidy up by the time you go out to play)
 * - 締め切りまでに出す (submit by the deadline)
 *
 * Key discriminators:
 * - Must have まで (made) followed by に (ni) particle
 * - に must be a case marker (dep=case)
 * - Different from まで which means "until" (duration) rather than "by" (deadline)
 *
 * GiNZA parse structure:
 * - １０時までに: １０(NUM) + 時(NOUN) + まて(AUX/ADP) + に(ADP,dep=case)
 * - 冬が終わるまでに: 冬(NOUN) + が(AUX) + 終わる(VERB) + る(AUX) + まで(ADP) + に(ADP,dep=case)
 *
 * Contrast with まで (duration):
 * - 来月までレポートを書く (write report until next month - continuous action)
 * - 来月までにレポートを書く (write report by next month - deadline)
 */
export default bunproLinguisticRule('までに', (r) => {
  r.either(
    // Branch 1: Noun/Time + まで + に
    (b) => {
      const made = b.tok({ textOneOf: ['まで', '迄'] }, 'made');
      const ni = b.particle('に', 'ni', { dep: 'case' });
      b.inOrder(made, ni, 1);

      // Capture the までに span
      b.captureSpan('までに', made, ni);
    },
    // Branch 2: Verb + まで + に (by the time [verb])
    (b) => {
      const verb = b.verb({}, 'verb');
      const made = b.tok({ textOneOf: ['まで', '迄'] }, 'made');
      const ni = b.particle('に', 'ni', { dep: 'case' });

      b.inOrder(verb, made, 5);
      b.inOrder(made, ni, 1);

      // Capture from verb to に
      b.captureSpan('までに', verb, ni);
    }
  );
});
