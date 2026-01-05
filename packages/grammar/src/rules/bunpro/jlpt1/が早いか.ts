import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: が早いか (gahayaika) - As soon as, The moment, No sooner than
 *
 * A formal grammar point indicating that (B) happens the instant (A) happens.
 * Often used for habitual actions and in literary contexts. The (B) clause
 * is always in past tense.
 *
 * Structure:
 * - Verb［る］+ が早いか
 * - Verb［た］+ が早いか
 *
 * Can follow either dictionary form or past tense verbs.
 *
 * Examples:
 * - チャイムが鳴るが早いか、生徒たちは教室を出た。
 *   (As soon as the bell rang, the students left the classroom.)
 * - 彼はベッドに入るが早いか寝てしまった。
 *   (No sooner had he gotten into bed than he fell asleep.)
 * - アラームが鳴ったが早いか、スヌーズボタンを押した。
 *   (The moment the alarm went off, he pressed the snooze button.)
 * - 警察官は犯人を見つけたが早いか、飛びつき手錠をかけた。
 *   (As soon as the police officer found the suspect, he tackled him.)
 *
 * Key discriminators:
 * - Particle が follows the verb (case marking)
 * - 早い is an adjective (not a verb)
 * - か is the final particle (question/indefinite)
 * - This is a fixed expression: "が + 早い + か"
 */
export default linguisticRule('が早いか', (r) => {
  // Any verb (can be dictionary form, past form, etc.)
  // We don't constrain the form since both dictionary and past forms are valid
  const verb = r.tok({
    posOneOf: ['VERB', 'AUX']
  }, 'verb');

  // Particle が (case marker)
  const ga = r.particle('が', 'ga');
  r.inOrder(verb, ga, 3); // Allow distance for compound verbs

  // 早い (adjective: early/fast) - can be kanji or hiragana
  // GiNZA may parse as ADJ or as part of fixed expression
  // Try matching as any token first
  const hayai = r.tok({ textOneOf: ['早い', 'はやい'] }, 'hayai');
  r.inOrder(ga, hayai, 2);

  // Final particle か
  const ka = r.particle('か', 'ka');
  r.inOrder(hayai, ka, 1);

  // Capture from verb to か
  r.captureSpan('が早いか', verb, ka);
});
