import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ようにいのる - Pray that/for, Hope that/for
 *
 * Expresses a prayer or hope for something to happen (or not happen).
 * Combines the purpose marker ように with the verb 祈る (to pray/wish).
 *
 * Structure:
 * - Verb (dictionary/negative/potential form) + ように + 祈る (and conjugations)
 *
 * Examples:
 * - 世界が平和になるように祈る (pray that the world becomes peaceful)
 * - 授業中に先生に呼ばれないように祈る (pray that teacher doesn't call on me)
 * - 合格できるように毎日祈っている (pray every day that I can pass)
 * - 安全に帰れるように祈ろう (let's pray that he can return safely)
 * - 雨が降らないように祈った (prayed that it wouldn't rain)
 *
 * Key discriminators:
 * - Preceded by verb in any form (dictionary, negative, potential)
 * - ように (purpose marker) connects to forms of 祈る
 * - 祈る can be in various conjugations:
 *   - 祈る (dictionary form)
 *   - 祈っている (progressive)
 *   - 祈った (past)
 *   - 祈ろう (volitional)
 *   - 祈って (te-form)
 *   - 祈りましょう (polite volitional)
 *
 * GiNZA parsing notes:
 * - ように often parsed as single token (text=ように, pos=SCONJ)
 * - Sometimes parsed as よう (pos=NOUN/SCONJ) + に (pos=ADP/particle)
 * - 祈る is lemma=祈る, pos=VERB
 * - Conjugated forms have inflectionForm set appropriately
 *
 * Note: This pattern is distinct from other ように patterns because it
 * specifically requires the verb 祈る (to pray/wish).
 */
export default linguisticRule('ようにいのる', (r) => {
  r.either(
    // Branch 1: ように as single token + various forms of 祈る
    (b1) => {
      const verb = b1.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'verb');
      const yoni = b1.tok({ textOneOf: ['ように', '樣に'] }, 'yoni');
      const inoru = b1.verb({ lemmaOneOf: ['祈る', 'いのる'] }, 'inoru');

      b1.inOrder(verb, yoni, 10);
      b1.inOrder(yoni, inoru, 10);
      b1.captureSpan('ようにいのる', verb, inoru);
    },

    // Branch 2: よう and に as separate tokens + various forms of 祈る
    (b2) => {
      const verb = b2.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'verb');
      const you = b2.tok({ text: 'よう' }, 'you');
      const ni = b2.particle('に', 'ni');
      const inoru = b2.verb({ lemmaOneOf: ['祈る', 'いのる'] }, 'inoru');

      b2.inOrder(verb, you, 10);
      b2.inOrder(you, ni, 1);
      b2.inOrder(ni, inoru, 10);
      b2.captureSpan('ようにいのる', verb, inoru);
    }
  );
});
