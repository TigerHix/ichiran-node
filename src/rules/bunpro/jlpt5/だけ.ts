import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('だけ', (r) => {
  // だけ (dake) - adverbial particle meaning "only, just"
  // Attaches to: Verb + だけ, i-adj + だけ, na-adj + なだけ, Noun + だけ
  //
  // This particle indicates limitation or restriction - "only this thing" or "just this much".
  // Unlike しか～ない (which requires negative verbs), だけ can be used in any context.
  //
  // Examples:
  // - 食べるだけ (only eat)
  // - 長いだけ (just long - i-adj)
  // - 綺麗なだけ (just beautiful - na-adj + な)
  // - パンだけ (only bread - noun)
  // - 私だけ (only me - pronoun)
  //
  // Note: When だけ follows a verb at a clause boundary, it has dep=mark instead of dep=case

  // だけ can be dep=case (when attached to nouns) or dep=mark (when attached to verbs/clauses)
  r.either(
    // Branch 1: Noun/Pronoun + だけ (dep=case)
    (branch1) => {
      const dake = branch1.particle('だけ', 'dake', { dep: 'case' });
      const noun = branch1.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'NUM'] }, 'noun');
      branch1.caseMarker(noun, dake);
      branch1.capture(dake);
    },
    // Branch 2: Verb + だけ (dep=mark for clause boundary)
    (branch2) => {
      const dake = branch2.particle('だけ', 'dake', { depOneOf: ['case', 'mark'] });
      const verb = branch2.verb({}, 'verb');
      branch2.headChild(verb, dake, 'mark');
      branch2.capture(dake);
    },
    // Branch 3: i-Adjective + だけ
    (branch3) => {
      const dake = branch3.particle('だけ', 'dake', { depOneOf: ['case', 'mark'] });
      const iAdj = branch3.adj({}, 'iAdj');
      branch3.inOrder(iAdj, dake, 1);
      branch3.capture(dake);
    },
    // Branch 4: na-Adjective + な + だけ
    (branch4) => {
      const dake = branch4.particle('だけ', 'dake', { depOneOf: ['case', 'mark'] });
      const naAdj = branch4.adj({}, 'naAdj');
      const na = branch4.tok({ text: 'な', pos: 'AUX' }, 'na');
      branch4.inOrder(naAdj, na, 1);
      branch4.inOrder(na, dake, 1);
      branch4.capture(dake);
    }
  );
});
