import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ただ-のみ', (r) => {
  // Pattern: (ただ) + X + のみ (formal version of だけ)
  // X can be:
  // 1. Verb (e.g., 待つのみだ, ただ待つのみだ, 従うのみだ, 降伏するのみだ)
  // 2. Noun + ある (from する verb) (e.g., 勉強あるのみだ, ただ勉強あるのみだ)
  // Note: ただ is optional and adds emphasis

  r.either(
    // Pattern 1a: ただ + Verb + のみ
    (b) => {
      const tada = b.adv({ text: 'ただ' }, 'tada');
      const verb = b.verb({}, 'verb');
      const nomi = b.tok({ text: 'のみ', pos: 'PART' }, 'nomi');
      b.inOrder(tada, verb).inOrder(verb, nomi);
      b.captureSpan('ただ-のみ', tada, nomi);
    },
    // Pattern 1b: Verb + のみ (without ただ)
    (b) => {
      const verb = b.verb({}, 'verb');
      const nomi = b.tok({ text: 'のみ', pos: 'PART' }, 'nomi');
      b.inOrder(verb, nomi);
      b.captureSpan('ただ-のみ', verb, nomi);
    },
    // Pattern 2a: ただ + Noun + ある + のみ (from する verbs)
    (b) => {
      const tada = b.adv({ text: 'ただ' }, 'tada');
      const noun = b.noun({}, 'noun');
      const aru = b.verb({ lemma: 'ある' }, 'aru');
      const nomi = b.tok({ text: 'のみ', pos: 'PART' }, 'nomi');
      b.inOrder(tada, noun).inOrder(noun, aru, 1).inOrder(aru, nomi, 1);
      b.captureSpan('ただ-のみ', tada, nomi);
    },
    // Pattern 2b: Noun + ある + のみ (without ただ)
    (b) => {
      const noun = b.noun({}, 'noun');
      const aru = b.verb({ lemma: 'ある' }, 'aru');
      const nomi = b.tok({ text: 'のみ', pos: 'PART' }, 'nomi');
      b.inOrder(noun, aru, 1).inOrder(aru, nomi, 1);
      b.captureSpan('ただ-のみ', noun, nomi);
    }
  );
});
