import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ので', (r) => {
  // ので (node): conjunction particle meaning "because, so, since"
  // Semi-formal expression indicating A caused or instigated B
  // Similar to から but more polite/formal
  //
  // Pattern 1: Noun/Na-adj + な + ので (e.g., 先生なので, 綺麗なので)
  // Pattern 2: い-Adjective + ので (e.g., 寒いので, 弱いので) - NO な
  // Pattern 3: Verb + ので (e.g., 行くので, 来るので) - NO な

  r.either(
    // Pattern 1: Noun/Na-adj + な + ので
    // This handles both nouns and na-adjectives
    (r1) => {
      const nounOrNaAdj = r1.tok({
        posOneOf: ['NOUN', 'PROPN', 'ADJ'],
      }, 'nounOrNaAdj');
      const na = r1.particle('な', 'na');
      const node = r1.tok({ text: 'ので' }, 'node');

      r1.inOrder(nounOrNaAdj, na, 1);
      r1.inOrder(na, node, 1);
      r1.headChild(nounOrNaAdj, node);

      r1.captureSpan('ので', nounOrNaAdj, node);
    },
    // Pattern 2: い-Adjective + ので (no な between)
    (r2) => {
      const iAdj = r2.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const node = r2.tok({ text: 'ので' }, 'node');

      r2.inOrder(iAdj, node, 1);
      r2.headChild(iAdj, node);

      r2.captureSpan('ので', iAdj, node);
    },
    // Pattern 3: Verb + (た)? + ので (no な between)
    // Optional た for past tense verbs
    (r3) => {
      const verb = r3.verb({}, 'verb');
      const node = r3.tok({ text: 'ので' }, 'node');

      r3.inOrder(verb, node, 3);
      r3.headChild(verb, node);

      r3.captureSpan('ので', verb, node);
    }
  );
});
