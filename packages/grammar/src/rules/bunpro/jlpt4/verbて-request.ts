import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('verbて-request', (r) => {
  // Verbて-request: verb in te-form used as casual request at end of sentence
  // Examples: ちょっとまって。部屋を片付けてね。助けて！
  // Key: te-form verb that is sentence-final (possibly followed by sentence-final particles)

  r.either(
    // Pattern 1: Simple Verb-て at sentence end (verb has dep=root)
    (b1) => {
      const verb = b1.tok({ pos: 'VERB', dep: 'root' }, 'verb');
      const te = b1.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      b1.headChild(verb, te, 'mark');
      b1.inOrder(verb, te, 1);
      b1.captureSpan('verbて-request', verb, te);
    },

    // Pattern 1b: Verb-て where te has dep=root
    (b1b) => {
      const verb = b1b.tok({ pos: 'VERB' }, 'verb');
      const te = b1b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'root' }, 'te');
      b1b.headChild(verb, te, 'mark');
      b1b.inOrder(verb, te, 1);
      b1b.captureSpan('verbて-request', verb, te);
    },

    // Pattern 2: Verb-て + sentence-final particle (ね)
    (b2) => {
      const verb = b2.tok({ pos: 'VERB', dep: 'root' }, 'verb');
      const te = b2.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const particle = b2.particle('ね', 'ne');
      b2.headChild(verb, te, 'mark');
      b2.inOrder(verb, te, 1);
      b2.inOrder(te, particle, 3);
      b2.captureSpan('verbて-request', verb, particle);
    },

    // Pattern 3: Verb-て + sentence-final particle (よ)
    (b3) => {
      const verb = b3.tok({ pos: 'VERB', dep: 'root' }, 'verb');
      const te = b3.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const particle = b3.particle('よ', 'yo');
      b3.headChild(verb, te, 'mark');
      b3.inOrder(verb, te, 1);
      b3.inOrder(te, particle, 3);
      b3.captureSpan('verbて-request', verb, particle);
    },

    // Pattern 4: Verb-ておいて (ておく pattern as request)
    (b4) => {
      const verb = b4.tok({ pos: 'VERB', dep: 'root' }, 'verb');
      const te = b4.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const o = b4.tok({ text: 'お' }, 'o');
      const ite = b4.verb({ text: 'いて' }, 'ite');
      b4.headChild(verb, te, 'mark');
      b4.inOrder(verb, te, 1);
      b4.inOrder(te, o, 1);
      b4.inOrder(o, ite, 1);
      b4.captureSpan('verbて-request', verb, ite);
    },

    // Pattern 5: Verb-てみて (てみる pattern as request)
    (b5) => {
      const verb = b5.tok({ pos: 'VERB', dep: 'root' }, 'verb');
      const te = b5.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const mi = b5.tok({ text: 'み' }, 'mi');
      const te2 = b5.tok({ text: 'て', pos: 'SCONJ' }, 'te2');
      b5.headChild(verb, te, 'mark');
      b5.inOrder(verb, te, 1);
      b5.inOrder(te, mi, 1);
      b5.inOrder(mi, te2, 1);
      b5.captureSpan('verbて-request', verb, te2);
    },

    // Pattern 6: Verb-てて (short for ている, as in 待ってて)
    (b6) => {
      const verb = b6.tok({ pos: 'VERB', dep: 'root' }, 'verb');
      const te1 = b6.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te1');
      const te2 = b6.tok({ text: 'て', pos: 'SCONJ' }, 'te2');
      b6.headChild(verb, te1, 'mark');
      b6.inOrder(verb, te1, 1);
      b6.inOrder(te1, te2, 1);
      b6.captureSpan('verbて-request', verb, te2);
    }
  );
});
