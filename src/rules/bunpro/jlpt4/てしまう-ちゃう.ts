import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('てしまう-ちゃう', (r) => {
  // GiNZA parses てしまう/ちゃう/じゃう inconsistently:
  // - 食べてしまった: しまう as VERB, dep=fixed
  // - 太っちゃう: ちゃう as AUX, dep=aux
  // - 食べてしまおう (volitional): しまおう as VERB, dep=root (GiNZA quirk)
  // Use r.either() to handle each case with optimal matching
  r.either(
    // Pattern 1: standard てしまう (dep=fixed)
    (b) => {
      const shimau = b.tok({ lemma: 'しまう', dep: 'fixed' }, 'shimau');
      b.capture(shimau);
    },
    // Pattern 2: contracted ちゃう/じゃう (pos=AUX)
    (b) => {
      const shimau = b.tok({ lemmaOneOf: ['ちゃう', 'じゃう'], pos: 'AUX' }, 'shimau');
      b.capture(shimau);
    },
    // Pattern 3: volitional てしまおう (GiNZA quirk: dep=root but has advcl child)
    (b) => {
      const shimau = b.tok({ lemma: 'しまう', dep: 'root' }, 'shimau');
      const verb = b.verb({}, 'v');
      b.headChild(shimau, verb, 'advcl'); // verb clause attached to shimau
      b.capture(shimau);
    }
  );
});

