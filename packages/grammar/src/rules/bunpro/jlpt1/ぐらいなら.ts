import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ぐらいなら', (r) => {
  // Pattern: Verb (dictionary form) + ぐらい/くらい + なら
  // Expresses preference: "rather than X, I'd Y"
  // Examples: 死ぬぐらいなら, 修理するくらいなら, 嫌々するくらいなら

  // Match the verb (predicate) - can be VERB or NOUN acting as verb
  // Both くらい and なら will attach to this with dep=mark and dep=aux respectively
  const predicate = r.tok({
    posOneOf: ['VERB', 'NOUN', 'ADJ']
  }, 'predicate');

  // Match ぐらい/くらい (particle, lemma=ぐらい or くらい, dep=mark)
  const kurai = r.tok({
    textOneOf: ['ぐらい', 'くらい'],
    lemmaOneOf: ['ぐらい', 'くらい'],
    pos: 'PART',
    dep: 'mark'
  }, 'kurai');

  // Match なら (copula auxiliary, lemma=だ, inflectionForm=仮定形)
  const nara = r.aux({
    lemma: 'だ',
    inflectionForm: '仮定形-一般'
  }, 'nara');

  // Structural constraints: predicate appears somewhere before くらい, which is immediately before なら
  r.inOrder(predicate, kurai);
  r.inOrder(kurai, nara, 1);

  // Dependency constraints: both くらい and なら must attach to predicate
  r.headChild(predicate, kurai, 'mark');
  r.headChild(predicate, nara, 'aux');

  // Capture from predicate through なら
  r.captureSpan('ぐらいなら', predicate, nara);
});
