import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('てもかまわない', (r) => {
  // Pattern: Verb/Adj[て-form] + も + かまわない/かまいません (it's fine even if X / I don't mind if X)
  // e.g.:
  //   - やってもかまわない (it's fine even if you do it)
  //   - なくてもかまいません (it's fine even if there isn't any)
  //   - おそくてもかまいません (it's fine even if it's late)
  //   - 未経験者でもかまいません (it's fine even if inexperienced)

  r.either(
    // Pattern 1: Verb[て-form] + も + かまわない/かまいません
    // e.g., やってもかまわない, なくてもかまわない, されてもかまわない
    // GiNZA: verb[renyou/mizen] + て/で(SCONJ) + も(ADP,case) + かまう(VERB)
    (b) => {
      const verb = b.verb({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
          '未然形-一般',  // For negative verb forms like ない+て+も
        ],
      }, 'verb');
      const te = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      const kamawaru = b.verb({ lemma: 'かまう' }, 'kamawaru');
      b.headChild(verb, te, 'mark');
      b.headChild(verb, mo, 'case');
      b.inOrder(mo, kamawaru, 5);
      b.captureSpan('てもかまわない', verb, kamawaru);
    },
    // Pattern 2: い-Adj[て-form] + も + かまわない/かまいません
    // e.g., おそくてもかまいません, なくてもかまわない
    // GiNZA: adj[renyou] + て/で(SCONJ/AUX) + も(ADP,case) + かまう(VERB)
    (b) => {
      const adj = b.adj({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
        ],
      }, 'adj');
      const _te = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      const kamawaru = b.verb({ lemma: 'かまう' }, 'kamawaru');
      void _te;
      b.inOrder(mo, kamawaru, 5);
      b.captureSpan('てもかまわない', adj, kamawaru);
    },
    // Pattern 3: な-Adj + でも + かまわない/かまいません
    // e.g., 大変でもかまいません (rare, but possible)
    // GiNZA: adj(ADJ) + で(AUX,lemma=だ) + も(ADP,case) + かまう(VERB)
    (b) => {
      const adj = b.adj({}, 'adj');
      const de = b.aux({ lemma: 'だ', inflectionForm: '連用形-一般' }, 'de');
      b.auxOf(adj, de);
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(adj, mo, 'case');
      const kamawaru = b.verb({ lemma: 'かまう' }, 'kamawaru');
      b.inOrder(mo, kamawaru, 5);
      b.captureSpan('でもかまわない', adj, kamawaru);
    },
    // Pattern 4: Noun + でも + かまわない/かまいません
    // e.g., 未経験者でもかまいません, ピザでもかまわない
    // GiNZA: noun(NOUN/PRON) + で(ADP,case) + も(ADP,case) + かまう(VERB)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const de = b.tok({ text: 'で', pos: 'ADP', dep: 'case' }, 'de');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(noun, de, 'case');
      b.headChild(noun, mo, 'case');
      const kamawaru = b.verb({ lemma: 'かまう' }, 'kamawaru');
      b.inOrder(mo, kamawaru, 5);
      b.captureSpan('でもかまわない', noun, kamawaru);
    }
  );
});
