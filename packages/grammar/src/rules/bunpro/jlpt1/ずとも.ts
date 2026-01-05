import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ずとも', (r) => {
  // Pattern: verb/noun stem + ず + とも (classical negative form meaning "even without X")
  // Examples: 見ずとも, 言わずとも, 専門家ならずとも
  //
  // GiNZA inconsistencies:
  // 1. とも can be parsed as single token (SCONJ) or two tokens (と + も)
  // 2. Verbs can be in 未然形 directly (聞か), need せ auxiliary (練習せ), or be せ itself (言葉にせ)
  // 3. なら can have lemma なり or なる

  r.either(
    // Pattern 1a: Verb(未然形-一般) + ず + とも (single)
    // Example: 聞かずとも, まなばずとも
    (b) => {
      const verb = b.verb({ inflectionForm: '未然形-一般' }, 'verb');
      const zu = b.tok({ text: 'ず', pos: 'AUX', lemma: 'ず' }, 'zu');
      const tomo = b.tok({ text: 'とも', pos: 'SCONJ' }, 'tomo');
      b.inOrder(verb, zu, 1);
      b.inOrder(zu, tomo, 1);
      b.captureSpan('ずとも', verb, tomo);
    },
    // Pattern 1b: Verb(未然形-一般) + ず + と + も (two tokens)
    (b) => {
      const verb = b.verb({ inflectionForm: '未然形-一般' }, 'verb');
      const zu = b.tok({ text: 'ず', pos: 'AUX', lemma: 'ず' }, 'zu');
      const to = b.particle('と', 'to');
      const mo = b.particle('も', 'mo');
      b.inOrder(verb, zu, 1);
      b.inOrder(zu, to, 1);
      b.inOrder(to, mo, 1);
      b.captureSpan('ずとも', verb, mo);
    },
    // Pattern 2a: Verb + せ(AUX,lemma=する) + ず + とも (single)
    // Example: 練習せずとも, 勉強せずとも, 留学せずとも
    (b) => {
      const verb = b.verb({}, 'verb');
      const se = b.aux({ lemma: 'する' }, 'se');
      const zu = b.tok({ text: 'ず', pos: 'AUX', lemma: 'ず' }, 'zu');
      const tomo = b.tok({ text: 'とも', pos: 'SCONJ' }, 'tomo');
      b.inOrder(verb, se, 1);
      b.inOrder(se, zu, 1);
      b.inOrder(zu, tomo, 1);
      b.captureSpan('ずとも', verb, tomo);
    },
    // Pattern 2b: Verb + せ(AUX) + ず + と + も (two tokens)
    (b) => {
      const verb = b.verb({}, 'verb');
      const se = b.aux({ lemma: 'する' }, 'se');
      const zu = b.tok({ text: 'ず', pos: 'AUX', lemma: 'ず' }, 'zu');
      const to = b.particle('と', 'to');
      const mo = b.particle('も', 'mo');
      b.inOrder(verb, se, 1);
      b.inOrder(se, zu, 1);
      b.inOrder(zu, to, 1);
      b.inOrder(to, mo, 1);
      b.captureSpan('ずとも', verb, mo);
    },
    // Pattern 2c: Verb(lemma=する,inflectionForm=未然形-セ) + ず + とも (single)
    // Example: 言葉にせずとも, 心配などせずとも
    // Note: Here せ is the main verb (not aux), typically after particle like に, など
    (b) => {
      const se = b.verb({ lemma: 'する', inflectionForm: '未然形-セ' }, 'se');
      const zu = b.tok({ text: 'ず', pos: 'AUX', lemma: 'ず' }, 'zu');
      const tomo = b.tok({ text: 'とも', pos: 'SCONJ' }, 'tomo');
      b.inOrder(se, zu, 1);
      b.inOrder(zu, tomo, 1);
      b.captureSpan('ずとも', se, tomo);
    },
    // Pattern 2d: Verb(lemma=する,inflectionForm=未然形-セ) + ず + と + も (two tokens)
    (b) => {
      const se = b.verb({ lemma: 'する', inflectionForm: '未然形-セ' }, 'se');
      const zu = b.tok({ text: 'ず', pos: 'AUX', lemma: 'ず' }, 'zu');
      const to = b.particle('と', 'to');
      const mo = b.particle('も', 'mo');
      b.inOrder(se, zu, 1);
      b.inOrder(zu, to, 1);
      b.inOrder(to, mo, 1);
      b.captureSpan('ずとも', se, mo);
    },
    // Pattern 3a: Noun + なら(AUX,lemma=なり/なる) + ず + とも (single)
    // Example: 専門家ならずとも, 親ならずとも
    (b) => {
      const noun = b.noun({}, 'noun');
      const nara = b.aux({ lemmaOneOf: ['なり', 'なる'] }, 'nara');
      const zu = b.tok({ text: 'ず', pos: 'AUX', lemma: 'ず' }, 'zu');
      const tomo = b.tok({ text: 'とも', pos: 'SCONJ' }, 'tomo');
      b.inOrder(noun, nara, 1);
      b.inOrder(nara, zu, 1);
      b.inOrder(zu, tomo, 1);
      b.captureSpan('ずとも', noun, tomo);
    },
    // Pattern 3b: Noun + なら + ず + と + も (two tokens)
    (b) => {
      const noun = b.noun({}, 'noun');
      const nara = b.aux({ lemmaOneOf: ['なり', 'なる'] }, 'nara');
      const zu = b.tok({ text: 'ず', pos: 'AUX', lemma: 'ず' }, 'zu');
      const to = b.particle('と', 'to');
      const mo = b.particle('も', 'mo');
      b.inOrder(noun, nara, 1);
      b.inOrder(nara, zu, 1);
      b.inOrder(zu, to, 1);
      b.inOrder(to, mo, 1);
      b.captureSpan('ずとも', noun, mo);
    }
  );
});
