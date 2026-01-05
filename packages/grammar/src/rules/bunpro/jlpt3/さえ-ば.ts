import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('さえ-ば', (r) => {
  // さえ-ば (if only/even if) - conditional pattern with さえ particle
  // Key: The ば (conditional) is always a separate SCONJ token with lemma=ば, dep=mark
  r.either(
    // Pattern 1: Noun + さえ + aux/verb/adj (conditional) + ば
    // e.g., 機会さえあれば、天気さえ良ければ、暇さえあれば
    // GiNZA: noun (NOUN/PROPN/PRON) + さえ (ADP) + aux/verb/adj (conditional stem) + ば (SCONJ)
    (r1) => {
      const noun = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const sae = r1.particle('さえ', 'sae');
      const stem = r1.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'stem');
      const ba = r1.tok({ text: 'ば', pos: 'SCONJ', dep: 'mark' }, 'ba');

      r1.inOrder(noun, sae, 1);
      r1.inOrder(sae, stem, 1);
      r1.inOrder(stem, ba, 1);
      r1.captureSpan('さえ-ば', noun, ba);
    },
    // Pattern 2: Verb stem + さえ + aux/verb (conditional) + ば
    // e.g., 飲みさえすれば (nomi sae sureba), ふらさえなければ
    // GiNZA: verb (VERB) + さえ (PART) + aux/verb (conditional) + ば (SCONJ)
    (r2) => {
      const verb1 = r2.verb({}, 'verb1');
      const sae = r2.particle('さえ', 'sae');
      const verb2 = r2.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb2');
      const ba = r2.tok({ text: 'ば', pos: 'SCONJ', dep: 'mark' }, 'ba');

      r2.inOrder(verb1, sae, 3);
      r2.inOrder(sae, verb2, 3);
      r2.inOrder(verb2, ba, 1);
      r2.captureSpan('さえ-ば', verb1, ba);
    },
    // Pattern 3: Verb て-form + さえ + aux/verb (conditional) + ば
    // e.g., 生きてさえいれば、貯めてさえいれば
    // GiNZA: verb + て (SCONJ) + さえ (PART) + aux/verb (conditional) + ば (SCONJ)
    (r3) => {
      const verb1 = r3.verb({}, 'verb1');
      const te = r3.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      const sae = r3.particle('さえ', 'sae');
      const verb2 = r3.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb2');
      const ba = r3.tok({ text: 'ば', pos: 'SCONJ', dep: 'mark' }, 'ba');

      r3.inOrder(verb1, te, 1);
      r3.inOrder(te, sae, 1);
      r3.inOrder(sae, verb2, 3);
      r3.inOrder(verb2, ba, 1);
      r3.captureSpan('さえ-ば', verb1, ba);
    },
    // Pattern 4: い-adj stem (連用形) + さえ + aux/verb (conditional) + ば
    // e.g., 若くさえあれば (wakaku sae areba), 広くさえあれば
    // GiNZA: adj (ADJ, 連用形-一般) + さえ (ADP) + aux/verb (conditional) + ば (SCONJ)
    (r4) => {
      const adj = r4.tok({
        pos: 'ADJ',
        conjugationClass: '形容詞',
        inflectionForm: '連用形-一般',
      }, 'adj');
      const sae = r4.particle('さえ', 'sae');
      const stem = r4.tok({ posOneOf: ['VERB', 'AUX'] }, 'stem');
      const ba = r4.tok({ text: 'ば', pos: 'SCONJ', dep: 'mark' }, 'ba');

      r4.inOrder(adj, sae, 1);
      r4.inOrder(sae, stem, 1);
      r4.inOrder(stem, ba, 1);
      r4.captureSpan('さえ-ば', adj, ba);
    },
    // Pattern 5: な-adj + で + さえ + aux/verb (conditional) + ば
    // e.g., 静かでさえあれば
    // GiNZA: adj (ADJ) + で (AUX) + さえ (ADP) + aux/verb (conditional) + ば (SCONJ)
    (r5) => {
      const adj = r5.adj({}, 'adj');
      const de = r5.tok({ text: 'で', pos: 'AUX', lemma: 'だ' }, 'de');
      const sae = r5.particle('さえ', 'sae');
      const stem = r5.tok({ posOneOf: ['VERB', 'AUX'] }, 'stem');
      const ba = r5.tok({ text: 'ば', pos: 'SCONJ', dep: 'mark' }, 'ba');

      r5.inOrder(adj, de, 1);
      r5.inOrder(de, sae, 1);
      r5.inOrder(sae, stem, 1);
      r5.inOrder(stem, ba, 1);
      r5.captureSpan('さえ-ば', adj, ba);
    },
    // Pattern 6: Noun + さえ + verb + aux (potential/conditional) + ば
    // e.g., 死体さえ発見できれば、責任を持った生活さえできれば
    // GiNZA: noun + さえ + verb + aux + ば
    (r6) => {
      const noun = r6.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const sae = r6.particle('さえ', 'sae');
      const verb = r6.verb({}, 'verb');
      const aux = r6.aux({}, 'aux');
      const ba = r6.tok({ text: 'ば', pos: 'SCONJ', dep: 'mark' }, 'ba');

      r6.inOrder(noun, sae, 1);
      r6.inOrder(sae, verb, 4);
      r6.inOrder(verb, aux, 1);
      r6.inOrder(aux, ba, 1);
      r6.captureSpan('さえ-ば', noun, ba);
    },
    // Pattern 7: Noun + さえ + [intervening words] + verb + aux + ば
    // e.g., あの背が高い人さえ横にずれてくれれば
    // This handles complex structures with adverbials between さえ and verb
    (r7) => {
      const noun = r7.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const sae = r7.particle('さえ', 'sae');
      const verb = r7.verb({}, 'verb');
      const ba = r7.tok({ text: 'ば', pos: 'SCONJ', dep: 'mark' }, 'ba');

      r7.inOrder(noun, sae, 1);
      r7.inOrder(sae, verb, 10);
      r7.inOrder(verb, ba, 6);
      r7.captureSpan('さえ-ば', noun, ba);
    }
  );
});
