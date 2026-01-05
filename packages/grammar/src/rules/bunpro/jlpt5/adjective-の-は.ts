import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('adjective-の-は', (r) => {
  // Adjective + の(は) - The 'one' that... (Indefinite pronoun, Adjective nominalization)
  // Matches both い-adjectives and な-adjectives followed by の + (は/が/も)
  // Also matches sentence-final の as nominalizer

  r.either(
    // Branch 1: い-adjective + の + は (nominalizer + topic marker)
    // e.g., 高いのは, 可愛いのは
    (branch) => {
      const iAdj = branch.adj({
        tag: '形容詞-一般',
        depOneOf: ['advcl', 'csubj'],
      }, 'iAdj');

      const no = branch.tok({
        text: 'の',
        tag: '助詞-準体助詞',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'no');

      const particle = branch.particle('は', 'particle');

      branch.inOrder(iAdj, no, 1);
      branch.inOrder(no, particle, 1);
      branch.captureSpan('adjective-の-は', iAdj, particle);
    },
    // Branch 2: い-adjective + の + が (nominalizer + subject marker)
    // e.g., 熱いのが
    (branch) => {
      const iAdj = branch.adj({
        tag: '形容詞-一般',
        depOneOf: ['advcl', 'csubj'],
      }, 'iAdj');

      const no = branch.tok({
        text: 'の',
        tag: '助詞-準体助詞',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'no');

      const particle = branch.particle('が', 'particle', {
        tag: '助詞-格助詞',
        dep: 'case',
      });

      branch.inOrder(iAdj, no, 1);
      branch.inOrder(no, particle, 1);
      branch.captureSpan('adjective-の-は', iAdj, particle);
    },
    // Branch 3: い-adjective + の + も (nominalizer + also marker)
    // e.g., 冷たいのも
    (branch) => {
      const iAdj = branch.adj({
        tag: '形容詞-一般',
        depOneOf: ['advcl', 'csubj'],
      }, 'iAdj');

      const no = branch.tok({
        text: 'の',
        tag: '助詞-準体助詞',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'no');

      const particle = branch.particle('も', 'particle');

      branch.inOrder(iAdj, no, 1);
      branch.inOrder(no, particle, 1);
      branch.captureSpan('adjective-の-は', iAdj, particle);
    },
    // Branch 4: な-adjective + な + の + は
    // e.g., 心配なのは, 元気なのは
    (branch) => {
      const naAdj = branch.adj({
        depOneOf: ['advcl', 'csubj'],
      }, 'naAdj');

      const na = branch.aux({
        lemma: 'だ',
        text: 'な',
        dep: 'aux',
      }, 'na');

      const no = branch.tok({
        text: 'の',
        tag: '助詞-準体助詞',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'no');

      const particle = branch.particle('は', 'particle');

      branch.auxOf(naAdj, na);
      branch.inOrder(naAdj, no, 2);
      branch.inOrder(no, particle, 1);
      branch.captureSpan('adjective-の-は', naAdj, particle);
    },
    // Branch 5: な-adjective + な + の + が
    // e.g., 好きなのが
    (branch) => {
      const naAdj = branch.adj({
        depOneOf: ['advcl', 'csubj'],
      }, 'naAdj');

      const na = branch.aux({
        lemma: 'だ',
        text: 'な',
        dep: 'aux',
      }, 'na');

      const no = branch.tok({
        text: 'の',
        tag: '助詞-準体助詞',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'no');

      const particle = branch.particle('が', 'particle', {
        tag: '助詞-格助詞',
        dep: 'case',
      });

      branch.auxOf(naAdj, na);
      branch.inOrder(naAdj, no, 2);
      branch.inOrder(no, particle, 1);
      branch.captureSpan('adjective-の-は', naAdj, particle);
    },
    // Branch 6: い-adjective + の (sentence-final nominalizer)
    // e.g., 明るいの
    (branch) => {
      const iAdj = branch.adj({
        tag: '形容詞-一般',
        depOneOf: ['advcl', 'csubj', 'root'],
      }, 'iAdj');

      const no = branch.tok({
        text: 'の',
        tag: '助詞-終助詞',
        pos: 'PART',
        dep: 'mark',
      }, 'no');

      branch.inOrder(iAdj, no, 1);
      branch.captureSpan('adjective-の-は', iAdj, no);
    }
  );
});
