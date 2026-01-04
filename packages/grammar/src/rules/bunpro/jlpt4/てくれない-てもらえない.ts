import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('てくれない-てもらえない', (r) => {
  // This grammar point captures casual negative request forms:
  // 1. てくれない (か) - "Won't you do (A) for me?"
  // 2. てもらえない (か) - "Could I get you to do (A) for me?"
  // 3. ないでくれない (か) - "Won't you NOT do (A) for me?"
  // 4. ないでもらえない (か) - "Could I get you to NOT do (A) for me?"
  //
  // All forms are casual requests, used with friends/family.
  // The question particle か is optional (more polite with it).
  //
  // GiNZA parsing notes:
  // - "教えてくれない" parses as:
  //   - 教え (verb, root)
  //   - て (SCONJ, mark) -> head points to 教え
  //   - くれ (VERB, lemma=くれる, 未然形-一般, dep=fixed) -> head points to て
  //   - ない (AUX, lemma=ない, 終止形-一般, dep=aux) -> head points to 教え!
  // - "とってもらえない" parses as:
  //   - とっ (verb, root)
  //   - て (SCONJ, mark) -> head points to とっ
  //   - もらえ (VERB, lemma=もらえる, 未然形-一般, dep=fixed) -> head points to て
  //   - ない (AUX, lemma=ない, 終止形-一般, dep=aux) -> head points to とっ!
  //
  // Key insight: The ない auxiliary attaches to the main verb, not to くれ/もらえ.

  r.either(
    // Pattern 1a: てくれない (without か)
    (b) => {
      const verbTe = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verbTe');
      const te1 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te1');
      const kure = b.verb({
        lemma: 'くれる',
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'kure');
      const nai = b.aux({
        lemma: 'ない',
        inflectionForm: '終止形-一般',
      }, 'nai');

      b.headChild(verbTe, te1, 'mark');
      b.inOrder(verbTe, te1, 3); // Allow causative/passive auxiliaries (verb+causative+te)
      b.inOrder(te1, kure, 5);
      b.auxOf(verbTe, nai);
      b.inOrder(kure, nai, 3);

      b.captureSpan('てくれない', verbTe, nai);
    },

    // Pattern 1b: てくれないか (with か)
    (b) => {
      const verbTe = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verbTe');
      const te1 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te1');
      const kure = b.verb({
        lemma: 'くれる',
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'kure');
      const nai = b.aux({
        lemma: 'ない',
        inflectionForm: '終止形-一般',
      }, 'nai');
      const ka = b.particle('か', 'ka');

      b.headChild(verbTe, te1, 'mark');
      b.inOrder(verbTe, te1, 3); // Allow causative/passive auxiliaries (verb+causative+te)
      b.inOrder(te1, kure, 5);
      b.auxOf(verbTe, nai);
      b.inOrder(kure, nai, 3);
      b.inOrder(nai, ka, 2);

      b.captureSpan('てくれない', verbTe, ka);
    },

    // Pattern 2a: てもらえない (without か)
    (b) => {
      const verbTe = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verbTe');
      const te1 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te1');
      const morae = b.verb({
        lemmaOneOf: ['もらう', 'もらえる'],
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'morae');
      const nai = b.aux({
        lemma: 'ない',
        inflectionForm: '終止形-一般',
      }, 'nai');

      b.headChild(verbTe, te1, 'mark');
      b.inOrder(verbTe, te1, 1);
      b.inOrder(te1, morae, 5);
      b.auxOf(verbTe, nai);
      b.inOrder(morae, nai, 3);

      b.captureSpan('てもらえない', verbTe, nai);
    },

    // Pattern 2b: てもらえないか (with か)
    (b) => {
      const verbTe = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verbTe');
      const te1 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te1');
      const morae = b.verb({
        lemmaOneOf: ['もらう', 'もらえる'],
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'morae');
      const nai = b.aux({
        lemma: 'ない',
        inflectionForm: '終止形-一般',
      }, 'nai');
      const ka = b.particle('か', 'ka');

      b.headChild(verbTe, te1, 'mark');
      b.inOrder(verbTe, te1, 1);
      b.inOrder(te1, morae, 5);
      b.auxOf(verbTe, nai);
      b.inOrder(morae, nai, 3);
      b.inOrder(nai, ka, 2);

      b.captureSpan('てもらえない', verbTe, ka);
    },

    // Pattern 3a: ないでくれない (without か)
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai1 = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai1');
      const de = b.tok({ text: 'で', pos: 'SCONJ', dep: 'mark' }, 'de');
      const kure = b.verb({
        lemma: 'くれる',
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'kure');
      const nai2 = b.aux({
        lemma: 'ない',
        inflectionForm: '終止形-一般',
      }, 'nai2');

      b.auxOf(verb, nai1);
      b.inOrder(verb, nai1, 2); // Allow suru-verb auxiliary (verb+suru+nai)
      b.headChild(verb, de, 'mark');
      b.inOrder(nai1, de, 1);
      b.inOrder(de, kure, 5);
      b.auxOf(verb, nai2);
      b.inOrder(kure, nai2, 3);

      b.captureSpan('ないでくれない', verb, nai2);
    },

    // Pattern 3b: ないでくれないか (with か)
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai1 = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai1');
      const de = b.tok({ text: 'で', pos: 'SCONJ', dep: 'mark' }, 'de');
      const kure = b.verb({
        lemma: 'くれる',
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'kure');
      const nai2 = b.aux({
        lemma: 'ない',
        inflectionForm: '終止形-一般',
      }, 'nai2');
      const ka = b.particle('か', 'ka');

      b.auxOf(verb, nai1);
      b.inOrder(verb, nai1, 2); // Allow suru-verb auxiliary (verb+suru+nai)
      b.headChild(verb, de, 'mark');
      b.inOrder(nai1, de, 1);
      b.inOrder(de, kure, 5);
      b.auxOf(verb, nai2);
      b.inOrder(kure, nai2, 3);
      b.inOrder(nai2, ka, 2);

      b.captureSpan('ないでくれない', verb, ka);
    },

    // Pattern 4a: ないでもらえない (without か)
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai1 = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai1');
      const de = b.tok({ text: 'で', pos: 'SCONJ', dep: 'mark' }, 'de');
      const morae = b.verb({
        lemmaOneOf: ['もらう', 'もらえる'],
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'morae');
      const nai2 = b.aux({
        lemma: 'ない',
        inflectionForm: '終止形-一般',
      }, 'nai2');

      b.auxOf(verb, nai1);
      b.inOrder(verb, nai1, 2); // Allow suru-verb auxiliary (verb+suru+nai)
      b.headChild(verb, de, 'mark');
      b.inOrder(nai1, de, 1);
      b.inOrder(de, morae, 5);
      b.auxOf(verb, nai2);
      b.inOrder(morae, nai2, 3);

      b.captureSpan('ないでもらえない', verb, nai2);
    },

    // Pattern 4b: ないでもらえないか (with か)
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai1 = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai1');
      const de = b.tok({ text: 'で', pos: 'SCONJ', dep: 'mark' }, 'de');
      const morae = b.verb({
        lemmaOneOf: ['もらう', 'もらえる'],
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'morae');
      const nai2 = b.aux({
        lemma: 'ない',
        inflectionForm: '終止形-一般',
      }, 'nai2');
      const ka = b.particle('か', 'ka');

      b.auxOf(verb, nai1);
      b.inOrder(verb, nai1, 2); // Allow suru-verb auxiliary (verb+suru+nai)
      b.headChild(verb, de, 'mark');
      b.inOrder(nai1, de, 1);
      b.inOrder(de, morae, 5);
      b.auxOf(verb, nai2);
      b.inOrder(morae, nai2, 3);
      b.inOrder(nai2, ka, 2);

      b.captureSpan('ないでもらえない', verb, ka);
    }
  );
});
