import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('と共に', (r) => {
  // と共に (to tomo ni) - "together with", "at the same time as", "as well as"
  // Shows two or more things happening or existing in unison, or as one.
  //
  // Patterns:
  // 1. Noun + と共に: 心身と共に, 夜明けと共に, 驚きと共に, 私と共に
  // 2. Verb (dictionary form) + と共に: 強くなると共に, 年を取ると共に
  // 3. Na-adjective + である + と共に: 静かであると共に, 怪奇であると共に
  // 4. I-adjective + と共に: 厳しいと共に, 寂しいと共に
  //
  // GiNZA parsing notes:
  // - と is ADP (particle) with lemma=と
  // - 共に/ともに may be tokenized as:
  //   1. Single token: 共に or ともに (ADV)
  //   2. Split tokens: 共 + に or とも + に
  //   3. More split: と + も + に

  r.either(
    // Pattern 1a: Noun + と + 共に/ともに (single token)
    // 心身と共に, 夜明けと共に, 驚きと共に, 私と共に, 父と共に
    // 風と共に, 仲間と共に, 妻と共に, 普及と共に
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const to = b.particle('と', 'to');
      const tomoNi = b.tok({
        textOneOf: ['共に', 'ともに'],
      }, 'tomoNi');
      b.inOrder(noun, to, 1);
      b.inOrder(to, tomoNi, 1);
      b.captureSpan('と共に', noun, tomoNi);
    },

    // Pattern 1b: Noun + と + 共/とも + に (split tokens)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const to = b.particle('と', 'to');
      const tomo = b.tok({
        textOneOf: ['共', 'とも'],
      }, 'tomo');
      const ni = b.particle('に', 'ni');
      b.inOrder(noun, to, 1);
      b.inOrder(to, tomo, 1);
      b.inOrder(tomo, ni, 1);
      b.captureSpan('と共に', noun, ni);
    },

    // Pattern 2a: Verb (終止形) + と + 共に/ともに (single token)
    // 風が強くなると共に, 年を取ると共に
    (b) => {
      const verb = b.verb({
        inflectionForm: '終止形-一般',
      }, 'verb');
      const to = b.particle('と', 'to');
      const tomoNi = b.tok({
        textOneOf: ['共に', 'ともに'],
      }, 'tomoNi');
      b.inOrder(verb, to, 1);
      b.inOrder(to, tomoNi, 1);
      b.captureSpan('と共に', verb, tomoNi);
    },

    // Pattern 2b: Verb (終止形) + と + 共/とも + に (split tokens)
    (b) => {
      const verb = b.verb({
        inflectionForm: '終止形-一般',
      }, 'verb');
      const to = b.particle('と', 'to');
      const tomo = b.tok({
        textOneOf: ['共', 'とも'],
      }, 'tomo');
      const ni = b.particle('に', 'ni');
      b.inOrder(verb, to, 1);
      b.inOrder(to, tomo, 1);
      b.inOrder(tomo, ni, 1);
      b.captureSpan('と共に', verb, ni);
    },

    // Pattern 3a: Na-adj + である + と + 共に/ともに (single token)
    // 静かであると共に, 怪奇であると共に, 綺麗であると共に
    (b) => {
      const naAdj = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
        tagOneOf: [
          '形状詞-一般',
          '名詞-普通名詞-一般',
          '名詞-普通名詞-形状詞可能',
        ],
      }, 'naAdj');
      const dearu = b.aux({
        lemma: 'である',
      }, 'dearu');
      const to = b.particle('と', 'to');
      const tomoNi = b.tok({
        textOneOf: ['共に', 'ともに'],
      }, 'tomoNi');
      b.inOrder(naAdj, dearu, 1);
      b.inOrder(dearu, to, 1);
      b.inOrder(to, tomoNi, 1);
      b.captureSpan('と共に', naAdj, tomoNi);
    },

    // Pattern 3b: Na-adj + である + と + 共/とも + に (split tokens)
    (b) => {
      const naAdj = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
        tagOneOf: [
          '形状詞-一般',
          '名詞-普通名詞-一般',
          '名詞-普通名詞-形状詞可能',
        ],
      }, 'naAdj');
      const dearu = b.aux({
        lemma: 'である',
      }, 'dearu');
      const to = b.particle('と', 'to');
      const tomo = b.tok({
        textOneOf: ['共', 'とも'],
      }, 'tomo');
      const ni = b.particle('に', 'ni');
      b.inOrder(naAdj, dearu, 1);
      b.inOrder(dearu, to, 1);
      b.inOrder(to, tomo, 1);
      b.inOrder(tomo, ni, 1);
      b.captureSpan('と共に', naAdj, ni);
    },

    // Pattern 4a: I-adj + と + 共に/ともに (single token)
    // 厳しいと共に, 寂しいと共に
    (b) => {
      const iAdj = b.adj({
        inflectionForm: '終止形-一般',
      }, 'iAdj');
      const to = b.particle('と', 'to');
      const tomoNi = b.tok({
        textOneOf: ['共に', 'ともに'],
      }, 'tomoNi');
      b.inOrder(iAdj, to, 1);
      b.inOrder(to, tomoNi, 1);
      b.captureSpan('と共に', iAdj, tomoNi);
    },

    // Pattern 4b: I-adj + と + 共/とも + に (split tokens)
    (b) => {
      const iAdj = b.adj({
        inflectionForm: '終止形-一般',
      }, 'iAdj');
      const to = b.particle('と', 'to');
      const tomo = b.tok({
        textOneOf: ['共', 'とも'],
      }, 'tomo');
      const ni = b.particle('に', 'ni');
      b.inOrder(iAdj, to, 1);
      b.inOrder(to, tomo, 1);
      b.inOrder(tomo, ni, 1);
      b.captureSpan('と共に', iAdj, ni);
    }
  );
});
