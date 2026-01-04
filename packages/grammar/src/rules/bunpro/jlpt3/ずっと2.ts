import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ずっと2', (r) => {
  // ずっと2 (zutto) - adverb meaning "by far, way, far more"
  // Used in comparisons to show that A is much greater than B
  // Different from ずっと1 which means "continuously/always"
  //
  // Key distinction from ずっと1:
  // - ずっと1: precedes progressive verbs (待っていた, 住んでいる, 勉強している)
  //          or state predicates (ずっと好き, ずっと嫌い)
  // - ずっと2: precedes adjectives or comparison contexts (大きい, 安い, 強い, etc.)
  //
  // Patterns from examples:
  // - + i-adjective: ずっと安い, ずっと強い, ずっと大きい, ずっと早い
  // - + na-adjective: ずっと大人らしい, ずっと混雑している
  // - + verb (potential): ずっとよく見える, ずっと使いやすくなった
  // - + abstract noun: ずっと昔, ずっと気分, ずっと年上
  // - Emphatic: ずっとずっと (doubled for emphasis)

  const zutto = r.adv({ text: 'ずっと' }, 'zutto');

  r.either(
    // Pattern 1: ずっと + i-adjective
    // ずっと安い, ずっと強い, ずっと大きい, ずっと早い
    (b) => {
      const adj = b.adj({
        tag: '形容詞-一般',
      }, 'adj');
      b.inOrder(zutto, adj, 3);
      b.captureSpan('ずっと', zutto, adj);
    },

    // Pattern 2: ずっと + na-adjective (大人らしい, 年上 - specific lemmas)
    // These are ずっと2 (comparison) not ずっと1 (continuous)
    (b) => {
      const adj = b.adj({
        tag: '形状詞-一般',
        lemmaOneOf: ['大人', '年上', '混雑', '大人らしい'],
      }, 'adj');
      b.inOrder(zutto, adj, 3);
      b.captureSpan('ずっと', zutto, adj);
    },

    // Pattern 2b: ずっと + aux (らしい) for "大人らしい" type
    (b) => {
      const noun = b.noun({
        lemmaOneOf: ['大人'],
      }, 'noun');
      const rashii = b.aux({
        lemma: 'らしい',
      }, 'rashii');
      b.inOrder(zutto, noun, 2);
      b.inOrder(noun, rashii, 1);
      b.captureSpan('ずっと', zutto, rashii);
    },

    // Pattern 3: ずっと + abstract noun (time/degree/comparison)
    // ずっと昔, ずっと気分, ずっと年上
    (b) => {
      const noun = b.noun({
        lemmaOneOf: ['昔', '気分', '年上', '大人'],
      }, 'noun');
      b.inOrder(zutto, noun, 2);
      b.captureSpan('ずっと', zutto, noun);
    },

    // Pattern 4: ずっと + potential/resultative verb (なる, 見える, etc.)
    // ずっと使いやすくなった, ずっとよく見える
    // But NOT: ずっと待っている, ずっと住んでいる (progressive forms)
    (b) => {
      const verb = b.verb({
        lemmaOneOf: ['なる', '見える', '聞こえる'],
      }, 'verb');
      b.inOrder(zutto, verb, 6);  // Allow gaps for auxiliaries
      b.captureSpan('ずっと', zutto, verb);
    },

    // Pattern 5: ずっと + adverb + verb
    // ずっとよく見える
    (b) => {
      const adv = b.adv({}, 'adv');
      const verb = b.verb({
        lemmaOneOf: ['なる', '見える', '聞こえる'],
      }, 'verb');
      b.inOrder(zutto, adv, 2);
      b.inOrder(adv, verb, 5);
      b.captureSpan('ずっと', zutto, verb);
    },

    // Pattern 6: Specific state verb (混雑している - "is crowded")
    // Exception: 混雑 is a state, not continuous action
    (b) => {
      const noun = b.noun({
        lemma: '混雑',
      }, 'noun');
      const suru = b.aux({
        lemma: 'する',
      }, 'suru');
      const te = b.tok({
        text: 'て',
        pos: 'SCONJ',
      }, 'te');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');

      b.inOrder(zutto, noun, 2);
      b.inOrder(noun, suru, 1);
      b.inOrder(suru, te, 1);
      b.inOrder(te, iru, 1);
      b.captureSpan('ずっと', zutto, iru);
    },

    // Pattern 7: ずっとずっと (emphatic doubled form)
    // ずっとずっと強くなる
    (b) => {
      const zutto2 = b.adv({ text: 'ずっと' }, 'zutto2');
      const target = b.tok({
        posOneOf: ['ADJ', 'VERB'],
      }, 'target');
      b.inOrder(zutto, zutto2, 1);
      b.inOrder(zutto2, target, 3);
      b.captureSpan('ずっと', zutto, target);
    }
  );
});
