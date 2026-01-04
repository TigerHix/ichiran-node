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

    // Pattern 2: ずっと + na-adjective (excluding state predicates)
    // ずっと大人らしい, ずっと混雑している, ずっと年上
    // Exclude: 好き, 嫌い, which are ずっと1 (continuous state)
    (b) => {
      const adj = b.adj({
        tag: '形状詞-一般',
      }, 'adj');
      b.not((bb) => {
        // Exclude state predicates that indicate continuous feeling (ずっと1)
        bb.tok({
          tag: '形状詞-一般',
          lemmaOneOf: ['好き', '嫌い', 'きらい'],
        });
      });
      b.inOrder(zutto, adj, 3);
      b.captureSpan('ずっと', zutto, adj);
    },

    // Pattern 3: ずっと + abstract noun (time/degree/comparison)
    // ずっと昔, ずっと気分, ずっと年上
    // Note: Excluding action nouns like 勉強 to avoid matching ずっと勉強 (ずっと1)
    (b) => {
      const noun = b.noun({}, 'noun');
      b.not((bb) => {
        // Exclude action nouns that would be ずっと1
        bb.tok({
          lemmaOneOf: ['勉強', '待っ', '住ん', '運動'],
        });
      });
      b.inOrder(zutto, noun, 2);
      b.captureSpan('ずっと', zutto, noun);
    },

    // Pattern 4: ずっと + adverb (e.g., よく) + verb
    // ずっとよく見える, ずっと使いやすくなった
    (b) => {
      const adv = b.adv({}, 'adv');
      const verb = b.verb({}, 'verb');
      b.inOrder(zutto, adv, 2);
      b.inOrder(adv, verb, 3);
      b.captureSpan('ずっと', zutto, verb);
    },

    // Pattern 5: ずっとずっと (emphatic doubled form)
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
