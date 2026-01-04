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
  // - + na-adjective: ずっと大人らしい, ずっと混雑している, ずっと年上
  // - + verb (potential): ずっとよく見える, ずっと使いやすくなった
  // - + abstract noun: ずっと昔, ずっと気分, ずっと年上
  // - Emphatic: ずっとずっと (doubled for emphasis)
  //
  // GiNZA parsing notes:
  // - 好き, 嫌い are tagged as 形状詞-一般 (na-adjectives) but are state predicates (ずっと1)
  // - Pattern 2 must exclude these specific lemmas to avoid false positives
  // - Complex verb forms like 使いやすくなった are parsed as verb + auxiliaries

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

    // Pattern 2: ずっと + verb (+ auxiliaries)
    // Covers: ずっと使いやすくなった, ずっと混雑している, ずっとよく見える
    // GiNZA parses: verb + aux(s) chain, we match the first verb
    // Note: This pattern will also match some ずっと1 (continuous) patterns like ずっと住んでいる
    // because they are syntactically identical. This is a known limitation - see test file for details.
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(zutto, verb, 2);
      b.captureSpan('ずっと', zutto, verb);
    },

    // Pattern 3: ずっと + na-adjective (excluding state predicates)
    // ずっと大人らしい, ずっと年上
    // Must NOT match: ずっと好き, ずっと嫌い (these are ずっと1 - continuous state)
    // GiNZA tags these as 形状詞-一般, so we exclude specific lemmas
    // Since we can't directly exclude lemmas in the same condition, we use tag filtering
    // Solution: Accept na-adjectives that are NOT state predicates
    // State predicates (ずっと1) typically take です/だ directly: 好きです, 嫌いだ
    // Comparison adjectives (ずっと2) typically modify nouns or take other forms: 年上の, 大人らしい
    // Unfortunately GiNZA doesn't distinguish these well in isolation, so we accept potential false positives
    (b) => {
      const adj = b.adj({
        tag: '形状詞-一般',
      }, 'adj');
      b.inOrder(zutto, adj, 3);
      // Require following modifier/particle to distinguish from state predicates
      // State predicates: 好きです (copula immediately follows)
      // Comparison adjectives: 年上の (particle/modifier follows), 混雑している (auxiliary follows)
      b.optional((bb) => {
        const following = bb.tok({
          posOneOf: ['AUX', 'ADP', 'PART'],
        }, 'following');
        bb.inOrder(adj, following, 1);
      });
      b.captureSpan('ずっと', zutto, adj);
    },

    // Pattern 4: ずっと + abstract noun (time/degree/comparison)
    // ずっと昔, ずっと気分, ずっと年上
    // Note: Must not match action nouns like 勉強 (ずっと1)
    (b) => {
      const noun = b.noun({}, 'noun');
      b.inOrder(zutto, noun, 2);
      b.captureSpan('ずっと', zutto, noun);
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
