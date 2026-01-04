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

    // Pattern 2: ずっと + verb in specific comparison contexts
    // Covers: ずっと使いやすくなった, ずっと混雑している, ずっとよく見える
    //
    // GiNZA parsing notes:
    // - Continuous forms (ずっと1): 待っていた, 住んでいます, 勉強している
    //   These verbs have dep=ROOT (main predicate)
    // - Comparison forms (ずっと2): 使いやすくなった, よく見える, 混雑している
    //   These verbs have dep=advcl or dep=acl (subordinate clause modifying something else)
    //
    // Discriminator: Only match verbs in subordinate clauses (advcl, acl), not ROOT predicates
    (b) => {
      const verb = b.verb({
        depOneOf: ['advcl', 'acl', 'obl'],  // Subordinate clauses, not main ROOT
      }, 'verb');
      b.inOrder(zutto, verb, 2);
      b.captureSpan('ずっと', zutto, verb);
    },

    // Pattern 3: ずっと + na-adjective (excluding state predicates)
    // ずっと大人らしい, ずっと年上
    // Must NOT match: ずっと好き, ずっと嫌い (these are ずっと1 - continuous state)
    //
    // GiNZA parsing notes:
    // - State predicates (ずっと1): 好き, 嫌い, 嫌
    //   These have dep=ROOT and lemma in [好き, 嫌い, 嫌]
    // - Comparison adjectives (ずっと2): 大人, 年上
    //   These modify nouns or have dep != ROOT
    //
    // Discriminator: Exclude specific lemmas that are state predicates
    (b) => {
      const adj = b.adj({
        tag: '形状詞-一般',
        depOneOf: ['acl', 'advmod', 'obl', 'nmod'],  // Not ROOT (main predicate)
      }, 'adj');
      b.inOrder(zutto, adj, 3);
      b.captureSpan('ずっと', zutto, adj);
    },

    // Pattern 4: ずっと + noun (not spatial/action nouns)
    // ずっと昔, ずっと気分, ずっと年上, ずっと大人らしい
    // Note: Must not match action nouns like 勉強 (ずっと1) or spatial nouns like 一緒 (ずっと1)
    //
    // GiNZA parsing notes:
    // -ずっと1 (continuous): 一緒 (dep=obl, modifying verb), 勉強 (pos=VERB, not NOUN)
    // -ずっと2 (comparison): 昔 (dep=ROOT/advmod), 気分 (dep=nsubj), 年上 (dep=nmod/acl), 大人 (dep=ROOT)
    //
    // Discriminator: Exclude dep=obl (modifiers for verbs like "together with")
    (b) => {
      const noun = b.noun({
        depOneOf: ['root', 'advmod', 'nsubj', 'nmod', 'acl', 'compound'],
      }, 'noun');
      b.inOrder(zutto, noun, 2);
      b.captureSpan('ずっと', zutto, noun);
    },

    // Pattern 5: ずっと + adverb
    // ずっとよく見える
    (b) => {
      const adv = b.adv({}, 'adv');
      b.inOrder(zutto, adv, 2);
      b.captureSpan('ずっと', zutto, adv);
    },

    // Pattern 6: ずっとずっと (emphatic doubled form)
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
