import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ざる (zaru) - "not doing, un-"
 *
 * An archaic/literary negative form, the attributive form of the classical
 * auxiliary verb ず (zu). Equivalent to modern ない (nai) when modifying nouns.
 * Used primarily in set phrases and formal/literary contexts.
 *
 * Formation:
 * - Remove ない from the negative form and attach ざる
 * - Five-grade verbs (五段動詞): 知らない → 知らざる
 * - Ichidan verbs (一段動詞): 得ない → 得ざる
 * - Irregular verbs:
 *   - する → せざる (NOT しざる)
 *   - 来る → 来ざる
 *
 * Structures:
 * - Verb［negative stem］+ ざる + Noun
 *
 * Examples:
 * - 知られざる傑作がある。
 *   (There is an unknown masterpiece.)
 * - たゆまざる努力が成功につながった。
 *   (Unwavering efforts led to success.)
 * - 消えざる傷を負った。
 *   (Suffered a wound that will not disappear.)
 * - 取り返しのつかざる大変なミス。
 *   (A serious mistake that cannot be undone.)
 * - 絶えざる失敗にも負けない。
 *   (Will not lose even to unending failures.)
 *
 * Key discriminators:
 * - ざる is the attributive (rentaikei) form of classical auxiliary ず
 * - Always modifies a noun (attributive use)
 * - Literary/archaic register, used in formal contexts
 * - Found primarily in set phrases and idioms
 * - Different from:
 *   - ざるを得ない (have no choice but to - related but different grammar)
 *   - ず (classical negative in other forms like ずに)
 *   - ない (modern negative)
 *
 * GiNZA parse structure:
 * - Verb stem (negative form without ない) + ざる(AUX)
 * - ざる may be tagged as AUX, VERB, or PART depending on context
 * - Typically has dep=aux or dep=mark relation to preceding verb
 * - Verb stem is in 連用形 (ren'youkei/stem form)
 * - When modifying noun, ざる has acl/dep relationship to following noun
 *
 * Important: Matches ざる as auxiliary to verb stem to exclude:
 * - ざるを得ない (different grammar - separate rule)
 * - Independent use of ざる (rare)
 * - Modern ない forms
 */
export default linguisticRule('ざる', (r) => {
  r.either(
    // Pattern 1: ざる as auxiliary modifying a following noun
    // This is the correct attributive use: verb stem + ざる + noun
    (b) => {
      const zaru = b.aux({
        text: 'ざる',
        dep: 'aux',
      }, 'zaru');
      // ざる should modify a noun (attributive function)
      // This excludes ざるを得ない which has particle を after ざる
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      b.inOrder(zaru, noun, 3);
      b.capture(zaru);
    },

    // Pattern 2: ざる as particle (PART) with dep=mark modifying a noun
    // GiNZA sometimes parses classical auxiliaries as particles
    (b) => {
      const zaru = b.tok({
        text: 'ざる',
        pos: 'PART',
        depOneOf: ['mark', 'case'],
      }, 'zaru');
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      b.inOrder(zaru, noun, 3);
      b.capture(zaru);
    },

    // Pattern 3: ざる with lemma indicating classical negative form, modifying a noun
    // Some GiNZA parsings use different lemmas
    (b) => {
      const zaru = b.tok({
        text: 'ざる',
        lemmaOneOf: ['ざる', 'ず', 'ぬ'],
      }, 'zaru');
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      b.inOrder(zaru, noun, 3);
      b.capture(zaru);
    },

    // Pattern 4: ざる as VERB modifying a noun
    // GiNZA sometimes mis-tags classical auxiliaries as verbs
    (b) => {
      const zaru = b.verb({
        text: 'ざる',
      }, 'zaru');
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      b.inOrder(zaru, noun, 3);
      b.capture(zaru);
    },

    // Pattern 5: Verb stem + ざる + noun (full pattern)
    // Ensures it's acting as auxiliary to verb stem
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const zaru = b.tok({
        text: 'ざる',
      }, 'zaru');
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      b.inOrder(stem, zaru, 3);
      b.inOrder(zaru, noun, 3);
      b.captureSpan('ざる', stem, zaru);
    }
  );
});
