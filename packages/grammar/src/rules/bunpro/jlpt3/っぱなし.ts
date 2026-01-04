import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: っぱなし (leave in state/neglect)
 *
 * Verb stem (masu form) + っぱなし = leave X as is / keep doing X
 * Usually negative: leaving something on, in a state, neglected
 *
 * Examples:
 * - 開けっぱなし (left open)
 * - 立ちっぱなし (keep standing)
 * - つけっぱなし (left on)
 * - 出しっぱなし (left running/out)
 *
 * Key patterns:
 * 1. Verb stem (連用形-一般) + っぱなし/っ放し
 *
 * GiNZA parse structure:
 * - 開けっぱなし: 開け (VERB, inflectionForm=連用形-一般) + っぱなし (PART, tag=接尾辞-形状詞的)
 * - つけっぱなし: つけ (VERB, inflectionForm=連用形-一般) + っぱなし (PART, tag=接尾辞-形状詞的)
 * - 立ちっ放し: 立ち (VERB, inflectionForm=連用形-一般) + っ放し (PART, tag=接尾辞-形状詞的)
 * - 出しっぱなし: 出し (VERB, inflectionForm=連用形-一般) + っぱなし (PART, tag=接尾辞-形状詞的)
 *
 * The っぱなし suffix is parsed as:
 * - PART (particle) with tag=接尾辞-形状詞的 (na-adjective-like suffix)
 * - Can be written as っぱなし or っ放し
 * - lemma can be "っぱなし" or "っ放し"
 *
 * This rule matches verb stems (連用形-一般) + っぱなし/っ放し suffix
 */
export default linguisticRule('っぱなし', (r) => {
  // Verb stem (masu form, 連用形-一般)
  const stem = r.tok({
    inflectionForm: '連用形-一般',
  }, 'stem');

  // っぱなし as suffix (PART, tag=接尾辞-形状詞的)
  // Can be written as っぱなし or っ放し
  const ppanashi = r.tok({
    textOneOf: ['っぱなし', 'っ放し'],
    tag: '接尾辞-形状詞的',
  }, 'ppanashi');

  r.inOrder(stem, ppanashi, 1);
  r.captureSpan('っぱなし', stem, ppanashi);
});
