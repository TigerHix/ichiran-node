import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: かれ-かれ (either X or Y; regardless of which)
 *
 * Literary expression pairing opposite い-adjectives: "whether X or Y, result is the same"
 *
 * Examples:
 * - 遅かれ早かれ (sooner or later)
 * - 多かれ少なかれ (more or less)
 * - 良かれ悪しかれ (for better or worse)
 * - 暑かれ寒かれ (hot or cold)
 *
 * Pattern: Adj-stem-かれ + Adj-stem-かれ (two consecutive classical adjective imperatives)
 *
 * GiNZA parse structure (when at sentence start):
 * - 遅かれ早かれ: 遅かれ (NOUN/ADJ, tag=形容詞-一般, inflectionForm=命令形) + 早かれ (same)
 * - 多かれ少なかれ: 多かれ (ADJ, tag=形容詞-一般, inflectionForm=命令形) + 少なかれ (same)
 * - よかれあしかれ: よかれ (PRON/NOUN, inflectionForm=命令形) + あしかれ (same)
 *
 * Key discriminators:
 * 1. inflectionForm="命令形" (classical imperative form)
 * 2. tag="形容詞-一般" or "形容詞-非自立可能" (adjective-like)
 * 3. pos can be NOUN, ADJ, or PRON (inconsistent by GiNZA)
 * 4. lemma ends in "し" (classical adjective stem, e.g., 遅し, 多し, よし)
 *
 * The 〜かれ suffix is the imperative form contraction of classical く + あれ → かれ
 *
 * NOTE: Each Adj-かれ is a SINGLE token in GiNZA (e.g., "遅かれ" is one token, not "遅"+"かれ")
 */
export default linguisticRule('かれ-かれ', (r) => {
  // First adjective + かれ (classical imperative form, single token)
  // textOneOf provides literal triggers for dispatch
  const adj1 = r.tok({
    posOneOf: ['NOUN', 'ADJ', 'PRON'],
    inflectionForm: '命令形',
    tagOneOf: ['形容詞-一般', '形容詞-非自立可能'],
    textOneOf: ['遅かれ', '早かれ', '多かれ', '少なかれ', '良かれ', '悪しかれ', '暑かれ', '寒かれ', 'よかれ', 'あしかれ'],
    lemmaRe: /し$/,   // Classical adjective stem ends with し
  }, 'adj1');

  // Second adjective + かれ (classical imperative form, single token)
  const adj2 = r.tok({
    posOneOf: ['NOUN', 'ADJ', 'PRON'],
    inflectionForm: '命令形',
    tagOneOf: ['形容詞-一般', '形容詞-非自立可能'],
    textOneOf: ['遅かれ', '早かれ', '多かれ', '少なかれ', '良かれ', '悪しかれ', '暑かれ', '寒かれ', 'よかれ', 'あしかれ'],
    lemmaRe: /し$/,   // Classical adjective stem ends with し
  }, 'adj2');

  // Consecutive pairs
  r.inOrder(adj1, adj2, 1);

  // Capture the full span (e.g., "遅かれ早かれ")
  r.captureSpan('かれ-かれ', adj1, adj2);
});
