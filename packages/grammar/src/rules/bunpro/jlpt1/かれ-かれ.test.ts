import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かれ-かれ.js';
import { BUNPRO_JLPT1 } from './index.js';

/**
 * Negative examples: similar patterns that should NOT match
 *
 * The かれ-かれ pattern has several distinctive features:
 * 1. Classical imperative form (命令形) of adjectives
 * 2. Consecutive pairs: Adj-かれ + Adj-かれ
 * 3. Literary/archaic expression
 */
const negatives = [
  // Modern かれ patterns (not classical imperative)
  '彼は彼女が好きかもしれない。',
  '彼かれの問題ではない。',
  // か as question particle, not かれ
  '行くか行かないか、迷っています。',
  // Separate usages of かれ
  '彼れと彼れの意見が違う。',
  // Imperative verbs ending in れ (not かれ pattern)
  '入れてれと言われた。',
  // Adjective + か (question marker) + れ (not かれ pattern)
  '暑いか寒いか、わからない。',
];

/**
 * GiNZA parsing limitations for かれ-かれ pattern:
 *
 * The pattern is parsed correctly ONLY when it appears at the START of a sentence
 * or in specific contexts. When it appears in the middle of sentences with hiragana,
 * GiNZA misparses it:
 *
 * CORRECT parses (2 tokens, both with inflectionForm=命令形):
 * - 遅かれ早かれ病気になる → 遅かれ (NOUN, 命令形) + 早かれ (NOUN, 命令形) ✓
 * - 多かれ少なかれ誰もが → 多かれ (ADJ, 命令形) + 少なかれ (ADJ, 命令形) ✓
 * - 良かれ悪しかれ、大きな → 良かれ (NOUN, 命令形) + 悪しかれ (ADJ, 命令形) ✓
 * - 暑かれ寒かれ、脱水症状 → 暑かれ (NOUN, 命令形) + 寒かれ (NOUN, 命令形) ✓
 *
 * INCORRECT parses (split into multiple tokens):
 * - ...おそかれはやかれ... → おそかれ (NOUN, 命令形) + は (PARTICLE) + やか (VERB) + れ (AUX) ✗
 * - ...おおかれすくなかれ... → お (NOUN) + おか (VERB) + れ (AUX) + すく (ADJ) + なかれ (ADJ, 命令形) ✗
 *
 * The discriminator is inflectionForm=命令形 on both adjacent tokens. GiNZA only assigns
 * this correctly when the pattern is at sentence start. When embedded in hiragana context,
 * it incorrectly tokenizes "はやかれ" as "は" + "やかれ" (reading it as potential verb form).
 *
 * No reliable discriminator exists for the incorrect parses - matching would cause overcapture
 * on unrelated verb patterns (e.g., おく + れる = 置かれる "can be placed").
 */
const skipPositives = [
  'ストレスをアルコールで発散するような生活を続けていたらおそかれはやかれ病院送りになってしまうよ。',
  '私は、誰もがおおかれすくなかれ人には言えない秘密を隠し持っていると考えています。',
  'おそかれはやかれ有人火星探査の時代はやってくるはずだ。',
  '毎日必死に日本語の勉強をしているのだから、おそかれはやかれあなたが日本語を習得する日が来るでしょう。',
  'あなたが他人に対してとる態度は、 よかれあしかれ、いずれ自分に返ってくるものだと思いなさい。',
  'よかれあしかれ、確かに社会がキャッシュレス化する傾向が続いている。',
  '好奇心はおおかれすくなかれ誰もが持っているものだろう。',
  '違法薬物の使用を続けている彼女は、おそかれはやかれ後悔する事になるだろう。',
  '「こんなにタバコを吸い続けてはおそかれはやかれ病気になるだろう。」',
  'おおかれすくなかれ、どんな親も子育てに対して常に悩みを持っているものです。',
  'この発見はよかれあしかれ、大きな変化をもたらすだろう。',
  '研究によるとあらゆる頭部外傷はおおかれすくなかれ脳に影響を及ぼすということだ。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives, skipPositives });
});
