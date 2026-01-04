import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './particle-の.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Simple possessive の (without other particles) - different grammar
  'これは私の本です。',
  '日本の車は人気があります。',
  '友達の家に行きます。',

  // Nominalizer の (e.g., のは, のが, のを) - different grammar
  '本を読むのが好きです。',
  '日本語を勉強するのは楽しいです。',

  // Particle combinations that DON'T take の
  '日本には多くの山があります。',
  '会社に行きます。',
  '鉛筆で書きました。',
  '東京から来ました。',
  '友達と行きます。',
  '駅まで歩きました。',

  // では as conjunction (not での)
  '東京では雨が降っています。',
  'この店ではお寿司が売られています。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// 1. 今までの対立の原因は広く知られていない。
//    GiNZA parses "今まで" as a single NOUN token (lemma=今まで), not as "今 + まで + の".
//    Compare with "これまで" which is correctly parsed as "これ + まで + の":
//      - これまでの話 → [0]これ(PRON) [1]まで(ADP,case) [2]の(ADP,case) ✓ WORKS
//      - 今までの対立 → [0]今まで(NOUN) [1]の(ADP,case) ✗ INDISTINGUISHABLE
//    The pattern requires a particle before の, but "今まで" is tokenized as a single word.
//    There is no way to distinguish "今までの" (until now) from a simple noun+の possessive.
//
// 2. あちゃんとの握手会に行こう！
//    GiNZA completely mangles this parse: "あ + ちゃんと + の" instead of "あちゃん + と + の".
//    The expected parse should be:
//      - あちゃん(NOUN) + と(ADP,case) + の(ADP,case)
//    But GiNZA produces:
//      - あ(INTJ,interjection) + ちゃんと(ADV,adverb) + の(ADP,case)
//    This is completely wrong - "ちゃんと" is parsed as an adverb meaning "properly/carefully",
//    not as part of the name "A-chan" + particle "と".
//    No discriminator exists when the tokenization itself is wrong.
const skipPositives = [
  '今までの対立の原因は広く知られていない。',
  'あちゃんとの握手会に行こう！',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
