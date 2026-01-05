import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がけに.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the がけに grammar rule
// These test similar expressions and related grammar patterns
const negatives = [
  // 途中で/途中に (tochuu de/ni) - "on the way, in the middle of"
  // Different grammar: attaches to dictionary form, not verb stem
  '帰る途中で友達に会った。',
  '学校へ行く途中にコンビニによる。',
  '来る途中で雨が降ってきた。',
  '家に帰る途中で事故を見た。',

  // ついでに (tsuide ni) - "while you're at it, on the occasion"
  // Different grammar: focus on opportunity/convenience, attaches to noun+の
  '買い物のついでに郵便局による。',
  '出張のついでに観光する。',
  '駅に来るついでにこれを買ってきた。',

  // 単純な移動動詞 + に (simple movement verbs + particle ni)
  // Different grammar: に is just destination marker, not がけに
  '家に帰る。',
  '学校に行く。',
  '駅に来る。',
  'そこに通る。',

  // が + 何か (ga + nanika) - different grammar
  '何かある。',
  '何か買う。',

  // かけ (kake) - other uses of 掛ける
  // Different grammar: various meanings of verb 掛ける
  '眼鏡をかける。',
  '電話をかける。',
  '時間をかける。',
  '鍵をかける。',

  // かけに (kake ni) - potential form + particle
  // Different grammar: potential verb form + に
  '彼が行けに行く。',
  '見えに見える。',

  // Similar sounding but unrelated words
  '金がある。',
  '彼にかなう。', // kanau (to match)

  // Verb stem + other suffixes (not がけ)
  '帰りながら電話する。',
  '行きそうな店。',
  '来たばかりの人。',

  // Verb te-form + に (te-form + ni)
  '帰ってくる。',
  '行きたい。',

  // Other uses of に as particle
  '東京に行く。',
  '彼に会う。',
  '家にいる。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
