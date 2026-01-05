import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たちまち.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the たちまち grammar rule
const negatives = [
  // たち (tachi) - different word (plural suffix, reaching, etc.)
  '彼らは学生たちです。',
  '目的地にたちました。',
  '道をたちながら歩く。',

  // まち (machi) - different word (town, waiting, etc.)
  '東京は大きなまちです。',
  'バスをまちました。',
  'まちがえて電話した。',

  // Similar adverbs with different nuances
  // ただちに (tadachini) - "immediately" (formal, with control/urgency)
  'ただちに避難してください。',
  '問題をただちに修正する。',

  // いきなり (ikinari) - "suddenly, abruptly" (emphasizes lack of warning)
  '彼はいきなり部屋に入ってきた。',
  'いきなり泣き出した。',

  // すぐに (sugu ni) - "immediately, soon" (neutral, everyday usage)
  'すぐに来てください。',
  '食事をすぐに終わらせた。',

  // いつの間にか (itsu no ma ni ka) - "unnoticed, before realizing"
  'いつの間にか夜になっていた。',
  'いつの間にか眠っていた。',

  // はやく (hayaku) - "quickly, early" (general speed or time)
  'はやく行きましょう。',
  '朝はやく起きた。',

  // とつぜん (totsuzen) - "sudden" (adjective/noun, not adverb)
  'とつぜんの雨に驚いた。',
  '事故はとつぜん起こった。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
