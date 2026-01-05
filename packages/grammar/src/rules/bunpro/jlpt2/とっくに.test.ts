import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とっくに.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the とっくに grammar rule
const negatives = [
  // もう (mou) - "already" (neutral, everyday usage)
  'もう食べた。',
  'もう春が来た。',
  'もう帰りました。',

  // すでに (sude ni) - "already" (more formal, neutral)
  'すでに完成しました。',
  'すでに期限が過ぎている。',
  'その事実はすでに知っている。',

  // とっく (tokku) - different word (not the adverb)
  // No common false positives here as とっく is not a standalone word

  // に (ni) - particle alone
  '東京に行く。',
  '友達に会う。',
  '日本語を勉強している。',

  // Similar adverbs with time meanings but different nuances
  // いつの間にか (itsu no ma ni ka) - "unnoticed, before realizing"
  'いつの間にか夜になっていた。',
  'いつの間にか春が来ていた。',

  // とっくには (tokkuni wa) - とっくに + topic particle
  // (This would match since we capture just the adverb itself)
  // But it's a valid use of the grammar

  // とうに (tou ni) - different adverb meaning "long ago" (literary)
  // Note: とっくに is the emphasized form of とうに
  // They are essentially the same grammar point, so we don't test against it

  // とうの昔に (tou no seneni) - "long, long ago"
  'それはとうの昔の話だ。',
  'その時代はとうの昔に終わった。',

  // ずっと前から (zutto mae kara) - "for a long time"
  'ずっと前から知っていた。',
  'ずっと前から待っていた。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
