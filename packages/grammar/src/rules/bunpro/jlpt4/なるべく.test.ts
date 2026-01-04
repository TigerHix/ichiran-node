import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なるべく.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT match
  // できるだけ (dekirudake) - to the full extent possible (more direct/forceful)
  'できるだけ早く来てね。',
  'できるだけ勉強してください。',
  'できるだけ多くの人に会いたい。',

  // できれば (dekireba) - if possible (different grammar)
  'できれば来てください。',
  'できれば手伝ってください。',

  // だんだん (dandan) - gradually (slower progression)
  'だんだん暑くなる。',
  'だんだん大きくなる。',

  // ますます (masumasu) - more and more (increasing)
  'ますます寒くなる。',
  'ますます面白くなる。',

  // なるほど (naruhodo) - I see / indeed (different word)
  'なるほど、そうでしたか。',
  'なるほど面白い。',

  // なる (naru) - to become (verb, not adverb)
  '春になる。',
  '大人になる。',

  // べく (beku) - auxiliary meaning "in order to" (different grammar)
  '成功するべく努力する。',
  '解決すべく作業を進める。',

  // Other extent adverbs
  'とても速い。',
  'かなりよくできた。',
  'ずっと好きです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
