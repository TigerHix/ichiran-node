import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './どんどん.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT match
  // だんだん (dandan) - gradually/slowly (slower progression)
  'だんだん雨が降ってきた。',
  'だんだん勉強してください。',
  'だんだん大きくなる。',
  'だんだん暑くなる。',
  'だんだんピアノが上手になってきた。',

  // ますます (masumasu) - more and more (increasing)
  'ますます寒くなる。',
  'ますます面白くなる。',

  // じょじょに (jojo ni) - gradually (more formal)
  'じょじょに寒くなる。',
  'じょじょに慣れてきました。',

  // そろそろ (orosoro) - about to/sometime soon (different meaning)
  'そろそろ帰りましょう。',
  'そろそろ春ですね。',

  // とうとう (toutou) - finally (different meaning)
  'とうとう来た。',
  'とうとう終わった。',

  // とんとん (tonton) - steadily/rapidly (different onomatopoeia)
  'とんとんと売れる。',
  'とんとんと進む。',
  'とんとんぶるーにのる',

  // たんたん (tantan) - plain/simple (different word)
  'たんたんとした味。',

  // どうどう (doudou) - grandly/publicly (different word)
  'どうどうと宣言する。',

  // Other onomatopoeic adverbs
  'ばりばり働く。',
  'きちんと片付ける。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
