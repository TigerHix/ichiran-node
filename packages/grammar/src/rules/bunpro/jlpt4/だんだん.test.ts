import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だんだん.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT match
  // どんどん (dondon) - rapidly/increasingly (faster progression)
  'どんどん雨が降ってきた。',
  'どんどん勉強してください。',
  'どんどん大きくなる。',
  'どんどん暑くなる。',

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

  // たんたん (tantan) - plain/simple (different word)
  'たんたんとした味。',

  // だいたい (daitai) - generally/approximately (different meaning)
  'だいたいわかった。',
  'だいたい３時間かかる。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
