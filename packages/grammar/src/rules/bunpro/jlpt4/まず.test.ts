import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './まず.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT match
  // さいしょに (saisho ni) - first (in time/space/rank)
  '最初に日本語を勉強しました。',
  '最初に自己紹介をしてください。',
  '最初に行きたいところはどこですか。',

  // はじめに (hajime ni) - at first/in the beginning
  'はじめに説明しました。',
  'はじめに挨拶をしましょう。',
  'はじめにこれを見てください。',

  // だんだん (dandan) - gradually
  'だんだん暑くなってきました。',
  'だんだん慣れてきました。',

  // どんどん (dondon) - rapidly/progressively
  'どんどん大きくなる。',
  'どんどん上手になってきた。',

  // ますます (masumasu) - more and more
  'ますます面白くなる。',
  'ますます寒くなります。',

  // そろそろ (sorosoro) - about to/sometime soon
  'そろそろ帰りましょう。',
  'そろそろ春ですね。',

  // とうとう (toutou) - finally/eventually
  'とうとう終わりました。',
  'とうとう来ました。',

  // はじめて (hajimete) - for the first time
  'はじめて日本に行きました。',
  'はじめて食べました。',

  // さきに (saki ni) - before/earlier
  'さきに行ってください。',
  'さきに食べてしまいました。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
