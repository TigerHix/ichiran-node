import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ているあいだに.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // ている without 間に (just progressive form, not "while")
  '彼は今勉強している。',
  '私は本を読んでいる。',
  '雨が降っている。',
  // 間に without いる (different grammar - just "during/within")
  '一時間の間に終わらせてください。',
  '来週の間に返事をします。',
  // 間 (aida) without に (just "interval/duration", not temporal marker)
  '長い間待ちました。',
  'ずっと間が空きました。',
  // に as directional particle (different grammar)
  '友達に行く。',
  '家に帰る。',
  // Similar but different grammar: てから (after doing)
  '勉強してから遊びます。',
  '食事をしてから出かけます。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
