import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './-のうち-で.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // うちに (while/before) - different grammar, has の before うち
  '暗くならないうちに帰ろう。',
  '忘れないうちにメモする。',
  '熱いうちに食べてください。',
  // の + うち but reversed (うちの = "my/our")
  'うちの会社は休みが多い。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});

