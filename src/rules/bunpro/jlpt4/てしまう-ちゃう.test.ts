import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てしまう-ちゃう.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // しまう as standalone verb meaning "to put away/store"
  '本を棚にしまう。',
  '道具を箱にしまった。',
  // ちゃん (name suffix, not contraction)
  '花ちゃんが来た。',
  // しまい (end/conclusion, noun) not しまう verb
  '最後はお開きになりました。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
