import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './お-になる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Regular になる (becoming) - no honorific prefix
  '彼は先生になる。',
  '春になります。',
  '彼女は医者になりたい。',
  // Noun + に + なる (result/change of state)
  '水は氷になる。',
  '彼は有名になった。',
  // Separate words (not honorific construction)
  'お茶にする。',
  'ご飯を食べる。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
