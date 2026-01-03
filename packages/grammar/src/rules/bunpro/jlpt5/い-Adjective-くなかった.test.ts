import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './い-Adjective-くなかった.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Positive past (～かった) - not negative
  'たのしかった。',
  'あつかった。',
  'おもしろかったです。',
  // Negative non-past (～くない) - not past
  'たのしくない。',
  'あつくないです。',
  'おもしろくない。',
  // na-adjective negative past (じゃなかった) - not i-adjective
  '静かじゃなかった。',
  'きれいじゃなかったです。',
  // Verb negative past - not adjective
  '行かなかった。',
  '食べなかった。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
