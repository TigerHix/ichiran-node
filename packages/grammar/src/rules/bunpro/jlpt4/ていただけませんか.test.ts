import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ていただけませんか.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // て form without いただけませんか (plain request)
  '見てください。',
  '買ってください。',
  // てくれる (different grammar - someone does something for you)
  '彼が私を手伝ってくれた。',
  '友達が貸してくれた。',
  // てあげる (different grammar - I do something for someone)
  '私は彼に本を買ってあげた。',
  '手伝ってあげましょう。',
  // てもらう (different grammar - have someone do something)
  '彼に英語を教えてもらった。',
  '兄に手伝ってもらった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
