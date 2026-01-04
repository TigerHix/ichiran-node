import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './から見ると.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  '映画を見ました。',
  'あなたがいるから、安心できます。',
  '子供にしたら大問題だ。',
  '子供にしては詳しい。',
  '彼の性格からして、彼と一緒に住むことは無理だろう。',
  '彼の話からすると、嘘をついているようだ。',
  '経験からいうとそれは無理だ。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
