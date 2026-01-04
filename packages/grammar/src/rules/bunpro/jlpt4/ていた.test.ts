import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ていた.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple past tense (not continuous)
  '待った。',
  '食べた。',
  '座った。',
  'した。',
  // Present continuous (ている, not ていた)
  '待っている。',
  '食べている。',
  '座っている。',
  'している。',
  // いた as standalone verb meaning "to exist" (animate objects)
  '昨日、公園に犬がいた。',
  '部屋に誰かいた。',
  // て-form + other auxiliaries (not いた)
  '食べてしまった。',
  '行ってしまった。',
  // Negative forms
  '待っていなかった。',
  'していなかった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
