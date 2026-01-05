import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ぐらいなら.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // くらい as "approximately" (pos=ADP, dep=case, not followed by なら)
  '30分くらい待ってください。',
  '1000円くらいの買い物をした。',
  '彼くらい背が高い人は珍しい。',

  // なら as conditional marker without くらい
  '雨なら行きません。',
  '明日なら時間があります。',

  // なら as topic marker (different usage)
  '私ならそんなことはしません。',
  '田中さんなら来るかもしれません。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
