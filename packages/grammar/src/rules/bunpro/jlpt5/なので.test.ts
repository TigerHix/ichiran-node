import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ので.js';
import { BUNPRO_JLPT5 } from './index.js';

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, {
    negatives: [
      // Avoid false positives with locative で + topic は
      '東京では電車が便利です。',
      'ここでは食べることができません。',
      // Avoid false positives with nominal ので
      // These are different grammar patterns
    ],
  });
});
