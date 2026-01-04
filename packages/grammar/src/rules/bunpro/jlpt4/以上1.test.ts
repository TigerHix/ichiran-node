import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './以上1.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // いか (ika) - opposite meaning (less than or equal to)
  '１００人以下が来ました。',
  '３０万円以下の車を買った。',
  // いがい (igai) - different meaning (other than)
  'これ以外に方法がない。',
  '日曜以外は働きます。',
  // まで (made) - different meaning (until)
  '１０時まで勉強しました。',
  // Sentences without 以上 at all
  '１００人来ました。',
  'この値段は安いです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
