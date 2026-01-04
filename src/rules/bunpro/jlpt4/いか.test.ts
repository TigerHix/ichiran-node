import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いか.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 以上 (ijou) - opposite meaning (more than or equal to)
  '１００人以上が来ました。',
  '３０万円以上の車を買った。',
  // いがい (igai) - different meaning (other than)
  'これ以外に方法がない。',
  '日曜以外は働きます。',
  // みまん (miman) - different meaning (less than, not including)
  '１８歳未満の方はご遠慮ください。',
  // Sentences without いか at all
  '１００人来ました。',
  'この値段は安いです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
