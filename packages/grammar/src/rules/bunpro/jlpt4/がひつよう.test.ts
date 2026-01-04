import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がひつよう.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // が and 必要 as separate elements (not the grammar pattern)
  'お金があるから、必要なものを買う。',
  '時間があるから、必要だ。',
  // 必要がある (different grammar - verb phrase)
  '勉強する必要がある。',
  '行く必要がある。',
  // は + 必要 (topic marker, not subject marker)
  'これは必要です。',
  // に + 必要 (different particle)
  '成功に必要なもの。',
  // を + 必要 (object marker)
  '必要なものを買う。',
  // 必要 as standalone noun
  '水は必要だ。',
  // 必要 as verb complement (not copula)
  '必要とする。',
  '必要とされる。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
