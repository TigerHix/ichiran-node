import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './すら.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // さえ (similar particle but different grammar - JLPT3)
  // '子供さえできる。',
  // 'お金さえあれば幸せだ。',

  // でも (casual "even" - different particle)
  '雨でも行きます。',
  '子供でもできる。',
  '先生でも知っている。',

  // も (also/too - different particle)
  '私も行きます。',
  '子供も知っている。',
  '先生も来た。',

  // まで (even/to - different particle)
  '最後まで頑張りました。',
  '子供まで参加した。',

  // だって (casual "even" - different particle)
  '子供だってできる。',
  '私だって知っている。',

  // Simple locative で (instrumental case marker)
  '東京で働きます。',
  '鉛筆で書きます。',

  // Simple case markers (に, で without すら)
  '東京に行きます。',
  '家で食べます。',

  // こそ (emphasis particle - different grammar)
  'これこそが正解だ。',
  'あなたこそ適任者だ。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
