import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './っぱなし.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences that should NOT match
const negatives = [
  // まま (similar grammar - "left as is" but neutral tone)
  '電気をつけたまま寝た。',
  'ドアを開けたままにした。',
  '窓を開けたまま寝ています。',

  // てある (similar grammar - done for future benefit)
  '窓が開けてある。',
  '電気がつけてあります。',

  // てばかり (similar - keeps doing X repeatedly)
  '彼は寝てばかりいる。',
  '食べすぎてばかりだ。',

  // がち (similar - tendency to)
  '遅刻しがちだ。',
  'さぼりがちな人。',

  // Other patterns with verb stems but different suffixes
  '食べすぎた。', // -すぎ (too much)
  '飲み始めた。', // -始める (start)
  '読み終わった。', // -終わる (finish)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
