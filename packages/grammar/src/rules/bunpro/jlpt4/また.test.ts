import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './また.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT match
  // まだ (mada) - not yet/still (different meaning)
  'まだ勉強しています。',
  'まだ雨が降っています。',
  'まだ来ていません。',

  // もう (mou) - already (different meaning)
  'もう食べました。',
  'もう来ました。',
  'もう時間があります。',

  // さらに (sara ni) - furthermore/even more (more formal)
  'さらに勉強が必要です。',
  'さらに問題が複雑になりました。',

  // ふたたび (futatabi) - once again/second time (more formal/literary)
  'ふたたび会いたい。',

  // もっと (motto) - more (comparative)
  'もっと勉強してください。',
  'もっと速く走って。',

  // もし (moshi) - if (conditional, completely different meaning)
  'もし雨が降ったら、行きません。',

  // もろとも (morotomo) - along with/together (different word)
  '家もろとも燃えた。',

  // もやもや (moyamoya) - feeling of uneasiness (onomatopoeia)
  '胸がもやもやする。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
