import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だいたい.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT match
  // ほとんど (hotondo) - different meaning (almost all/hardly any)
  'ほとんどの人は知っている。',
  'お金はほとんど残っていない。',

  // たいてい (taitei) - usually/generally (but different word)
  'たいてい家で食べています。',
  '日曜日はたいてい買い物に行きます。',

  // おおよそ (oyoyoso) - approximately (more formal)
  'おおよそ見当がつく。',
  'おおよそ３時間かかる。',

  // そもそも (somosomo) - in the first place (different word)
  'そもそも、なんでそんなことを言ったの？',
  'そもそもそれは間違いだ。',

  // やく (yaku) - approximately (informal)
  'やく３０人来た。',

  // だいぶ (daibu) - considerably/quite a bit (different meaning)
  'だいぶ疲れた。',
  'だいぶ寒くなった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
