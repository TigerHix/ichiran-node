import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './すでに.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // もう (mou) - informal "already" (different grammar point)
  // We can distinguish by lemma: もう vs すでに
  'もう昼ごはんを食べました。',
  'もう帰ります。',
  'もう春ですね。',

  // まだ (mada) - antonym meaning "still" or "not yet"
  'まだ雨が降っている。',
  'まだ勉強しています。',
  'まだ昼ごはんを食べていない。',

  // ついに (tsuini) - "finally" or "at last" (different meaning)
  'ついに完成しました。',
  'ついに会えました。',

  // とっくに (tokkuni) - "long ago" or "way back"
  // Similar meaning but less formal
  'とっくに終わりました。',

  // また (mata) - "again" (different meaning)
  'また明日会いましょう。',
  'また来てください。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
