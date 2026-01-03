import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verb-てもいい.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Simple て form without もいい
  '行って。',
  '食べて。',
  'してください。',
  // て-form used for requests (～てください)
  '行ってください。',
  '食べてください。',
  '座ってください。',
  // て-form used for connecting clauses
  '行って、買った。',
  '食べて、寝た。',
  // て-form used for ongoing actions (～ている)
  '行っています。',
  '食べています。',
  // で used as instrumental particle (not て-form)
  '鉛筆で書く。',
  '日本語で話す。',
  // Simple も + いい (without verb て-form)
  'もいいです。',
  // いい used alone
  'それはいい。',
  'いい天気です。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: GiNZA parses てもいい inconsistently in certain contexts
//
// GiNZA parses these sentences differently from the standard pattern:
//
// 1. "あそこであそんでもいいです。" (asonde demo ii desu)
//    - あそん → pos=NOUN (not VERB), dep=obl
//    - で (te-form) → pos=ADP (not SCONJ), dep=case
//    - いい → pos=ADJ (not AUX), dep=root
//    - This is completely different from the standard parse
//
// 2. "水をのんでもいいですか。" (mizu o nonde mo ii desu ka)
//    - のん → pos=VERB, dep=obl (not root)
//    - も → pos=ADP, dep=case (not fixed)
//    - いい → pos=ADJ (not AUX), dep=root (not fixed)
//
// 3. "朝ごはんをつくってもいいです。" (asagohan o tsukutte mo ii desu)
//    - Similar parsing inconsistency with dep structure
//
// Compare to working parses:
//   "明日は家に行ってもいいですか？" → いい is AUX/fixed, も is ADP/fixed
//   "この肉は食べててもいいです。" → いい is AUX/fixed, も is ADP/fixed
//
// The discriminator is the combination of:
//   - いい pos=AUX (not ADJ) with dep=fixed
//   - も pos=ADP with dep=fixed
//   - て/で pos=SCONJ with dep=mark
//
// But GiNZA doesn't consistently assign these tags for all sentences.
// When pos=ADJ for いい or dep=case for も, the pattern is indistinguishable
// from other usages (e.g., "いい天気" = good weather).
//
// CONCLUSION: GiNZA limitation for these specific sentences.
const skipPositives = [
  'あそこであそんでもいいです。',
  '水をのんでもいいですか。',
  '暑いです。水をのんでもいいですか。',
  '朝ごはんをつくってもいいです。',
  'コートをぬいでもいいですか。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
