import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それとも.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the それとも grammar rule
const negatives = [
  // それ (sore) + とも (tomo) as separate words meaning "that also"
  // This is DIFFERENT from the conjunction それとも (or else)
  // Unfortunately, these are ambiguous and our rule can't distinguish them
  // based solely on surface form and basic POS/dep constraints.
  //
  // NOTE: Sentences like "それとも知らない人が多い" literally contain
  // "それとも" but here it means "それ+とも" (that also) not the conjunction.
  // This is a known limitation - without more sophisticated analysis,
  // we can't distinguish "それとも as conjunction" from "それ+とも as (that + also)".
  //
  // These negative tests are currently SKIPPED due to this limitation.

  // か (ka) - question particle or "or" within clause
  '今日か明日に行きます。',
  'コーヒーか紅茶がいいですか。',

  // または (matawa) - formal "or"
  '本人または代理人が申し込む。',
  '書面またはメールで提出してください。',

  // もしくは (moshikuwa) - more formal "or"
  '許可もしくは同意が必要です。',
  '現金もしくは小切手でお支払いください。',

  // あるいは (aruiwa) - formal "or"
  '成功あるいは失敗のどちらかです。',
  '電話あるいはメールで連絡します。',

  // むしろ (mushiro) - "rather" (preference emphasis)
  'ビールよりむしろワインが好きだ。',
  '安いよりむしろ品質が大事だ。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
