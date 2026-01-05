import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それとも.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the それとも grammar rule
const negatives = [
  // それ (sore) + とも (tomo) as separate words
  // "That also" - different grammar
  'それは彼とも共有しています。',
  'それとも知らない人が多い。',

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

  // それか (soreka) - "or that" (less formal)
  'それか、別の方法を探してみよう。',
  'バスで行くか、それか歩いて行く。',

  // むしろ (mushiro) - "rather" (preference emphasis)
  'ビールよりむしろワインが好きだ。',
  '安いよりむしろ品質が大事だ。',

  // Similar sounding fragments
  'それともう一度考えてみて。',
  'それとももちろんです。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
