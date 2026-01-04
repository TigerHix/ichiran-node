import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それで.js';
import { BUNPRO_JLPT4 } from './index.js';

// False positives: sentences with それ/で that should NOT match the conjunction pattern
const negatives = [
  // それ as object + different particle (not それで)
  'それを持ってきた。',       // brought that (wo, not de)
  'それを見せてください。',  // please show that (wo, not de)
  'それを買った。',           // bought that (wo, not de)
  'それについて話した。',     // talked about that (ni tsuite, not de)
  'それから作った。',         // made from that (kara, not de)
  // それ as topic
  'それはいいですね。',       // that's good (wa, not de)
  'それも好きです。',         // I also like that (mo, not de)
  // で without それ (different grammar)
  '東京で行きましょう。',     // let's go in Tokyo (different de)
  '鉛筆で書いた。',           // wrote with pencil (different de)
  // Copula sentences (not conjunction)
  'これはそれです。',         // this is that (copula, not sorede)
  '私のそれです。',           // it's mine (copula, not sorede)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
