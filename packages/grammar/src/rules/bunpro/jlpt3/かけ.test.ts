import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かけ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: similar but different usages that should NOT match
const negatives = [
  // 掛ける (transitive "to hang/suspend") - different meaning
  '彼は絵を壁に掛けた。',
  '眼鏡を掛けて本を読む。',
  // 騎手 (kishu = jockey) - different word
  '彼は有名な騎手だ。',
  // 家計 (kakei = household budget) - different word
  '今月の家計を節約する。',
];

// Skip positives: GiNZA parsing limitations
// These are valid かけ usages that GiNZA parses in ways the rule cannot match.
const skipPositives = [
  // Nominalized かけ (verb stem + かけ used as noun before particles)
  // GiNZA tokenizes these as single compound nouns or with different POS tags
  'のみかけのジュースがあるのを忘れていた。',  // のみかけ as NOUN + の
  '飲みかけた水を捨てる。',                      // 飲みかけた as single token or different structure
  '俺の食べかけだけど大丈夫？',                 // 食べかけ + だ (copula)
  'これは私の飲みかけだ。',                      // 飲みかけ + だ (copula)
  'はがれかけているポスターを貼り直してください。', // はがれかけて (te-form + iru)
  'ふきかけたテーブルをちゃんと最後まで拭いてください。', // ふきかけた (ta-form)
  '私は、何冊もよみかけの本がある。',           // よみかけ + の (particle)
  '靴をはきかけて、靴下を履いていないことに気づいた。', // はきかけて (te-form)
  'この時計こわれかけだけど、ないよりはいいか。', // こわれかけ + だ (copula)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
