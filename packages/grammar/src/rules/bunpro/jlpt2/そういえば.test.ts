import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そういえば.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Other discourse markers and conjunctions that should NOT match
  'ところで、昨日彼に会ったの？', // ところで is a different discourse marker
  'ちなみに、今日は誕生日です。', // ちなみに is a different discourse marker
  'それに、昨日も雨が降っていた。', // それに is a conjunction meaning "and/besides"
  'その上、値段も安い。', // その上 is a conjunction
  'さらに、問題が発生した。', // さらに is a conjunction

  // いえば without そう (e.g., といえば pattern - different grammar)
  '夏といえば花火だね。', // といえば follows a noun
  '彼といえば、最近元気だ。', // といえば with different meaning

  // Similar patterns but not そういえば
  'そうですね、明日は雨です。', // そうですね (agreement, different)
  'そうすると、彼も来るでしょう。', // そうすると (consequence)
  'そうしたら、彼も来た。', // そうしたら (sequence/consequence)

  // Verb いう in different contexts
  '彼はそう言った。', // 言った (past tense of "to say")
  'どういう意味ですか。', // どういう (what kind of)
  'いうまでもなく。', // いうまでもなく (needless to say)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
