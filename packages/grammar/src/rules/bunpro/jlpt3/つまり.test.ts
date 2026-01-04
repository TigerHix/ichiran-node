import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './つまり.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences with similar conjunctions/adverbs that should NOT match
const negatives = [
  // つもり (intention) - sounds similar but different meaning
  '旅行に行くつもりです。',
  '彼は来るつもりがない。',
  '勉強するつもりだったが、',

  // むしろ (rather/instead) - different conjunction type
  'むしろ、こちらの方が良い。',
  '失敗したからといって、むしろ勉強になったと思う。',

  // それで (therefore/so) - cause-effect conjunction
  'それで、どうしましたか？',
  '雨が降りました。それで、試合は中止です。',

  // そこで (therefore/accordingly) - response to situation
  'そこで、新しい方法を考えました。',
  '問題がありました。そこで、すぐに対応しました。',

  // さて (well/then) - topic changer
  'さて、次の話題に移りましょう。',

  // ついに (finally) - different adverb
  'ついに完成しました。',
  'ついに雨が止みました。',

  // かえって (on the contrary) - different conjunction
  '薬を飲んだら、かえって具合が悪くなった。',

  // だって (because) - casual causal conjunction
  '遅刻したんだ。だって、電車が止まったから。',

  // いわゆる (so-called) - different meaning
  'それはいわゆる「常識」というものです。',

  // 要するに (to sum up) - more formal synonym
  '要するに、時間が足りないということです。',

  // すなわち (namely/in other words) - more formal synonym
  '父、すなわちこの会の社長が決定する。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
