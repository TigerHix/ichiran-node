import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かは-によって違う.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences with similar particles that should NOT match
const negatives = [
  // Simple か + は without によって (just question + topic)
  'それが正しいかは分かりません。',
  'いつ行くかはまだ決めていません。',

  // かどうか + は without によって (question clause without "depending on")
  'できるかどうかは疑問です。',

  // によって without 違う (different grammar - "by means of", "due to")
  'バスによって通勤しています。',
  '彼の成功は努力によってもたらされた。',

  // 違う without かは～によって (plain "different")
  'それは私の考えとは違う。',
  '彼の意見は私とは違う。',

  // による without かは (nominalization: "by/through")
  'この製品は特許によって保護されている。',
  '投票によって決定します。',

  // は used as topic marker without question dependency
  '日本は場所によって文化が違う。',  // Topic は but no か question

  // 違う used in other contexts
  '意見が違うから議論した。',  // "Different opinions", not "depends on"
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
