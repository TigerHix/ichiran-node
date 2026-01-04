import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なさる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Regular する (to do) - not honorific
  '勉強する。',
  '明日は何をするつもりですか。',
  '仕事をする。',
  '掃除をしました。',
  '何をしていますか。',

  // Other honorific verbs (not なさる)
  '先生はいらっしゃいます。',   // いらっしゃる (honorific of いる/くる/いく)
  '社長はおっしゃいました。',   // おっしゃる (honorific of 言う)

  // なさい (imperative form, different grammar point)
  '座りなさい。',
  '早くしなさい。',
  '勉強しなさい。',

  // Negative form of regular verbs (not honorific)
  'しないでください。',
  '勉強しなくてもいいです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
