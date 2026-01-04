import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なし.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // ない (regular negation of ある/verb)
  // This is different from なし which is a suffix meaning "without"
  'お金がない。',
  '彼が来ない。',

  // ぬきで (JLPT2) - similar meaning but different grammar
  // わさびぬきでお願いします (remove wasabi please)
  // Note: This has similar meaning but different structure

  // ことなく ( JLPT2) - "without doing" for verbs
  // Used with verb stems, not nouns

  // ないで (verb te-form negation) - "without doing"
  // 朝ご飯を食べないで学校に行きました (went to school without eating breakfast)
  // This is verb negation, not the suffix なし
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
