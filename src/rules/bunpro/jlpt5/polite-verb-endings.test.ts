import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './polite-verb-endings.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Casual verb forms (dictionary form)
  '食べる',
  '行く',
  '飲む',
  // Casual verb forms (past tense)
  '食べた',
  '行った',
  '飲んだ',
  // Casual verb forms (negative)
  '食べない',
  '行かない',
  '飲まない',
  // Casual verb forms (past negative)
  '食べなかった',
  '行かなかった',
  '飲まなかった',
  // Casual verb forms (te-form)
  '食べて',
  '行って',
  '飲んで',
  // Adjective + です (different grammar rule)
  '部屋は静かです。',
  '今日は暑いです。',
  // ru-verbs in polite form (these should still match)
  // '食べます',  // This should match
  // '見ます',     // This should match
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
