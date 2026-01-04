import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './命令形.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match imperative form
//
// The imperative form is specifically marked by inflectionForm: '命令形'.
// We need to avoid matching other verb forms like:
// - Volitional form (意志推量形): "let's do" - e.g., 食べよう, 行こう
// - Dictionary form (終止形-一般): e.g., 食べる, 行く
// - Te-form (連用形-一般 + て): e.g., 食べて, 行って
// - Negative (ない): e.g., 食べない, 行かない

const negatives = [
  // Volitional form (different from imperative)
  '食べましょう。',
  '行こう。',
  '来よう。',
  '勉強しよう。',

  // Dictionary form
  '食べる。',
  '行く。',
  '来る。',

  // Te-form (requests, softer than imperative)
  '食べて。',
  '行って。',

  // Negative
  '食べない。',
  '行かない。',

  // Polite forms
  '食べてください。',
  '行ってください。',
  '食べなさい。',

  // Potential form
  '食べられる。',
  '行ける。',

  // Passive form
  '食べられる。',
  '行かれる。',

  // Causative form
  '食べさせる。',
  '行かせる。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
