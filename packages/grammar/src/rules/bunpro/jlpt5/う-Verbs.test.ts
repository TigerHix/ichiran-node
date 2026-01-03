import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './う-Verbs.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that can't be matched due to being cloze questions with arrows:
//
// These are not natural Japanese sentences but conjugation exercise prompts
// showing: dictionary form → polite form. The "→" symbol is not Japanese
// punctuation and GiNZA cannot parse it meaningfully.
//
// Examples:
//   会う → あいます (au → aimasu)
//   あるく → 歩きます (aruku → arukimasu)
//
// These are teaching materials, not actual sentences to parse.
// The rule correctly matches u-verbs in real sentences like:
//   私はパンクも聞く。
//   学生たちは先生の話も聞きます。
const skipPositives = [
  '会う → あいます',
  'あるく → 歩きます',
];

// Negative test cases: verbs that should NOT match the u-verb pattern
const negatives = [
  // ru-verbs (ichidan) - should not match
  '食べる。', // taberu - ichidan verb
  '見る。', // miru - ichidan verb
  '起きる。', // okiru - ichidan verb

  // Irregular verbs - should not match
  'する。', // suru - irregular (サ行変格)
  '勉強する。', // benkyousuru - irregular compound
  '来る。', // kuru - irregular (カ行変格)
  '来ます。', // kimasu - irregular polite form
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
