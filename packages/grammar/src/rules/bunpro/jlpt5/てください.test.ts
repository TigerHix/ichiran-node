import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てください.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Independent ください (meaning "give me" - different grammar)
  // When used as main verb, ください is pos=VERB, dep=root (not AUX with dep=fixed)
  '水をください。',
  '本をください。',
  'これをください。',
  '切符を二枚ください。',

  // Similar constructions that shouldn't match
  // て-form without ください
  '食べて。',
  '待って。',

  // Other auxiliary patterns
  '食べている。',
  '待っていた。',
];

// Sentences from the Bunpro data that cannot be matched:
//
// ANALYSIS: Casual te-form requests (without ください)
//
// These sentences are included in the Bunpro てください data as "writeups" examples,
// but they do NOT actually contain the てください pattern. They are examples of
// casual requests where ください is omitted (te-form only).
//
// The writeup explicitly states: "In friendly conversation, ください can be omitted"
//
// Examples:
//   クッキーを作ったから食べて。 - Because I made cookies, you can eat them.
//   ちょっとテレビを消して。 - Hey, can you turn off the TV?
//
// These sentences end in て (te-form) without ください attached.
// The rule correctly does NOT match them because there is no ください token.
//
// CONCLUSION: Data issue - these are examples in the grammar explanation (writeups)
// showing the casual form, not actual てください examples. They should not be in
// the positive test data.
const skipPositives = [
  'クッキーを作ったから食べて。',
  'ちょっとテレビを消して。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
