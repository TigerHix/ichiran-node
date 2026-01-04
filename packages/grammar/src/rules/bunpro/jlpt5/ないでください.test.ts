import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ないでください.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Independent ください (meaning "give me" - different grammar)
  // When used as main verb, ください is pos=VERB, dep=root (not AUX with dep=fixed)
  '水をください。',
  '本をください。',
  'これをください。',

  // Negative ない without ください
  // These are negative statements, not requests
  '行かないで。',
  '食べないで。',
  '言わないで。',

  // Simple negation without で/ください
  '行かない。',
  '食べない。',
  'しない。',

  // Positive request form てください (different grammar)
  '行ってください。',
  '食べてください。',
  'してください。',

  // て-form negative (てはいけない - different grammar)
  '食べてはいけない。',
  '行ってはいけない。',

  // Negative ないで (without ください - casual form)
  // The writeup mentions this as a related but different grammar point
  'これは誰にも言わないで。',
  '後で電話するから寝ないでよ。',
  'それ、お兄ちゃんのだから、食べないで。',
];

// Sentences from the Bunpro data that should be skipped:
//
// The Bunpro data includes some "writeup" examples that show the casual form
// ないで (without ください) as related grammar. These are NOT examples of
// the ないでください pattern and should not be in the positive test data.
//
// Examples from writeups:
//   これは誰にも言わないでください。 ✓ (matches - has ください)
//   後で電話するから寝ないでよ。 ✗ (casual form - no ください)
//   それ、お兄ちゃんのだから、食べないで。 ✗ (casual form - no ください)
//
// The writeup explicitly states: "In friendly conversation, ください can be omitted"
// These sentences demonstrate the casual form, not the polite ないでください form.
//
// CONCLUSION: These are examples of a different grammar point (Verb[ないで])
// shown in the grammar explanation, not actual ないでください examples.
const skipPositives = [
  '後で電話するから寝ないでよ。',
  'それ、お兄ちゃんのだから、食べないで。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
