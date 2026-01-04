import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だれ.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: similar pronouns and question words that should NOT match
const negatives = [
  // Demonstrative pronouns (not interrogative)
  'これですか。',              // kore (this)
  'それですか。',              // sore (that)
  'あれですか。',              // are (that over there)
  'ここにいます。',            // koko (here)
  'そこに行きます。',           // soko (there)
  'あそこです。',              // asoko (over there)

  // Other question words (not "who")
  '何ですか。',                // nani/nan (what)
  'どこですか。',              // doko (where)
  'いつ行きますか。',          // itsu (when)
  'どれですか。',              // dore (which one)
  'どうですか。',              // dou (how)
  'なぜですか。',              // naze (why)
  'いくつですか。',            // ikutsu (how many)

  // どなた should be handled separately as it's the polite form
  // (but our rule includes it as an alternative, so we DON'T test it as negative)
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
