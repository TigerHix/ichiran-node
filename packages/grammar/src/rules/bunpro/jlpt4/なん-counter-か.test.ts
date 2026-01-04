import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なん-counter-か.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: similar-looking patterns that should NOT match
const negatives = [
  // Direct questions (sentence-ending か)
  '何人来ますか。',
  '何冊ありますか。',
  // 何 with other particles (not か)
  '何人で行きました。',
  '何人も来ました。',
  '何人にも会えませんでした。',
  // 何 as object (なに reading, not なん)
  '何を食べましたか。',
  '何が好きですか。',
  // Indefinite pronoun patterns (different grammar)
  '何か食べますか。',
  'どこかへ行きたい。',
  // Embedded questions (question-phrase-か grammar)
  '何人来るかわかりません。',
  '何時か知っていますか。',
  // Numbers + も (different grammar)
  '１０人も来ました。',
  '３冊も買いました。',
  // かどうか pattern (different grammar)
  '来るかどうかわかりません。',
];

// Positive sentences to skip due to GiNZA parsing limitations
const skipPositives = [
  // ANALYSIS: "なんだいか" (何台か - some cars)
  //
  // GiNZA tokenizes this completely inconsistently:
  //   "昨日なんだいかの車が盗まれたらしい。"
  //   Tokenization: "な"(AUX) + "ん"(PRON) + "だ"(AUX) + "いか"(NOUN) + "の"(ADP)
  //
  // The "か" is embedded inside "いか" as part of the counter reading, not as a separate particle.
  // Our pattern requires "か" to be a separate particle token.
  //
  // There is no reliable discriminator to match this without also matching:
  //   - "何を食べますか" (direct question: what will you eat?)
  //   - "何かありますか" (is there something?)
  //
  // CONCLUSION: Unmatchable due to GiNZA tokenization limitation.
  '昨日なんだいかの車が盗まれたらしい。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
