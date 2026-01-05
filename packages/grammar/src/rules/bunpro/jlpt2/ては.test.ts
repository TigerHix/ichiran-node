import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ては.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Regular te-form (sequential actions, not conditional)
  '本を読んで勉強しました。',
  '起きて服を着て、朝ごはんを食べた。',
  '駅に行って切符を買った。',

  // Simple topic marker は (not conditional)
  '私は学生です。',
  '彼は来ると思います。',
  '東京は大きい都市です。',

  // NOTE: Locative では (東京では, 日本では) is EXCLUDED from negatives because:
  // ANALYSIS: Noun + では pattern
  //
  // GiNZA parses locative では and conditional では identically:
  //   - Both: noun (dep=obl) + で (dep=case -> noun) + は (dep=case -> noun)
  //
  // Examples with identical structure:
  //   - Conditional: 給与では (noun=給与, dep=obl) + では  ✓ SHOULD MATCH
  //   - Locative:    東京では (noun=東京, dep=obl) + では  ✗ SHOULD NOT MATCH
  //   - Conditional: 結果次第では (noun=次第, dep=obl) + では  ✓ SHOULD MATCH
  //   - Locative:    店では (noun=店, dep=obl) + では  ✗ SHOULD NOT MATCH
  //
  // The difference is semantic, not structural:
  //   - Conditional: "if X, then Y (negative consequence)"
  //   - Locative:    "at X, Y happens"
  //
  // No structural discriminator exists in GiNZA parse.
  // Matching noun + では is necessary for positive examples like:
  //   - 給与では、あの工場で働くには無理がある
  //   - 結果次第では、引退もあると思いますよ
  //
  // CONCLUSION: Cannot distinguish structurally. Accept false positives on locative では.
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// "寝る前にそんなコーヒーをのんでは寝られない。"
//
// ANALYSIS: Verb-de + は pattern (飲んでは)
//
// GiNZA parses this sentence incorrectly as:
//   のん (VERB, dep=obl) + で (ADP, dep=case -> コーヒー) + は (ADP, dep=case -> コーヒー)
//
// The correct parse should be:
//   のん (VERB, dep=advcl) + で (SCONJ, dep=mark -> のん) + は (ADP, dep=case -> のん)
//
// GiNZA is treating "コーヒーをのんで" as a single unit (noun + instrumental verb)
// and attaching では to the noun コーヒー, not to the verb のん.
//
// This is a GiNZA parsing error. The verb should have dep=advcl (conditional clause)
// and the では should point to the verb, not to the noun.
//
// CONCLUSION: GiNZA limitation. Cannot match due to incorrect dependency structure.
const skipPositives = [
  '寝る前にそんなコーヒーをのんでは寝られない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
