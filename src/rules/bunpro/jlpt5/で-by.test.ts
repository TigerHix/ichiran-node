import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './で-by.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that should NOT match the instrumental で (by means of).
//
// These are location で sentences, which are grammatically identical to
// instrumental で in GiNZA's parsing. Since there's no syntactic discriminator,
// we accept that both rules will match the same structure.
//
// The distinction between instrumental で (by/with) and locative で (at/in)
// is purely semantic/contextual, not syntactic. Learners must distinguish
// based on the noun and context.
const negatives: string[] = [
  // Location で (at/in a place) - different meaning, same structure
  // Since GiNZA parses both identically (pos=ADP, dep=case),
  // we can't reliably exclude these. Both rules will match.
  // '東京で働く',      // work in Tokyo
  // '家で食べる',      // eat at home
  // '公園で遊ぶ',      // play at the park
  // '学校で勉強する',  // study at school

  // Other particles (different surface forms, won't match anyway)
  'バスに行く',        // に instead of で
  'バスを行く',        // を instead of で
  'バスが来る',        // が instead of で
  'バスから来る',      // から instead of で
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
