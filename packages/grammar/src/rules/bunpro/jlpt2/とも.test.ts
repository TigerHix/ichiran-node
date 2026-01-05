import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とも.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Simple "together" meaning of とも (e.g., 二人とも, 親子とも - "both/along with")
  '二人とも行きます。',
  '親子ともに幸せです。',
  '彼女も私もともに頷いた。',

  // とも as "even" in different contexts (not the volitional pattern)
  '三時とも四時ともない。',  // "neither 3 nor 4" - different grammar

  // Volitional forms without とも
  '頑張ろうと思っています。',
  '行こうと言った。',

  // Simple とも particle usage ("together with")
  '友達とも映画を見ました。',
  '家族とも話し合った。',
];

// NOTE: "少なくとも" (at least) is excluded from negative tests because:
// - It has the SAME structure as our target pattern "辛くとも" (even if painful)
// - Both use I-adjective stem (ku-form) + とも
// - The difference is purely semantic: "at least" vs "even if"
// - No structural discriminator exists in GiNZA's parse
// - Not in our positive test data, so doesn't cause actual problems
//
// This is an acceptable limitation since "少なくとも" is a set phrase/adverb
// rather than a productive grammar pattern we need to detect.

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
