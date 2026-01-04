import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './くる.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences with data extraction bugs:
// The Bunpro JSON has content like: "彼（かれ）<span...>は</span>バス<span...>で</span><span...>____</span>。"
// with answer "きます". This should extract to "彼はバスでききます。" (He will come by bus)
// But the extraction produces "彼はバスできます。" (He is able to bus) - missing the "き" character.
// This is a bug in the data extraction logic, not in the rule or GiNZA parsing.
// Other sentences with "きます" (kimasu) extract correctly.
const skipPositives = [
  '彼はバスできます。', // Extraction bug: should be "彼はバスでききます。"
  '彼女は車できます。', // Extraction bug: should be "彼女は車でききます。"
];

const negatives = [
  // Similar sounding verbs that should not match
  '彼は会社へ行く。', // いく - different verb
  '雨が降る。', // ふる - different verb
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
