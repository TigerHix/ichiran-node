import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がる.js';
import { BUNPRO_JLPT4 } from './index.js';

// Note: The noun form 怖がり (scaredy-cat) also matches since GiNZA parses
// it with lemma=がる. This is acceptable since it's linguistically related
// to the がる construction (it's a noun derived from the がる verb).
const negatives = [
  // No clear negatives - がる constructions are quite distinctive
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
