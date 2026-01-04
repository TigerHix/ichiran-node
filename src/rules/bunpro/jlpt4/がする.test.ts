import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がする.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Regular が + する (subject marker + verb)
  // Case 1: Person + が + verbphrase + する (suru-verb compound)
  '彼が勉強する。',
  '私が練習する。',
  '田中さんが発表する。',
  // Case 2: Subject + が + object + を + する
  '彼が夕食をする。',
  '私が掃除をする。',
  // Case 3: Just subject + が without suru
  '私が本を読む。',
  '彼が買物に行く。',
  '田中さんが来た。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
