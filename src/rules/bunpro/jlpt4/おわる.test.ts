import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おわる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 終わる (kanji) means "to end" as main verb, not "finish doing"
  '映画が終わる。',
  '映画が終わった。',
  '映画が終わります。',
  '会議が終わった。',
  '授業が終わるまで待って。',
  // おわる alone without preceding verb stem
  'これで終わる。',
  'もう終わった？',
  // Different verbs with similar surface forms
  '映画を見終わった時', // 見る + 終わる (kanji) separately
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
