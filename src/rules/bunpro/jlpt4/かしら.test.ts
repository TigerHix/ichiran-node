import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かしら.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar particles that should NOT match:
  // かな - masculine/casual wonder particle (not feminine かしら)
  '明日は晴れるかな。',
  'これでいいかな。',
  // か alone - question marker (not wonder)
  '明日は晴れるか。',
  'これでいいか。',
  // かい - masculine question marker
  '明日は晴れるかい？',
  'これを持ってくるかい。',
  // だろう - masculine/casual conjecture
  '明日は晴れるだろう。',
  '彼も来るだろう。',
  // だろうか - formal "I wonder"
  '明日は晴れるだろうか。',
  '本当にそうだろうか。',
  // らしい - "apparently/looks like" (not wonder)
  '明日は晴れるらしい。',
  '彼は日本人らしい。',
  // でしょう - polite "probably"
  '明日は晴れるでしょう。',
  '大丈夫でしょう。',
  // かもしれない - "maybe/might be"
  '明日は雨かもしれない。',
  '彼は犯人かもしれない。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
