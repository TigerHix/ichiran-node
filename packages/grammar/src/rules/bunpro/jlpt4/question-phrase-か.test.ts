import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './question-phrase-か.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: similar-looking patterns that should NOT match
const negatives = [
  // Direct questions (sentence-ending か)
  'どこですか。',
  '何を食べますか。',
  'いつ行きますか。',
  // Indefinite pronouns (JLPT5 pattern: どこか = somewhere, 誰か = someone)
  'どこかへ行きたいです。',
  '誰かが来ました。',
  '何か食べますか。',
  // Alternative particle か (A or B)
  'リンゴかバナナを選びます。',
  '月曜か火曜に会いましょう。',
  // Question word followed by other particles
  'どこへ行きますか。',
  '誰と来ましたか。',
  '何を買いましたか。',
  // かどうか pattern (different grammar point)
  '行くかどうかわかりません。',
  '来るかどうか決めてください。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
