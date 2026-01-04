import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と-conditional.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that should NOT match the conditional と rule
const negatives = [
  // Quotation と (ADP + case dep, marks quotes/thoughts)
  '彼は来ると思っている。',
  '「こんにちは」と言いました。',
  '明日は雨だと思われます。',
  '彼は天才だと言われている。',
  '何とか言いましたか。',

  // Case marker と "with/accompaniment" (ADP + case dep + noun with nmod/obl dep)
  '友達と映画を見に行く。',
  '家族と買い物に行きます。',
  '彼と結婚しました。',
  '私と一緒に来てください。',
  '先生と討論しました。',

  // と as "and" in listing (ADP + case dep + noun with nmod dep)
  'リンゴとバナナを買いました。',
  '東京と大阪に行きました。',
  '犬と猫を飼っています。',
  '朝と晩に薬を飲みます。',
  '本とノートを持ってきてください。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
