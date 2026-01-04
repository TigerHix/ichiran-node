import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that should NOT match the quotation と rule
const negatives = [
  // Conditional と (SCONJ + mark dep) - different grammar point
  '春になると桜が咲く。',
  '右に行くと駅があります。',
  'このボタンを押すと、動きます。',
  '雨が降ると行きません。',
  '駅に行くと、友達に会った。',

  // Conjunctive/accompaniment と "with" (ADP + case dep + noun with nmod dep)
  '友達と映画を見に行く。',
  '家族と買い物に行きます。',
  '彼と結婚しました。',
  '私と一緒に来てください。',
  '友達と話をしました。',
  '彼女と食事をします。',
  '先生と討論しました。',

  // と as "and" in listing (also uses nmod dep)
  'リンゴとバナナを買いました。',
  '東京と大阪に行きました。',
  '犬と猫を飼っています。',
  '朝と晩に薬を飲みます。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
