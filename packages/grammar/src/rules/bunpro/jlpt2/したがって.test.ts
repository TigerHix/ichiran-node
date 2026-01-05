import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './したがって.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the したがって grammar rule
const negatives = [
  // にしたがって (ni shitagatte) - "according to/along with" (different grammar)
  // Example: 手順にしたがって操作してください
  '手順にしたがって操作してください。',
  'ルールにしたがってゲームを行います。',

  // したがう (shitagau) - "to follow/comply with" (verb, not conjunction)
  '彼は上司の命令にしたがう。',
  '法律にしたがって行動しましょう。',

  // したがって followed by volitional/uncertain (not logical consequence)
  // This would be incorrect usage, but we want to ensure we don't overcapture

  // だから (dakara) - casual "therefore" (less formal)
  '雨が降っている。だから、行きません。',

  // それで (sorede) - "so/therefore" (less formal)
  '雨が降っている。それで、行きません。',

  // ですから (desukara) - polite "therefore" (conversational)
  '雨が降っています。ですから、行きません。',

  // よって (yotte) - "therefore" (very formal)
  '以上の理由により、よって、却下する。',

  // その結果 (sono kekka) - "as a result" (noun phrase)
  '雨が降った。その結果、試合は中止になった。',

  // そこで (sokode) - "therefore/so" + action taken
  '雨が降っていた。そこで、傘を持って出かけた。',

  // した (shita) - past tense of する (suru), not related to したがって
  '彼は宿題をした。',
  '昨日は映画を見たした。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
