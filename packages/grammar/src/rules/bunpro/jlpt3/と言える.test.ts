import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と言える.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // と言われている (passive/reported hearsay) - different grammar
  '彼は天才だと言われている。',
  '東京は人口が多い都市だと言われている。',

  // といえば (topic marker - "speaking of which")
  '日本といえば、桜です。',
  '彼といえば、最近会っていません。',

  // といっても ("even if I say" - concession)
  '安いといっても、まだ高いです。',
  'できるといっても、難しいです。',

  // ということだ (reported information/hearsay)
  '彼は来年帰国するということだ。',
  '試験は明日だということです。',

  // Simple quotational と without potential form
  '彼は「行く」と言った。',
  'はい、と言った。',

  // といい (wish/desire - "it would be good if")
  '明日といい、天気だといい。',
  'もっと早く来るといいのに。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
