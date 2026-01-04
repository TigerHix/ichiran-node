import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './できれば-できたら.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences that should NOT match できれば-できたら
const negatives = [
  // できる (potential form) without conditional - just "can do"
  '日本語ができます。',
  'お金がないから買えない。',
  'できるだけ早く来てください。',
  // Different conditional forms with similar patterns
  // These use other verbs, not できる
  '来ればいいです。',
  '行ったら楽しいです。',
  // なるべく (as much as possible) - similar meaning but different grammar
  'なるべく早く来てください。',
  'なるべく静かにしてください。',
  // でよければ (if [noun] is okay) - attached to nouns
  'これでよければ、使ってください。',
  '月曜でよければ大丈夫です。',
  // よければ (if it is good/okay) - different grammar
  'よければ、明日来てください。',
  '時間がよければ行きます。',
  // できる + と (when/if + result) - different conditional
  '日本語ができると、日本で働ける。',
  '勉強できると、試験に合格する。',
  // できる + なら (if it is the case that) - different conditional
  '日本語ができるなら、通訳になれる。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
