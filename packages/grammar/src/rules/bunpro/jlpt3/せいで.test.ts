import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './せいで.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Instrumental で (means "using/by means of", not "because of")
  '鉛筆で書く。',
  '日本語で話してください。',
  '電車で行きました。',
  // Locative で (means "at/in", not "because of")
  '公園で遊びます。',
  '家で勉強する。',
  '図書館で本を読む。',
  // Cause marker で with different nouns (not せい)
  '雨で試合が中止になった。',
  '病気で学校を休んだ。',
  '台風で電車が遅れた。',
  // Different words that contain せい (different lemmas)
  '彼は誠意を持って対応した。', // 誠意 (seii - sincerity, different lemma)
  '政権の政策を批判する。',     // 政権 (seiken - administration, different lemma)
  // Similar sounds but different words
  '成績が上がった。',           // 成績 (seiseki - grades, different word)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
