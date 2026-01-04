import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './でも.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Sentence-initial でも (conjunction "but/however", not the particle)
  'でも、高いから買えない。',
  'でも、今日は行けません。',
  '暑いです。でも、行きます。',
  '雨が降っています。でも、外出します。',
  // でもででも pattern (different grammar point: "whether A or B")
  // Note: This would be covered by the でもででも rule
  '安くても、高くても、買います。',
  '雨が降っても、風が吹いても、出かけます。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
