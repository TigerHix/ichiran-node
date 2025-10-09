// Segmentation tests - ported from tests.lisp
import { describe, test, expect } from 'bun:test';
import { simpleSegment } from '@ichiran/core';
import { setupTests, extractTexts } from '../../../test-utils/test-setup.js';
import { readFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

setupTests();

// Load test data from JSON file
interface SegmentationTest {
  input: string;
  expected: (string | ':gap')[];
}

const segmentationTests: SegmentationTest[] = JSON.parse(
  readFileSync(join(__dirname, 'data/segmentation.json'), 'utf-8')
);

describe('Segmentation Tests', () => {
  for (const testCase of segmentationTests) {
    test(`"${testCase.input}"`, async () => {
      const texts = extractTexts(await simpleSegment(testCase.input));
      expect(texts).toEqual(testCase.expected);
    });
  }
});