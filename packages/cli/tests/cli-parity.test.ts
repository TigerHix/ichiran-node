// CLI Parity Tests - Compare TypeScript CLI output with Lisp CLI output
import { describe, test, expect } from 'bun:test';
import { setupTests } from '../../../test-utils/test-setup.js';
import {
  loadParityTestData,
  runTsCli,
  parseCliOutput,
  normalizeJson,
} from './cli-parity-helpers.js';

const { testCases, expectedOutputs } = loadParityTestData(
  'cli.json',
  'cli-lisp-outputs.json',
  'Failed to load test data. Run "bun run preprocess-cli-tests" to generate expected outputs.'
);

setupTests();

describe('CLI Romanization Comparison', () => {
  test.each(testCases.romanization)('romanization matches for: %s', async (text) => {
    const tsOutput = await runTsCli(text);
    const expectedOutput = expectedOutputs.romanization[text];
    expect(tsOutput).toBe(expectedOutput);
  });
});

describe('CLI Info Output Comparison (-i flag)', () => {
  test.each(testCases.info)('info output matches for: %s', async (text) => {
    const tsOutput = await runTsCli(text, { withInfo: true });
    const expectedOutput = expectedOutputs.info[text];
    expect(tsOutput).toBe(expectedOutput);
  });
});

describe('CLI Full JSON Comparison (-f flag)', () => {
  test.each(testCases.fullJson)('full JSON matches for: $text (limit: $limit)', async (data) => {
    const tsOutput = await runTsCli(data.text, { full: true, limit: data.limit });
    const key = `${data.text}|${data.limit}`;
    const expectedOutput = expectedOutputs.fullJson[key];

    const tsJson = parseCliOutput(tsOutput);
    const expectedJson = parseCliOutput(expectedOutput);

    // Normalize both outputs to sort alternatives deterministically before comparing
    // This handles the case where alternatives have equal precedence and can appear in any order
    const normalizedTs = normalizeJson(tsJson);
    const normalizedExpected = normalizeJson(expectedJson);

    expect(normalizedTs).toEqual(normalizedExpected);
  });
});
