// Hard CLI Parity Tests - Complex auxiliary verb chains
import { describe, test, expect } from 'bun:test';
import { setupTests } from './test-setup.js';
import {
  loadParityTestData,
  runTsCli,
  parseCliOutput,
  normalizeJson,
} from './cli-parity-helpers.js';

const { testCases, expectedOutputs } = loadParityTestData(
  'hard-cli.json',
  'hard-cli-lisp-outputs.json',
  'Failed to load test data. Run "bun run preprocess-hard-cli-tests" to generate expected outputs.'
);

setupTests();

describe('Hard CLI Full JSON Comparison (-f flag)', () => {
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
