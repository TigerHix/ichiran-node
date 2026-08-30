import { describe, test } from 'bun:test';

import {
  assertJsonParity,
  loadParityTestData,
  runFullJsonParity
} from './cli-parity-helpers.js';

const RUN_PARITY_TESTS = process.env.RUN_PARITY_TESTS === 'true';
const RUN_PACKED_PARITY = RUN_PARITY_TESTS
  && Boolean(process.env.ICHIRAN_PACK_DIR);

const { testCases, expectedOutputs } = loadParityTestData(
  'hard-cli.json',
  'hard-cli-lisp-outputs.json',
  'Failed to load captured current-Lisp hard CLI fixtures.'
);

describe.skipIf(!RUN_PARITY_TESTS)('hard current-Lisp fixture configuration', () => {
  test('locks all 149 fixtures', () => {
    if (testCases.fullJson.length !== 149) {
      throw new Error(`Expected 149 hard full-JSON fixtures; found ${testCases.fullJson.length}`);
    }
  });
});

describe.skipIf(!RUN_PACKED_PARITY)('packed runtime vs current Lisp hard CLI', () => {
  test('matches all 149 hard full-JSON fixtures', async () => {
    assertJsonParity(
      'hard full JSON',
      await runFullJsonParity(testCases.fullJson, expectedOutputs.fullJson)
    );
  }, 600_000);
});
