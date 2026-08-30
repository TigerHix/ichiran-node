import { describe, test } from 'bun:test';

import {
  assertJsonParity,
  assertTextParity,
  loadCanonicalParityOutputs,
  loadParityTestData,
  runFullJsonParity,
  runTextParity
} from './cli-parity-helpers.js';

const RUN_PARITY_TESTS = process.env.RUN_PARITY_TESTS === 'true';
const PACK_CONFIGURED = Boolean(process.env.ICHIRAN_PACK_DIR);

const { testCases, expectedOutputs } = loadParityTestData(
  'cli.json',
  'cli-lisp-outputs.json',
  'Failed to load captured current-Lisp CLI fixtures.'
);
const canonicalOutputs = loadCanonicalParityOutputs(
  'cli-canonical-outputs.json',
  'cli-lisp-outputs.json',
  252
);

describe('ordinary canonical fixture provenance', () => {
  test('is linked to the raw Lisp capture and pinned oracle', () => {
    if (canonicalOutputs.stats.requests !== 252
      || canonicalOutputs.stats.rewrittenSeqFields <= 0) {
      throw new Error('Ordinary canonical identity fixture provenance is invalid');
    }
  });
});

describe.skipIf(!RUN_PARITY_TESTS)('packed runtime parity configuration', () => {
  test('locks the current-Lisp fixture counts', () => {
    const counts = [
      testCases.romanization.length,
      testCases.info.length,
      testCases.fullJson.length
    ];
    if (JSON.stringify(counts) !== JSON.stringify([5, 3, 252])) {
      throw new Error(`Expected 5 romanization, 3 info, and 252 full-JSON fixtures; found ${counts.join(', ')}`);
    }
  });

  test('uses an explicitly installed analyzer pack', () => {
    if (!PACK_CONFIGURED) {
      throw new Error('ICHIRAN_PACK_DIR must point to a complete analyzer release');
    }
  });
});

describe.skipIf(!RUN_PARITY_TESTS || !PACK_CONFIGURED)('packed runtime vs current Lisp CLI', () => {
  test('matches all 5 romanization fixtures', async () => {
    assertTextParity(
      'romanization',
      await runTextParity(testCases.romanization, expectedOutputs.romanization)
    );
  }, 120_000);

  test('matches all 3 info fixtures', async () => {
    assertTextParity(
      'info',
      await runTextParity(testCases.info, expectedOutputs.info, { withInfo: true })
    );
  }, 120_000);

  test('matches all 252 ordinary full-JSON fixtures', async () => {
    assertJsonParity(
      'ordinary full JSON',
      await runFullJsonParity(testCases.fullJson, canonicalOutputs.fullJson)
    );
  }, 600_000);
});
