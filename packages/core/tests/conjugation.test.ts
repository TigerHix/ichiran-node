// Conjugation tests - ported from tests.lisp
import { describe, test, expect } from 'bun:test';
import { getConnection } from '@ichiran/core';
import { setupTests } from './test-setup.js';
import type { Conjugation } from '@ichiran/core';

setupTests();

describe('Conjugation Tests', () => {
  test('で is a conjugation of だ', async () => {
    // Line 642-644 in tests.lisp:
    // "Tests that で is a conjugation of だ"
    // (assert-equal (with-db nil (ichiran/dict::seq-from (car (ichiran/dict::select-conjs 2028980)))) 2089020)

    const db = getConnection();
    const conjs = await db<Conjugation[]>`
      SELECT * FROM conjugation WHERE seq = 2028980 AND via IS NULL
    `;

    // If no null-via conjugations, get all
    const result = conjs.length > 0 ? conjs : await db<Conjugation[]>`
      SELECT * FROM conjugation WHERE seq = 2028980
    `;

    expect(result.length).toBeGreaterThan(0);
    expect(result[0].from).toBe(2089020);
  });
});