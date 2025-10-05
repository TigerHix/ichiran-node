/**
 * Tests for conjugation generation system
 */

import { describe, test, expect, beforeAll } from 'bun:test';
import { setupTests } from './test-setup.js';
import { getConnection } from '../src/conn.js';
import {
  getAllReadings,
  conjugateEntryInner,
} from '../src/data/conjugate.js';
import { loadAllConjugationRules } from '../src/data/conj-rules.js';

setupTests();

describe('Conjugation Generation', () => {
  beforeAll(() => {
    // Ensure conjugation rules are loaded
    loadAllConjugationRules('./data');
  });

  test('getAllReadings fetches readings for an entry', async () => {
    const sql = getConnection();

    // Find an entry with both kanji and kana
    const entries = await sql<{ seq: number }[]>`
      SELECT DISTINCT k.seq
      FROM kanji_text k
      INNER JOIN kana_text r ON k.seq = r.seq
      WHERE k.seq < 1200000
      LIMIT 1
    `;

    if (entries.length > 0) {
      const seq = entries[0].seq;
      const readings = await getAllReadings(seq);
      expect(readings.length).toBeGreaterThan(0);
      expect(Array.isArray(readings)).toBe(true);
    }
  });

  test('conjugateEntryInner generates conjugation matrix', async () => {
    const sql = getConnection();

    // Find a v5r verb (走る - hashiru, "to run" is seq 1593850)
    const entries = await sql<{ seq: number }[]>`
      SELECT DISTINCT sp.seq
      FROM sense_prop sp
      WHERE sp.tag = 'pos' AND sp.text = 'v5r'
      LIMIT 1
    `;

    if (entries.length > 0) {
      const seq = entries[0].seq;
      const matrix = await conjugateEntryInner(seq);

      expect(matrix.size).toBeGreaterThan(0);

      // Check that matrix contains 2x2 arrays
      for (const [key, arr] of matrix.entries()) {
        expect(arr.length).toBe(2); // 2 rows (neg)
        expect(arr[0].length).toBe(2); // 2 cols (fml)
        expect(arr[1].length).toBe(2);
      }
    }
  });

  test('conjugateEntryInner respects conjTypes filter', async () => {
    const sql = getConnection();

    const entries = await sql<{ seq: number }[]>`
      SELECT DISTINCT sp.seq
      FROM sense_prop sp
      WHERE sp.tag = 'pos' AND sp.text = 'v5r'
      LIMIT 1
    `;

    if (entries.length > 0) {
      const seq = entries[0].seq;

      // Filter to only past tense (conj_type = 2)
      const matrix = await conjugateEntryInner(seq, { conjTypes: [2] });

      // All keys should end with ',2' (conj_type = 2)
      for (const key of matrix.keys()) {
        expect(key.endsWith(',2')).toBe(true);
      }
    }
  });

  test('conjugateEntryInner respects asPosi override', async () => {
    const sql = getConnection();

    const entries = await sql<{ seq: number }[]>`
      SELECT seq FROM entry WHERE seq < 1200000 LIMIT 1
    `;

    if (entries.length > 0) {
      const seq = entries[0].seq;

      // Override POS to v1
      const matrix = await conjugateEntryInner(seq, { asPosi: ['v1'] });

      // Should process as v1 regardless of actual POS
      // If entry has no conjugatable readings, matrix may be empty
      expect(matrix).toBeDefined();
    }
  });

  test('conjugation matrix structure is correct', async () => {
    const sql = getConnection();

    // Get an i-adjective (adj-i)
    const entries = await sql<{ seq: number }[]>`
      SELECT DISTINCT sp.seq
      FROM sense_prop sp
      WHERE sp.tag = 'pos' AND sp.text = 'adj-i'
      LIMIT 1
    `;

    if (entries.length > 0) {
      const seq = entries[0].seq;
      const matrix = await conjugateEntryInner(seq);

      if (matrix.size > 0) {
        for (const [key, arr] of matrix.entries()) {
          // Check 2x2 structure
          expect(arr.length).toBe(2);
          expect(arr[0].length).toBe(2);

          // Check that readings have required properties
          for (let neg = 0; neg < 2; neg++) {
            for (let fml = 0; fml < 2; fml++) {
              const readings = arr[neg][fml];
              for (const reading of readings) {
                expect(reading).toHaveProperty('text');
                expect(reading).toHaveProperty('kanjiFlag');
                expect(reading).toHaveProperty('sourceText');
                expect(reading).toHaveProperty('ord');
                expect(reading).toHaveProperty('onum');
              }
            }
          }
        }
      }
    }
  });
});
