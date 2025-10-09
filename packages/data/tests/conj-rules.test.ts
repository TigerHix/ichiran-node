/**
 * Tests for conjugation rules system
 */

import { describe, test, expect } from 'bun:test';
import {
  loadAllConjugationRules,
  getPosIndex,
  getPosByIndex,
  getConjRules,
  constructConjugation,
  conjugateWord,
  POS_WITH_CONJ_RULES,
  SYNTHETIC_POS_CODES
} from '../src/data/conj-rules.js';
import { getConjDescription } from '@ichiran/core';

describe('Conjugation Rules', () => {
  test('loads POS index correctly', () => {
    loadAllConjugationRules('../../data');

    // Check some known POS codes
    expect(getPosIndex('adj-i')).toBe(1);
    expect(getPosIndex('v5r')).toBe(37);
    expect(getPosIndex('v1')).toBe(28);

    // Reverse lookup
    expect(getPosByIndex(1)).toBe('adj-i');
    expect(getPosByIndex(37)).toBe('v5r');
    expect(getPosByIndex(28)).toBe('v1');
  });

  test('loads conjugation descriptions', () => {
    loadAllConjugationRules('../../data');

    expect(getConjDescription(1)).toBe('Non-past');
    expect(getConjDescription(2)).toBe('Past (~ta)');
    expect(getConjDescription(3)).toBe('Conjunctive (~te)');
    expect(getConjDescription(4)).toBe('Provisional (~eba)');
  });

  test('loads conjugation rules', () => {
    loadAllConjugationRules('../../data');

    const adjIRules = getConjRules(1); // adj-i
    expect(adjIRules.length).toBeGreaterThan(0);

    // Check first rule structure
    const firstRule = adjIRules[0];
    expect(firstRule).toHaveProperty('pos');
    expect(firstRule).toHaveProperty('conj');
    expect(firstRule).toHaveProperty('neg');
    expect(firstRule).toHaveProperty('fml');
    expect(firstRule).toHaveProperty('stem');
    expect(firstRule).toHaveProperty('okuri');
  });

  test('constructs adj-i conjugations correctly', () => {
    loadAllConjugationRules('../../data');

    // Test 高い (takai, "tall/high")
    const word = '高い';
    const conjugations = conjugateWord(word, 'adj-i');

    expect(conjugations.length).toBeGreaterThan(0);

    // Find specific conjugations
    const pastForms = conjugations.filter(([rule]) => rule.conj === 2 && !rule.neg && !rule.fml);
    expect(pastForms.length).toBeGreaterThan(0);
    expect(pastForms[0][1]).toBe('高かった'); // Past affirmative

    const negForms = conjugations.filter(([rule]) => rule.conj === 1 && rule.neg && !rule.fml);
    expect(negForms.length).toBeGreaterThan(0);
    expect(negForms[0][1]).toBe('高くない'); // Negative non-past
  });

  test('constructs v5r conjugations correctly', () => {
    loadAllConjugationRules('../../data');

    // Test 走る (hashiru, "to run")
    const word = '走る';
    const conjugations = conjugateWord(word, 'v5r');

    expect(conjugations.length).toBeGreaterThan(0);

    // Find te-form
    const teForms = conjugations.filter(([rule]) => rule.conj === 3 && !rule.neg && !rule.fml);
    if (teForms.length > 0) {
      expect(teForms[0][1]).toBe('走って'); // Te-form
    }
  });

  test('constructs v1 conjugations correctly', () => {
    loadAllConjugationRules('../../data');

    // Test 食べる (taberu, "to eat")
    const word = '食べる';
    const conjugations = conjugateWord(word, 'v1');

    expect(conjugations.length).toBeGreaterThan(0);

    // Find specific forms
    const plainForms = conjugations.filter(([rule]) => rule.conj === 1 && !rule.neg && !rule.fml);
    if (plainForms.length > 0) {
      expect(plainForms[0][1]).toBe('食べる'); // Non-past (no change for v1 non-past affirmative)
    }

    const teForms = conjugations.filter(([rule]) => rule.conj === 3 && !rule.neg && !rule.fml);
    if (teForms.length > 0) {
      expect(teForms[0][1]).toBe('食べて'); // Te-form
    }
  });

  test('handles hiragana words correctly', () => {
    loadAllConjugationRules('../../data');

    // Test たかい (takai in hiragana)
    const word = 'たかい';
    const conjugations = conjugateWord(word, 'adj-i');

    expect(conjugations.length).toBeGreaterThan(0);

    // Find past form
    const pastForms = conjugations.filter(([rule]) => rule.conj === 2 && !rule.neg && !rule.fml);
    expect(pastForms.length).toBeGreaterThan(0);
    expect(pastForms[0][1]).toBe('たかかった'); // Past affirmative
  });

  test('POS_WITH_CONJ_RULES contains valid POS codes', () => {
    loadAllConjugationRules('../../data');

    for (const pos of POS_WITH_CONJ_RULES) {
      // Skip validation for synthetic POS codes that don't exist in kwpos.csv
      // but are added programmatically to the database (e.g., cop-da via conjugateDa())
      if (SYNTHETIC_POS_CODES.includes(pos)) {
        continue;
      }

      const posId = getPosIndex(pos);
      expect(posId).toBeDefined();
      expect(typeof posId).toBe('number');
    }
  });
});
