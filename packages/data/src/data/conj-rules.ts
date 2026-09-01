/**
 * Conjugation rules system - CSV loading and conjugation construction
 * Ported from ~/ichiran/dict-load.lisp lines 196-304
 */

import fs from 'fs';
import path from 'path';
import { parse } from 'csv-parse/sync';
import { testWord } from '@ichiran/core';

/**
 * Conjugation rule structure
 * Ported from dict-load.lisp:265-267 conjugation-rule
 *
 * Note: This is different from ConjProp in types.ts.
 * ConjProp represents database conjugation data,
 * while ConjugationRule represents CSV transformation rules.
 */
export interface ConjugationRule {
  pos: number;        // Part of speech ID
  conj: number;       // Conjugation type ID
  neg: boolean;       // Negative form
  fml: boolean;       // Formal form
  onum: number;       // Order number
  stem: number;       // Number of characters to remove from end
  okuri: string;      // Okurigana to append
  euphr: string;      // Euphonic change for hiragana
  euphk: string;      // Euphonic change for kanji
}

/**
 * Part of speech data (id -> pos code)
 */
const posIndex = new Map<string, number>();
const posByIndex = new Map<number, string>();

/**
 * Conjugation rules (pos_id -> rules[])
 */
const conjRules = new Map<number, ConjugationRule[]>();

/**
 * Constants for conjugation processing
 */
export const DO_NOT_CONJUGATE = ['n', 'vs', 'adj-na'];
export const DO_NOT_CONJUGATE_SEQ = [2765070, 2835284];

export const POS_WITH_CONJ_RULES = [
  'adj-i', 'adj-ix', 'cop-da', 'v1', 'v1-s', 'v5aru',
  'v5b', 'v5g', 'v5k', 'v5k-s', 'v5m', 'v5n', 'v5r', 'v5r-i', 'v5s',
  'v5t', 'v5u', 'v5u-s', 'vk', 'vs-s', 'vs-i'
];

/**
 * Synthetic POS codes that don't exist in kwpos.csv
 * but are added programmatically to the database
 */
export const SYNTHETIC_POS_CODES = [
  'cop-da', // Added via conjugateDa() in errata.ts
];

export const SECONDARY_CONJUGATION_TYPES_FROM = [5, 6, 7, 8, 53]; // potential, passive, causative, causative-passive, causative-su
export const SECONDARY_CONJUGATION_TYPES = [2, 3, 4, 9, 10, 11, 12, 13];

/**
 * Loads kwpos.csv (part of speech definitions)
 * Ported from dict-load.lisp:241-243 load-pos-index
 */
function loadPosIndex(dataPath: string): void {
  const csvPath = path.join(dataPath, 'kwpos.csv');
  const content = fs.readFileSync(csvPath, 'utf-8');

  const records = parse(content, {
    delimiter: '\t',
    skip_empty_lines: true,
    from_line: 2 // Skip header
  });

  posIndex.clear();
  posByIndex.clear();

  for (const [id, pos] of records) {
    const posId = parseInt(id);
    posIndex.set(pos, posId);
    posByIndex.set(posId, pos);
  }
}


/**
 * Loads conjo.csv (conjugation transformation rules)
 * Ported from dict-load.lisp:270-283 load-conj-rules
 */
function loadConjRules(dataPath: string): void {
  const csvPath = path.join(dataPath, 'conjo.csv');
  const content = fs.readFileSync(csvPath, 'utf-8');

  const records = parse(content, {
    delimiter: '\t',
    skip_empty_lines: true,
    from_line: 2 // Skip header
  });

  conjRules.clear();

  for (const [posId, conjId, neg, fml, onum, stem, okuri, euphr, euphk] of records) {
    const pos = parseInt(posId);

    const rule: ConjugationRule = {
      pos,
      conj: parseInt(conjId),
      neg: neg === 't',
      fml: fml === 't',
      onum: parseInt(onum),
      stem: parseInt(stem),
      okuri: okuri || '',
      euphr: euphr || '',
      euphk: euphk || ''
    };

    if (!conjRules.has(pos)) {
      conjRules.set(pos, []);
    }
    conjRules.get(pos)!.push(rule);
  }

  // Apply errata: add archaic i-adjective conjugation forms
  // Ported from dict-errata.lisp:1182-1201 errata-conj-rules-hook
  addArchaicAdjectiveForms();

  // Apply other conjugation rule fixes
  // Ported from dict-errata.lisp:1197-1240 errata-conj-rules-hook
  applyConjugationRuleFixes();
}

/**
 * Adds archaic conjugation forms for i-adjectives (adj-i and adj-ix)
 * These are not in conjo.csv but are added programmatically
 * Ported from dict-errata.lisp:1182-1201 errata-conj-rules-hook
 */
function addArchaicAdjectiveForms(): void {
  // Conjugation type constants (from dict-errata.lisp:1168-1172)
  const CONJ_ADVERBIAL = 50;        // Adverbial form (～く)
  const CONJ_ADJECTIVE_STEM = 51;   // Adjective stem (remove い)
  const CONJ_ADJECTIVE_LITERARY = 54; // Old/literary form (～き)

  // For adj-i (regular i-adjectives like いじましい)
  const adjI = posIndex.get('adj-i');
  if (adjI) {
    const rules: ConjugationRule[] = [
      {
        pos: adjI,
        conj: CONJ_ADVERBIAL,
        neg: false,
        fml: false,
        onum: 1,
        stem: 1,
        okuri: 'く',
        euphr: '',
        euphk: ''
      },
      {
        pos: adjI,
        conj: CONJ_ADJECTIVE_STEM,
        neg: false,
        fml: false,
        onum: 1,
        stem: 1,
        okuri: '',
        euphr: '',
        euphk: ''
      },
      {
        pos: adjI,
        conj: CONJ_ADJECTIVE_LITERARY,
        neg: false,
        fml: false,
        onum: 1,
        stem: 1,
        okuri: 'き',
        euphr: '',
        euphk: ''
      }
    ];

    const existing = conjRules.get(adjI) || [];
    conjRules.set(adjI, [...existing, ...rules]);
  }

  // For adj-ix (special i-adjectives like いい → よい)
  const adjIx = posIndex.get('adj-ix');
  if (adjIx) {
    const rules: ConjugationRule[] = [
      {
        pos: adjIx,
        conj: CONJ_ADVERBIAL,
        neg: false,
        fml: false,
        onum: 1,
        stem: 1,
        okuri: 'く',
        euphr: 'よ',
        euphk: ''
      },
      {
        pos: adjIx,
        conj: CONJ_ADJECTIVE_STEM,
        neg: false,
        fml: false,
        onum: 1,
        stem: 1,
        okuri: '',
        euphr: 'よ',
        euphk: ''
      },
      {
        pos: adjIx,
        conj: CONJ_ADJECTIVE_LITERARY,
        neg: false,
        fml: false,
        onum: 1,
        stem: 1,
        okuri: 'き',
        euphr: 'よ',
        euphk: ''
      }
    ];

    const existing = conjRules.get(adjIx) || [];
    conjRules.set(adjIx, [...existing, ...rules]);
  }
}

let rulesLoaded = false;

/**
 * Loads all conjugation CSV files
 */
export function loadAllConjugationRules(dataPath: string = './data'): void {
  loadPosIndex(dataPath);
  loadConjRules(dataPath);
  rulesLoaded = true;
}

/**
 * Ensures rules are loaded (lazy loading)
 */
function ensureRulesLoaded(): void {
  if (!rulesLoaded) {
    loadAllConjugationRules();
  }
}

/**
 * Gets part-of-speech ID from pos code
 * Ported from dict-load.lisp:241-243 get-pos-index
 * 
 * Note: cop-da is a synthetic POS tag added by errata to fix だ conjugation
 * It maps to cop (ID 15) for conjugation rules
 */
export function getPosIndex(pos: string): number | undefined {
  ensureRulesLoaded();
  
  // Map cop-da to cop (see dict-errata.lisp:281 - cop-da added to trigger conjugation)
  if (pos === 'cop-da') {
    return posIndex.get('cop');
  }
  
  return posIndex.get(pos);
}

/**
 * Gets part-of-speech code from ID
 * Ported from dict-load.lisp:245-247 get-pos-by-index
 */
export function getPosByIndex(posId: number): string | undefined {
  ensureRulesLoaded();
  return posByIndex.get(posId);
}

/**
 * Gets conjugation rules for a part-of-speech ID
 * Ported from dict-load.lisp:270-283 get-conj-rules
 */
export function getConjRules(posId: number): ConjugationRule[] {
  ensureRulesLoaded();
  return conjRules.get(posId) || [];
}

/**
 * Constructs a conjugated form from a word and a rule
 * Ported from dict-load.lisp:286-295 construct-conjugation
 *
 * @param word - The word to conjugate
 * @param rule - The conjugation rule to apply
 * @returns The conjugated form
 */
export function constructConjugation(word: string, rule: ConjugationRule): string {
  // Check if word ends in kana (test last 2 characters to be safe)
  const testStr = word.slice(Math.max(0, word.length - 2));
  const isKana = testWord(testStr, 'kana');

  const euphr = rule.euphr;
  const euphk = rule.euphk;

  // Calculate extra stem to remove if euphonic change applies
  const extraStem = (isKana && euphr.length > 0) || (!isKana && euphk.length > 0) ? 1 : 0;
  const stem = rule.stem + extraStem;

  // Build conjugation: remove stem + add euphonic change + add okurigana
  return word.slice(0, word.length - stem) + (isKana ? euphr : euphk) + rule.okuri;
}

/**
 * Conjugates a word for all applicable rules for a given part-of-speech
 * Ported from dict-load.lisp:297-302 conjugate-word
 *
 * @param word - The word to conjugate
 * @param pos - Part-of-speech code (e.g., 'v5r', 'adj-i')
 * @returns Array of [rule, conjugatedForm] pairs
 */
export function conjugateWord(word: string, pos: string): Array<[ConjugationRule, string]> {
  const posId = getPosIndex(pos);
  if (!posId) return [];

  const rules = getConjRules(posId);
  return rules.map(rule => [rule, constructConjugation(word, rule)]);
}

/**
 * Applies conjugation rule fixes and special cases
 * Ported from dict-errata.lisp:1197-1240 errata-conj-rules-hook
 */
function applyConjugationRuleFixes(): void {
  const CONJ_NEGATIVE_STEM = 52;
  const CONJ_CAUSATIVE_SU = 53;

  // Fix 1: Add v5aru te-form exception (あり)
  const v5aru = posIndex.get('v5aru');
  if (v5aru) {
    const rules = conjRules.get(v5aru) || [];
    rules.push({
      pos: v5aru,
      conj: 3, // te-form
      neg: false,
      fml: false,
      onum: 2,
      stem: 1,
      okuri: 'り',
      euphr: '',
      euphk: ''
    });
    conjRules.set(v5aru, rules);
  }

  // Fix 2 & 3: Fix v1/v1-s negative formal and v5u negative conditional
  for (const [posId, rules] of conjRules.entries()) {
    const pos = posByIndex.get(posId);

    for (const rule of rules) {
      // Fix v1/v1-s negative formal: change okuri to "ません"
      if ((pos === 'v1' || pos === 'v1-s') &&
          rule.conj === 1 && rule.neg === true && rule.fml === true) {
        rule.okuri = 'ません';
      }

      // Fix v5u negative conditional: change okuri to "わなかったら"
      if (pos === 'v5u' &&
          rule.conj === 11 && rule.neg === true && rule.fml === false) {
        rule.okuri = 'わなかったら';
      }
    }
  }

  // Fix 4: Remove potential forms for vs-s verbs
  const vss = posIndex.get('vs-s');
  if (vss) {
    const rules = conjRules.get(vss) || [];
    const filtered = rules.filter(rule => rule.conj !== 5); // Remove potential (5)
    conjRules.set(vss, filtered);
  }

  // Fix 5 & 6: Add negative-stem for godan verbs only, and remap causative-su
  for (const [posId, rules] of conjRules.entries()) {
    const pos = posByIndex.get(posId);
    if (!pos) continue;

    // Add negative-stem ONLY for godan (v5*) verbs (dict-errata.lisp:1220-1234)
    if (pos.startsWith('v5')) {
      const negRule = rules.find(r =>
        r.conj === 1 && r.neg === true && r.fml === false
      );

      if (negRule && negRule.okuri.length >= 2) {
        // Create negative stem by removing last 2 chars (ない)
        // Copy neg/fml from neg-rule (Lisp: copy-conjugation-rule)
        rules.push({
          pos: posId,
          conj: CONJ_NEGATIVE_STEM,
          neg: negRule.neg,  // Copy from neg-rule (true)
          fml: negRule.fml,  // Copy from neg-rule (false)
          onum: 1,
          stem: negRule.stem,
          okuri: negRule.okuri.slice(0, -2),
          euphr: negRule.euphr,
          euphk: negRule.euphk
        });
      }
    }

    // Remap causative onum=2 to type 53
    for (const rule of rules) {
      if (rule.conj === 7 && rule.onum === 2) {
        rule.conj = CONJ_CAUSATIVE_SU;
        rule.onum = 1;
      }
    }
  }
}
