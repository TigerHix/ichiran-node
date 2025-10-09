// Shared utilities for CLI parity tests
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import { readFileSync } from 'fs';
import { runCli } from '../src/index.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export interface TestCases {
  romanization: string[];
  info: string[];
  fullJson: Array<{ text: string; limit: number }>;
}

export interface ExpectedOutputs {
  romanization: Record<string, string>;
  info: Record<string, string>;
  fullJson: Record<string, string>;
}

export interface ParityTestData {
  testCases: TestCases;
  expectedOutputs: ExpectedOutputs;
}

/**
 * Load test data from JSON files
 */
export function loadParityTestData(
  testCasesFile: string,
  expectedOutputsFile: string,
  errorMessage: string
): ParityTestData {
  const testCasesPath = join(__dirname, 'data', testCasesFile);
  const expectedOutputsPath = join(__dirname, 'data', expectedOutputsFile);

  try {
    const testCases = JSON.parse(readFileSync(testCasesPath, 'utf-8'));
    const expectedOutputs = JSON.parse(readFileSync(expectedOutputsPath, 'utf-8'));
    return { testCases, expectedOutputs };
  } catch (e) {
    throw new Error(errorMessage);
  }
}

/**
 * Run TypeScript CLI with options
 * CLI defaults to normalizePunctuation: true (backward compatible with Lisp)
 */
export async function runTsCli(
  text: string,
  options: { withInfo?: boolean; full?: boolean; limit?: number } = {}
): Promise<string> {
  return await runCli(text, options);
}

/**
 * Parse CLI output as JSON
 */
export function parseCliOutput(output: string): any {
  try {
    return JSON.parse(output);
  } catch (e) {
    throw new Error(`Failed to parse CLI output: ${output}`);
  }
}

/**
 * Normalize JSON by sorting alternatives deterministically.
 * Alternatives with equal scores can appear in any order, so we sort them by seq for comparison.
 */
export function normalizeJson(obj: any): any {
  if (obj === null || obj === undefined) {
    return obj;
  }

  if (Array.isArray(obj)) {
    return obj.map(normalizeJson);
  }

  if (typeof obj === 'object') {
    const normalized: any = {};

    for (const key of Object.keys(obj)) {
      if (key === 'alternative' && Array.isArray(obj[key])) {
        // Sort alternatives by seq for deterministic comparison
        normalized[key] = [...obj[key]]
          .map(normalizeJson)
          .sort((a, b) => (a.seq || 0) - (b.seq || 0));
      } else {
        normalized[key] = normalizeJson(obj[key]);
      }
    }

    return normalized;
  }

  return obj;
}
