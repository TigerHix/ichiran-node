// ichiran/dict/splitQueries - Query functions for split/hint maps
// Extracted from split.ts to break circular dependencies
//
// This module contains only the query functions that look up in the maps.
// It does NOT import from readings.ts or scoring.ts (avoiding circular deps).
// The actual split/hint definitions remain in splitDefinitions.ts.

import type { Reading, SplitPart } from '../types.js';
import { wordConjData } from './conjugation.js';

// Import maps and types from splitMaps.ts (populated by splitDefinitions.ts)
import type { SplitAttrs } from './splitMaps.js';
import { splitMap, hintMap } from './splitMaps.js';

// =============================================================================
// SPLIT QUERIES
// =============================================================================

// Line 69-75: defun get-split*
async function getSplit_(reading: Reading, conjOf: number[] = []): Promise<[SplitPart[], SplitAttrs] | null> {
  // Reading (KanjiText | KanaText) always has seq property
  const readingSeq = reading.seq;

  if (readingSeq !== null) {
    const splitFn = splitMap.get(readingSeq);
    if (splitFn) {
      return await splitFn(reading);
    }
  }

  // Try conjugation ancestors
  for (const seq of conjOf) {
    const splitFn = splitMap.get(seq);
    if (splitFn) {
      return await splitFn(reading);
    }
  }

  return null;
}

// Line 77-81: defun get-split
// Main entry point for split queries
export async function getSplitImpl(reading: Reading, conjOf: number[] = []): Promise<[SplitPart[], SplitAttrs] | null> {
  const result = await getSplit_(reading, conjOf);
  if (result) {
    const [split, score] = result;
    // Safety check: ensure all split parts exist (no nulls)
    if (split && split.every(part => part !== null)) {
      return [split, score];
    }
  }
  return null;
}

// =============================================================================
// HINT QUERIES
// =============================================================================

// Line 933-938: defun get-hint
// Main entry point for hint queries
export async function getHintImpl(reading: Reading): Promise<string | null> {
  const seq = reading.seq;
  const hintFn = hintMap.get(seq);

  if (hintFn) {
    return await hintFn(reading);
  }

  // Try conjugation ancestors
  const conjData = await wordConjData(reading);
  if (conjData) {
    for (const data of conjData) {
      const conjSeq = data.from;
      const conjHintFn = hintMap.get(conjSeq);
      if (conjHintFn) {
        return await conjHintFn(reading);
      }
    }
  }

  return null;
}
