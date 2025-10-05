// Split and hint map definitions
// Extracted from splitDefinitions.ts to break circular dependencies
//
// These maps are populated by splitDefinitions.ts and queried by splitQueries.ts

import type { Reading, SplitPart } from '../types.js';

// Type for split attributes: simple score or extended object
export type SplitAttrs = number | { score: number; primary?: number; connector?: string; root?: number[] };

// Type for async split functions (used by segsplits that may call async findWordSeq)
export type AsyncSplitFunction = (reading: Reading) => Promise<[SplitPart[], SplitAttrs] | null>;

// Type for hint functions
export type HintFunction = (reading: Reading) => Promise<string | null>;

// Map from seq to split function
export const splitMap = new Map<number, AsyncSplitFunction>();

// Map from seq to hint function
export const hintMap = new Map<number, HintFunction>();

// Map from seq to segsplit function (used during segmentation)
export const segsplitMap = new Map<number, AsyncSplitFunction>();
