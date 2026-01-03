/**
 * Presentation layer types
 * Type definitions for API responses and client-facing interfaces
 */

import type { WordInfoGlossJson } from '../dict/presentation.js';

/**
 * Response from the romanize endpoint
 */
export interface RomanizeResponse {
  /** Romanized text */
  romanized: string;
  /** Optional word information with glosses */
  info?: Array<[string, string]>;
}

/**
 * Request parameters for romanize endpoint
 */
export interface RomanizeRequest {
  /** Japanese text to romanize */
  text: string;
  /** Include dictionary information */
  withInfo?: boolean;
}

/**
 * Response from the segment endpoint
 */
export interface SegmentResponse {
  /** Original input text */
  text: string;
  /** Segmented and analyzed words */
  segments: WordInfoGlossJson[];
  /** Limit used for segmentation */
  limit: number;
}

/**
 * Named entity hint for segmentation
 * Boosts score of segments matching the entity span and assigns proper noun PoS
 */
export interface EntityHint {
  /** Start character offset (0-indexed, inclusive) */
  start: number;
  /** End character offset (0-indexed, exclusive) */
  end: number;
  /** Score boost for matching segments (default: 50) */
  boost?: number;
}

/**
 * Request parameters for segment endpoint
 */
export interface SegmentRequest {
  /** Japanese text to segment */
  text: string;
  /** Maximum number of alternative segmentations per position */
  limit?: number;
  /** Named entity hints to guide segmentation */
  entities?: EntityHint[];
}

/**
 * Response from the romanizeStar endpoint (full JSON output)
 * Each segment is either a string (non-word) or an array of alternatives
 */
export type RomanizeStarSegment =
  | string
  | Array<[Array<[string, WordInfoGlossJson, any]>, number]>;

export interface RomanizeStarResponse {
  /** Original input text */
  text: string;
  /** Segmented output with alternatives and scores */
  segments: RomanizeStarSegment[];
  /** Limit used for segmentation */
  limit: number;
}

/**
 * Request parameters for romanizeStar endpoint
 */
export interface RomanizeStarRequest {
  /** Japanese text to process */
  text: string;
  /** Maximum number of alternative segmentations per position */
  limit?: number;
}

/**
 * Generic error response
 */
export interface ErrorResponse {
  /** Error message */
  error: string;
  /** Optional error details */
  details?: any;
}

/**
 * Health check response
 */
export interface HealthResponse {
  /** Service status */
  status: 'ok' | 'error';
  /** Optional status message */
  message?: string;
}
