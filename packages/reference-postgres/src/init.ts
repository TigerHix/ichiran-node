// Ichiran Initialization Module
// Registers suffix definitions that need to run at initialization time.
//
// This module must be imported and initializeIchiran() called before using
// any dictionary functions that depend on suffixes.

import { registerSuffixDefinitions } from './dict/suffixDefinitions.js';

let initialized = false;

/**
 * Initialize Ichiran by registering suffix definitions.
 * This must be called before using any dictionary functions.
 *
 * Safe to call multiple times - only initializes once.
 */
export function initializeIchiran(): void {
  if (initialized) return;

  registerSuffixDefinitions();

  initialized = true;
}

/**
 * Check if Ichiran has been initialized
 */
export function isInitialized(): boolean {
  return initialized;
}

/**
 * Reset initialization state (primarily for testing)
 */
export function resetInitialization(): void {
  initialized = false;
}
