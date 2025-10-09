import type {
  PatternNode,
  SequenceNode,
  AltNode,
  TokenNode,
  RepeatNode,
  OptionalNode,
  CaptureNode,
  PeekNode,
  NotNode,
  AnchorNode,
  MacroNode,
} from './types.js';

/**
 * Type-safe pattern building helpers to avoid 'as any' casts in macro definitions.
 * These functions provide a clean API for constructing pattern nodes with full type safety.
 */

/**
 * Create a sequence pattern node.
 * @param patterns - Array of pattern nodes to match in sequence
 */
export function seq(patterns: PatternNode[]): SequenceNode {
  return { sequence: patterns };
}

/**
 * Create an alternation pattern node.
 * @param patterns - Array of pattern nodes to match (any one)
 */
export function alt(patterns: PatternNode[]): AltNode {
  return { alt: patterns };
}

/**
 * Create a token pattern node.
 * @param predicates - Array of predicate strings to match against a token
 */
export function tok(predicates: string[]): TokenNode {
  return { token: predicates };
}

/**
 * Create a repeat pattern node.
 * @param options - Repeat configuration
 */
export function rep(options: {
  pattern: PatternNode;
  min?: number;
  max?: number;
  greedy?: boolean;
  until?: PatternNode;
}): RepeatNode {
  return { repeat: options };
}

/**
 * Create an optional pattern node.
 * @param pattern - Pattern to optionally match
 */
export function opt(pattern: PatternNode): OptionalNode {
  return { optional: pattern };
}

/**
 * Create a capture pattern node.
 * @param label - Label for the captured span
 * @param pattern - Pattern to capture
 */
export function cap(label: string, pattern: PatternNode): CaptureNode {
  return { capture: label, pattern };
}

/**
 * Create a peek (positive lookahead) pattern node.
 * @param pattern - Pattern to peek at without consuming
 */
export function peek(pattern: PatternNode): PeekNode {
  return { peek: pattern };
}

/**
 * Create a not (negative lookahead) pattern node.
 * @param pattern - Pattern that must not match
 */
export function not(pattern: PatternNode): NotNode {
  return { not: pattern };
}

/**
 * Create an anchor pattern node.
 * @param anchor - 'start' or 'end' anchor
 */
export function anchor(anchor: 'start' | 'end'): AnchorNode {
  return { anchor };
}

/**
 * Create a macro pattern node.
 * @param name - Name of the macro to invoke
 */
export function mac(name: string): MacroNode {
  return { macro: name };
}

