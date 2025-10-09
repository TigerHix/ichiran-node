import { GrammarInfo } from './parsing.js';
import type { WordInfo } from '@ichiran/core';

export interface Token {
  text: string;
  wordInfo: WordInfo | undefined;
  grammarInfo: GrammarInfo | undefined;
}

export interface PredicateContext {
  tokens: Token[];
  index: number;
  prev?: Token;
  next?: Token;
}

// PredicateFn moved to predicates.ts as AsyncPredicateFn (all predicates are now async)

export type PredicateSpec = string;

export interface Capture {
  label: string;
  start: number;  // Token start index (for internal use)
  end: number;    // Token end index (for internal use)
  tokens: Token[];
  text: string;   // The captured text from the original string
  textStart: number;  // Character position in original text (inclusive)
  textEnd: number;    // Character position in original text (exclusive)
}

// Internal type used during compilation - text positions are filled in by runtime
export interface PartialCapture {
  label: string;
  start: number;
  end: number;
  tokens: Token[];
}

export interface MatchSegment {
  type: 'raw' | 'capture';
  text: string;
  label?: string;  // Only present when type is 'capture'
}

export interface MatchHit {
  grammarId: string;
  level: string;
  description?: string;
  captures: Capture[];
  segments: MatchSegment[];  // Alternating raw/capture segments for easy rendering
}

export interface MatchOutcome {
  index: number;
  captures: PartialCapture[];
  preference?: number;
}

export interface MatchOptions {
  maxMatches?: number;
}

export interface GrammarDefinition {
  id: string;
  label?: string;
  level: string;
  description?: string;
  formation?: string;
  priority?: number;
  pattern: PatternNode;
  startGate?: StartGate;
  explanation?: string;
  examples?: ExampleSentence[];
  negativeExamples?: ExampleSentence[];
}

export interface ExampleSentence {
  jp: string;
  en?: string;
}

export type PatternNode =
  | SequenceNode
  | AltNode
  | TokenNode
  | RepeatNode
  | OptionalNode
  | CaptureNode
  | PeekNode
  | NotNode
  | AnchorNode
  | MacroNode;

export interface SequenceNode {
  sequence: PatternNode[];
}

export interface AltNode {
  alt: PatternNode[];
}

export interface TokenNode {
  token: PredicateSpec[];
}

export interface RepeatNode {
  repeat: {
    pattern: PatternNode;
    min?: number;
    max?: number;
    greedy?: boolean;
    until?: PatternNode;
  };
}

export interface OptionalNode {
  optional: PatternNode;
}

export interface CaptureNode {
  capture: string;
  pattern: PatternNode;
}

export interface PeekNode {
  peek: PatternNode;
}

export interface NotNode {
  not: PatternNode;
}

export interface AnchorNode {
  anchor: 'start' | 'end';
}

export interface MacroNode {
  macro: string;
}

// Start gate DSL extensions
export interface StartGate {
  /**
   * Predicates that must hold at the start token (like a token node)
   * Example: ["text:一番"]
   */
  firstToken?: PredicateSpec[];
  /**
   * Predicates that must appear on some token anywhere in the sentence
   * Example: ["text:一番", "kana:/^いちばん$/"]
   */
  anyToken?: PredicateSpec[];
  /**
   * Require a token satisfying these predicates within [left,right] tokens of the start index
   */
  near?: { predicates: PredicateSpec[]; window: { left: number; right: number } };
}
