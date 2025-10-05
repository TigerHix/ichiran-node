import { GrammarInfo } from './parsing.js';
import { WordInfo } from '../dict/wordInfo.js';

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
  start: number;
  end: number;
  tokens: Token[];
}

export interface MatchHit {
  grammarId: string;
  level: string;
  description?: string;
  captures: Capture[];
}

export interface MatchOutcome {
  index: number;
  captures: Capture[];
  preference?: number;
}

export interface MatchOptions {
  maxMatches?: number;
}

export interface GrammarDefinition {
  id: string;
  level: string;
  description?: string;
  priority?: number;
  pattern: PatternNode;
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
