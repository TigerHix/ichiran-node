// Shared types and interfaces

export interface WordInfo {
  type: 'word' | 'gap';
  text: string;
  kana?: string;
  score?: number;
  components?: any[];
  alternative?: any;
  primary?: boolean;  // For compound word components, indicates if this is the primary word
  start?: number;
  end?: number;
  counter?: any;
  skipped?: boolean;
  seq?: number;
  gloss?: any;
  conj?: any[];
}

export type BasicSegment = { type: 'word' | 'misc', text: string };