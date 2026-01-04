import type { GinzaPOS, GinzaDep, GinzaConjugationClass, GinzaInflectionForm } from './generated.js';

// Raw feature/misc maps as received from worker
export type GinzaFeatsRaw = Record<string, string>;
export type GinzaMiscRaw = Record<string, string | true>;

// GiNZA token type with pre-parsed typed fields
export type GinzaToken = {
  i: number; // sentence-local index
  text: string;
  lemma: string;
  pos: GinzaPOS;
  tag: string;
  dep: GinzaDep;
  head: number; // sentence-local head index, -1 for ROOT

  // Pre-parsed inflection (parsed once on ingest, not on every match)
  conjugationClass?: GinzaConjugationClass;
  inflectionForm?: GinzaInflectionForm;

  // Optional raw extras (spaCy/GiNZA JSON transport)
  norm?: string;
  whitespace?: string;
  feats?: GinzaFeatsRaw;
  inflection?: string; // raw Inflection string from GiNZA (e.g. "五段-カ行;連用形-イ音便")
  reading?: string;
  ne?: string;
  ene?: string;
  bunsetu?: { bi?: string | null; positionType?: string | null };
  clauseHead?: number; // sentence-local index of clause head

  // Raw key/value extras (for inventory + debugging)
  misc?: GinzaMiscRaw;

  // Token char span in original text
  start: number;
  end: number;
};

export type GinzaSentence = {
  text: string;
  start: number;
  end: number;
  tokens: GinzaToken[];
};

export type GinzaDoc = {
  text: string;
  sentences: GinzaSentence[];
};

export type GinzaMeta = {
  model?: string | null;
  lang?: string | null;
  spacyVersion?: string | null;
  ginzaVersion?: string | null;
  jaGinzaModelVersion?: string | null;
  pipes: string[];
  labels: Record<string, string[]>;
};
