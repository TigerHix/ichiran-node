export const SENSE_PROPERTY_TAGS = [
  'pos',
  'misc',
  'dial',
  'field',
  's_inf',
  'stagk',
  'stagr'
] as const;

export type SensePropertyTag = typeof SENSE_PROPERTY_TAGS[number];
export type CanonicalRoute = 'kanji' | 'kana';

export interface SourceLocation {
  readonly sourceId: string;
  readonly ordinal: number;
}

/** Chronological source mutation followed by order within that mutation. */
export interface SourceOrder {
  readonly event: number;
  readonly ordinal: number;
}

export interface CanonicalForm {
  readonly text: string;
  readonly ordinal: number;
  readonly sourceOrder: SourceOrder;
  readonly common: number | null;
  readonly priorityTags: readonly string[];
  readonly conjugatable: boolean;
  readonly noKanji: boolean;
  readonly best: string | null;
}

export interface CanonicalSenseProperty {
  readonly tag: SensePropertyTag;
  readonly text: string;
  readonly ordinal: number;
  readonly sourceOrder: SourceOrder;
}

export interface CanonicalSense {
  readonly ordinal: number;
  readonly glosses: readonly string[];
  readonly properties: readonly CanonicalSenseProperty[];
}

export interface CanonicalRestriction {
  readonly reading: string;
  readonly written: string;
  readonly ordinal: number;
}

export interface CanonicalEntry {
  readonly seq: number;
  readonly source: SourceLocation;
  readonly kanji: readonly CanonicalForm[];
  readonly kana: readonly CanonicalForm[];
  readonly senses: readonly CanonicalSense[];
  readonly restrictions: readonly CanonicalRestriction[];
  readonly primaryNoKanji: boolean;
}

export interface ConjugationProperty {
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
}

export interface GeneratedForm {
  readonly route: CanonicalRoute;
  readonly text: string;
  readonly sourceText: string;
  readonly sourceOrdinal: number;
  readonly ruleOrdinal: number;
}

export interface GeneratedTarget {
  readonly seq: number;
  readonly kanji: readonly string[];
  readonly kana: readonly string[];
  readonly conjugatable: boolean;
  readonly allocationOrdinal: number;
}

export interface ConjugationCandidate {
  readonly property: ConjugationProperty;
  readonly forms: readonly GeneratedForm[];
  readonly targetConjugatable: boolean;
}

export interface ConjugationLink {
  readonly ordinal: number;
  readonly from: number;
  readonly target: number;
  readonly via: number | null;
  readonly property: ConjugationProperty;
  readonly forms: readonly GeneratedForm[];
}

export function entryPartOfSpeech(entry: CanonicalEntry): string[] {
  const result: string[] = [];
  const seen = new Set<string>();
  for (const sense of entry.senses) {
    for (const property of sense.properties) {
      if (property.tag !== 'pos' || seen.has(property.text)) continue;
      seen.add(property.text);
      result.push(property.text);
    }
  }
  return result;
}
