import type {
  PortableAnalysisInflection,
  PortableAnalysisRoot
} from './analyzer-result.js';
import type {
  AnalyzerConjugation,
  AnalyzerScoreCandidate,
  AnalyzerScoreInfo,
  AnalyzerScoreModifier,
  AnalyzerSequenceFacts
} from './analyzer-types.js';
import type { AnalyzerSupportRoute } from './analyzer-support.js';
import type { MorphologyProperty } from './morphology.js';
import type { RootPayloadReader } from './root-payload.js';

export interface CandidateComponent {
  readonly text: string;
  readonly trueText: string | null;
  readonly route: AnalyzerSupportRoute;
  readonly reading: string;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly primary: boolean;
  readonly publicSeq: number | null;
  readonly physicalKey: string;
  readonly physicalGroup: number | null;
  readonly suffixClass: string | null;
  readonly definitionSeq: number | null;
  readonly semanticMembers: readonly CandidateSemanticMember[];
  /** All roots represented by the physical row before an explicit selector. */
  readonly identityRoots?: readonly number[];
  readonly conjugationSelection: 'default' | 'explicit' | 'root';
}

export interface CandidateSemanticMember {
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly publicSeq: number | null;
  readonly physicalGroup: number | null;
  readonly memberOrd: number | null;
  readonly targetNKanji: number | null;
  readonly targetNKana: number | null;
  /** Exact lexical intermediate used by a two-stage physical row, when any. */
  readonly viaSeq: number | null;
  /** Dense physical groups for each generated stage, root to final. */
  readonly stageGroups: readonly (number | null)[];
  /** Semantic locator for a physically unique stage without a dense group. */
  readonly stageKeys?: readonly (string | null)[];
  readonly stageMemberOrds: readonly (number | null)[];
  readonly stagePropOrds: readonly (number | null)[];
}

export interface MaterializedCandidate {
  readonly key: string;
  readonly kind: 'simple' | 'proxy' | 'compound' | 'counter';
  readonly text: string;
  readonly trueText: string;
  readonly route: AnalyzerSupportRoute;
  readonly reading: string;
  readonly publicSeq: number | null;
  readonly physicalSeq: number | null;
  /** Internal physical identity; never exposed by the clean result. */
  readonly physicalKey: string;
  /** Dense pack-local identity for exact legacy physical grouping. */
  readonly physicalGroup: number | null;
  /** Semantic locators whose global lookup ranks must agree after physical grouping. */
  readonly lookupLocators: readonly {
    readonly rootSeq: number;
    readonly aliases: readonly [number] | readonly [number, number] | null;
  }[];
  readonly memberOrd: number | null;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly scoreFacts: AnalyzerScoreCandidate;
  readonly components: readonly CandidateComponent[];
  readonly counter: readonly [string, boolean] | null;
  readonly suffixClass: string | null;
  readonly definitionSeq: number | null;
  readonly semanticMembers: readonly CandidateSemanticMember[];
  /** All roots represented by the physical row before an explicit selector. */
  readonly identityRoots?: readonly number[];
  readonly conjugationSelection: 'default' | 'explicit' | 'root';
}

export interface ScoredCandidate {
  readonly candidate: MaterializedCandidate;
  readonly score: number;
  readonly info: AnalyzerScoreInfo;
}

export const EMPTY_SEQUENCE_FACTS: AnalyzerSequenceFacts = Object.freeze({
  allArchived: false,
  preferKana: false,
  preferKanaOnOrdinalZero: false
});

export function union<T>(...lists: readonly (readonly T[])[]): T[] {
  return [...new Set(lists.flat())];
}

export function scoreModifier(multiplier = 0, constant = 0): AnalyzerScoreModifier {
  return { multiplier, constant };
}

export function sequenceFacts(
  roots: RootPayloadReader,
  entryIndex: number | null
): AnalyzerSequenceFacts {
  return entryIndex === null ? EMPTY_SEQUENCE_FACTS : {
    allArchived: roots.entryArchived(entryIndex),
    preferKana: roots.entryPreferKana(entryIndex),
    preferKanaOnOrdinalZero: roots.entryPreferKanaOnOrdinalZero(entryIndex)
  };
}

export function positions(roots: RootPayloadReader, entryIndex: number | null): string[] {
  if (entryIndex === null) return [];
  const result: string[] = [];
  for (let index = 0; index < roots.entryPosCount(entryIndex); index++) {
    result.push(roots.string(roots.entryPosStringIdAt(entryIndex, index)));
  }
  return result;
}

export function inflectionProperty(property: MorphologyProperty): PortableAnalysisInflection {
  return {
    pos: property.pos,
    type: property.type,
    negative: property.negative,
    formal: property.formal,
    ordinal: property.ordinal
  };
}

export function analyzerConjugation(
  physicalSeq: number,
  rootSeq: number,
  property: MorphologyProperty,
  secondary: boolean
): AnalyzerConjugation {
  return {
    seq: physicalSeq,
    from: rootSeq,
    via: secondary ? -rootSeq : null,
    property: {
      pos: property.pos,
      type: property.type,
      negative: property.negative,
      formal: property.formal
    }
  };
}

function candidateKey(candidate: Omit<MaterializedCandidate, 'key'>): string {
  const inflectionKey = (values: readonly PortableAnalysisInflection[]) => values.map(value => [
    value.pos, value.type, value.negative, value.formal, value.ordinal
  ]);
  return JSON.stringify([
    candidate.kind,
    candidate.route,
    candidate.text,
    candidate.root?.seq ?? null,
    candidate.root?.form ?? null,
    candidate.root?.reading ?? null,
    inflectionKey(candidate.inflection),
    candidate.components.map(value => [
      value.text,
      value.trueText,
      value.route,
      value.reading,
      value.publicSeq,
      value.root?.seq ?? null,
      value.root?.form ?? null,
      value.root?.reading ?? null,
      inflectionKey(value.inflection),
      value.primary
    ])
  ]);
}

export function withKey(candidate: Omit<MaterializedCandidate, 'key'>): MaterializedCandidate {
  return { ...candidate, key: candidateKey(candidate) };
}

export function dedupeCandidates(
  values: readonly MaterializedCandidate[]
): MaterializedCandidate[] {
  const seen = new Set<string>();
  const result: MaterializedCandidate[] = [];
  for (const value of values) {
    if (seen.has(value.key)) continue;
    seen.add(value.key);
    result.push(value);
  }
  return result;
}
