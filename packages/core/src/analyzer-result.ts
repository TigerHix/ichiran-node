import type { DetailStoreReader } from './details.js';
import {
  serializePortableLegacyCompact,
  serializePortableLegacyDetailed,
  type PortableLegacyPresentationFacts,
  type PortableLegacyPresentationValue,
  type PortableLegacySerializeOptions,
  type PortableLegacyTransformedResult
} from './analyzer-legacy.js';
import type { AnalyzerSupportReader, AnalyzerSupportRoute } from './analyzer-support.js';
import type {
  AnalyzerEntityHint,
  AnalyzerPathPart,
  AnalyzerScoreInfo,
  AnalyzerSegment,
  AnalyzerSegmentGroup
} from './analyzer-types.js';
import { getCharClass, testWord } from './characters.js';
import { romanizeWord } from './romanization.js';
import type { RootPayloadReader } from './root-payload.js';
import { selectAnalyzerAlternatives } from './analyzer-scoring.js';

export interface PortableAnalysisRoot {
  readonly seq: number;
  readonly form: string;
  readonly reading: string;
}

export interface PortableAnalysisInflection {
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
  readonly ordinal: number;
}

export interface PortableAnalysisComponent {
  readonly text: string;
  readonly trueText: string | null;
  readonly route: AnalyzerSupportRoute;
  readonly reading: string;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly primary: boolean;
}

export interface PortableAnalysisAlternative {
  /** Request-local identity only; never persist or compare across calls, packs, or runtimes. */
  readonly candidateId: number;
  readonly text: string;
  readonly trueText: string | null;
  readonly route: AnalyzerSupportRoute;
  readonly reading: string;
  readonly romanized: string;
  readonly pos: readonly string[];
  readonly score: number;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly components: readonly PortableAnalysisComponent[];
  readonly counter: readonly [string, boolean] | null;
}

export interface PortableAnalysisToken {
  /** Request-local identity only; `null` for gaps and unstable across calls, packs, or runtimes. */
  readonly candidateId: number | null;
  readonly start: number;
  readonly end: number;
  readonly text: string;
  readonly trueText: string | null;
  readonly route: AnalyzerSupportRoute | 'gap';
  readonly reading: string;
  readonly romanized: string;
  readonly pos: readonly string[];
  readonly score: number;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly components: readonly PortableAnalysisComponent[];
  readonly alternatives: readonly PortableAnalysisAlternative[];
  readonly skipped: number;
  readonly entity: boolean;
  readonly counter: readonly [string, boolean] | null;
}

export interface PortableAnalysisPath {
  readonly score: number;
  readonly tokens: readonly PortableAnalysisToken[];
}

export type PortableAnalysisChunk =
  | {
      readonly type: 'misc';
      readonly start: number;
      readonly end: number;
      readonly text: string;
    }
  | {
      readonly type: 'word';
      readonly start: number;
      readonly end: number;
      readonly text: string;
      /** Independently scored paths for this basicSplit word segment. */
      readonly paths: readonly PortableAnalysisPath[];
    };

export interface PortableAnalysisResult {
  readonly input: string;
  readonly normalized: string;
  readonly computeMs: number;
  /** Exact basicSplit ownership used by legacy romanize*. */
  readonly chunks: readonly PortableAnalysisChunk[];
  /** Deterministic top-N Cartesian merge of independent word-chunk paths. */
  readonly paths: readonly PortableAnalysisPath[];
}

interface ProjectionSemanticMember {
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly stageGroups: readonly (number | null)[];
  readonly stageKeys?: readonly (string | null)[];
  readonly stageMemberOrds: readonly (number | null)[];
  readonly stagePropOrds: readonly (number | null)[];
  readonly memberOrd: number | null;
}

interface ProjectionPresentationSource {
  readonly physicalGroup: number | null;
  readonly suffixClass: string | null;
  readonly definitionSeq: number | null;
  readonly semanticMembers: readonly ProjectionSemanticMember[];
  readonly identityRoots?: readonly number[];
  readonly conjugationSelection: 'default' | 'explicit' | 'root';
}

export interface AnalyzerProjectionComponent extends ProjectionPresentationSource {
  readonly text: string;
  readonly trueText: string | null;
  readonly route: AnalyzerSupportRoute;
  readonly reading: string;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly primary: boolean;
}

export interface AnalyzerProjectionCandidate extends ProjectionPresentationSource {
  readonly kind: 'simple' | 'proxy' | 'compound' | 'counter';
  readonly text: string;
  readonly trueText: string;
  readonly route: AnalyzerSupportRoute;
  readonly reading: string;
  readonly publicSeq: number | null;
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly components: readonly AnalyzerProjectionComponent[];
  readonly counter: readonly [string, boolean] | null;
}

export interface AnalyzerProjectionScoredCandidate {
  readonly candidate: AnalyzerProjectionCandidate;
  readonly info: AnalyzerScoreInfo;
}

export interface AnalyzerResultProjectorSource {
  readonly roots: RootPayloadReader;
  readonly support: AnalyzerSupportReader;
  readonly directSurface: (rank: number) => string;
  readonly hint: (
    definitionSeq: number,
    route: AnalyzerSupportRoute,
    surface: string,
    reading: string
  ) => string | null;
}

/** Owns public DTO construction and the legacy-only presentation metadata. */
export class AnalyzerResultProjector {
  readonly #source: AnalyzerResultProjectorSource;
  readonly #presentation = new WeakMap<
    PortableLegacyPresentationValue,
    PortableLegacyPresentationFacts
  >();

  constructor(source: AnalyzerResultProjectorSource) {
    this.#source = source;
  }

  serializeLegacy(
    result: PortableAnalysisResult,
    options: PortableLegacySerializeOptions = {}
  ): unknown {
    return serializePortableLegacyCompact(result, options, {
      presentationFacts: value => this.#presentation.get(value) ?? null
    });
  }

  serializeLegacyDetailed(
    result: PortableAnalysisResult,
    details: DetailStoreReader,
    options: PortableLegacySerializeOptions = {}
  ): Promise<PortableLegacyTransformedResult> {
    return serializePortableLegacyDetailed(result, details, {
      roots: this.#source.roots,
      support: this.#source.support,
      directSurface: this.#source.directSurface,
      hint: this.#source.hint,
      presentationFacts: value => this.#presentation.get(value) ?? null
    }, options);
  }

  shiftToken(token: PortableAnalysisToken, offset: number): PortableAnalysisToken {
    if (offset === 0) return token;
    const shifted: PortableAnalysisToken = {
      ...token,
      start: token.start + offset,
      end: token.end + offset
    };
    const facts = this.#presentation.get(token);
    if (facts) this.#presentation.set(shifted, facts);
    return shifted;
  }

  tokens(
    text: string,
    parts: readonly AnalyzerPathPart[],
    candidates: ReadonlyMap<number, AnalyzerProjectionScoredCandidate>,
    entities: readonly AnalyzerEntityHint[]
  ): PortableAnalysisToken[] {
    const groups = parts.filter((part): part is AnalyzerSegmentGroup => 'segments' in part);
    const tokens: PortableAnalysisToken[] = [];
    let offset = 0;
    for (const group of groups) {
      if (group.start > offset) tokens.push(this.gap(text, offset, group.start));
      const first = group.segments[0];
      if (!first) continue;
      if (first.entity) {
        const entityText = text.slice(group.start, group.end);
        tokens.push({
          candidateId: first.candidateId,
          start: group.start,
          end: group.end,
          text: entityText,
          trueText: null,
          route: testWord(entityText, 'kana') ? 'kana' : 'kanji',
          reading: entityText,
          romanized: romanizeWord(entityText),
          pos: ['proper-noun'],
          score: first.score,
          entryIndex: null,
          root: null,
          inflection: [],
          components: [],
          alternatives: [],
          skipped: 0,
          entity: true,
          counter: null
        });
        offset = group.end;
        continue;
      }
      const retained = selectAnalyzerAlternatives(group.segments);
      // Proxy ties use the lowest dictionary sequence for the clean scalar;
      // the legacy alternatives intentionally retain lookup order.
      const firstScored = candidates.get(first.candidateId);
      const primarySegment = firstScored?.candidate.kind === 'proxy'
        ? retained
          .filter(segment => segment.score === first.score)
          .map(segment => ({ segment, value: candidates.get(segment.candidateId) }))
          .filter((value): value is {
            segment: AnalyzerSegment;
            value: AnalyzerProjectionScoredCandidate;
          } => value.value?.candidate.kind === 'proxy')
          .sort((left, right) =>
            (left.value.candidate.publicSeq ?? Number.MAX_SAFE_INTEGER)
              - (right.value.candidate.publicSeq ?? Number.MAX_SAFE_INTEGER))[0]?.segment ?? first
        : first;
      const scored = candidates.get(primarySegment.candidateId);
      if (!scored) continue;
      const candidate = scored.candidate;
      const alternatives = retained.flatMap(segment => {
        const value = candidates.get(segment.candidateId);
        if (!value) return [];
        const alternative: PortableAnalysisAlternative = {
          candidateId: segment.candidateId,
          text: value.candidate.text,
          trueText: value.candidate.trueText === value.candidate.text
            ? null
            : value.candidate.trueText,
          route: value.candidate.route,
          reading: value.candidate.reading,
          romanized: romanizeWord(value.candidate.reading, {
            originalSpelling: value.candidate.text
          }),
          pos: value.info.positions,
          score: segment.score,
          entryIndex: value.candidate.entryIndex,
          root: value.candidate.root,
          inflection: value.candidate.inflection,
          components: value.candidate.components.map(component => this.#publicComponent(component)),
          counter: value.candidate.counter
        };
        return [this.#recordPresentation(alternative, value.candidate)];
      });
      const token: PortableAnalysisToken = {
        candidateId: primarySegment.candidateId,
        start: group.start,
        end: group.end,
        text: candidate.text,
        trueText: candidate.trueText === candidate.text ? null : candidate.trueText,
        route: candidate.route,
        reading: candidate.reading,
        romanized: romanizeWord(candidate.reading, { originalSpelling: candidate.text }),
        pos: scored.info.positions,
        score: primarySegment.score,
        entryIndex: candidate.entryIndex,
        root: candidate.root,
        inflection: candidate.inflection,
        components: candidate.components.map(component => this.#publicComponent(component)),
        alternatives,
        skipped: group.matches - alternatives.length,
        entity: entities.some(entity => entity.start === group.start && entity.end === group.end),
        counter: candidate.counter
      };
      tokens.push(this.#recordPresentation(token, candidate));
      offset = group.end;
    }
    if (offset < text.length) tokens.push(this.gap(text, offset, text.length));
    return this.#fixNani(tokens);
  }

  gap(text: string, start: number, end: number): PortableAnalysisToken {
    const value = text.slice(start, end);
    return {
      candidateId: null,
      start,
      end,
      text: value,
      trueText: null,
      route: 'gap',
      reading: value,
      romanized: romanizeWord(value),
      pos: [],
      score: 0,
      entryIndex: null,
      root: null,
      inflection: [],
      components: [],
      alternatives: [],
      skipped: 0,
      entity: false,
      counter: null
    };
  }

  #recordPresentation<T extends PortableLegacyPresentationValue>(
    value: T,
    source: ProjectionPresentationSource
  ): T {
    this.#presentation.set(value, {
      physicalGroup: source.physicalGroup,
      suffixClass: source.suffixClass,
      definitionSeq: source.definitionSeq,
      conjugationSelection: source.conjugationSelection,
      identityRoots: source.identityRoots ?? [...new Set(source.semanticMembers
        .flatMap(member => member.root === null ? [] : [member.root.seq]))],
      semanticMembers: source.semanticMembers.map(member => ({
        entryIndex: member.entryIndex,
        root: member.root,
        inflection: member.inflection,
        stageGroups: member.stageGroups,
        stageKeys: member.stageKeys,
        stageMemberOrds: member.stageMemberOrds,
        stagePropOrds: member.stagePropOrds,
        memberOrd: member.memberOrd
      }))
    });
    return value;
  }

  #publicComponent(component: AnalyzerProjectionComponent): PortableAnalysisComponent {
    return this.#recordPresentation({
      text: component.text,
      trueText: component.trueText,
      route: component.route,
      reading: component.reading,
      entryIndex: component.entryIndex,
      root: component.root,
      inflection: component.inflection,
      primary: component.primary
    }, component);
  }

  #fixNani(tokens: readonly PortableAnalysisToken[]): PortableAnalysisToken[] {
    const nanClasses = new Set([
      'ba', 'bi', 'bu', 'be', 'bo', 'pa', 'pi', 'pu', 'pe', 'po',
      'da', 'dji', 'dzu', 'de', 'do', 'za', 'ji', 'zu', 'ze', 'zo',
      'ta', 'chi', 'tsu', 'te', 'to', 'na', 'nu', 'ne', 'no',
      'ra', 'ri', 'ru', 're', 'ro'
    ]);
    const result = [...tokens];
    for (let index = 0; index + 1 < result.length; index++) {
      const token = result[index]!;
      const next = result[index + 1]!;
      if (token.text !== '何') continue;
      let nan = false;
      let nani = false;
      for (const value of [next.reading, ...next.alternatives.map(alternative => alternative.reading)]) {
        if (value.length === 0) continue;
        if (nanClasses.has(getCharClass(value[0]!))) nan = true;
        else nani = true;
      }
      const reading = nani ? 'なに' : nan ? 'なん' : null;
      if (!reading) continue;
      const replacement = {
        ...token,
        reading,
        romanized: romanizeWord(reading)
      };
      const facts = this.#presentation.get(token);
      if (facts) this.#presentation.set(replacement, { ...facts, contextualReading: true });
      result[index] = replacement;
    }
    return result;
  }
}

export type {
  PortableLegacySerializeOptions,
  PortableLegacyTransformedResult
};
