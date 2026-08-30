import {
  addAnalyzerEntityGroups,
  findAnalyzerPaths
} from './analyzer-paths.js';
import {
  filterAndCullAnalyzerSegments,
  scoreAnalyzerCandidate,
  selectAnalyzerAlternatives
} from './analyzer-scoring.js';
import type {
  AnalyzerConjugation,
  AnalyzerEntityHint,
  AnalyzerScoreCandidate,
  AnalyzerScoreInfo,
  AnalyzerScoreModifier,
  AnalyzerSegment,
  AnalyzerSegmentGroup,
  AnalyzerSequenceFacts,
  AnalyzerWordScoreFacts
} from './analyzer-types.js';
import {
  ITERATION_CHARACTERS,
  KANA_CHARACTERS,
  MODIFIER_CHARACTERS,
  asHiragana,
  basicSplit,
  consecutiveCharGroups,
  getCharClass,
  longVowelModifierP,
  normalize,
  sequentialKanjiPositions,
  testWord
} from './characters.js';
import type { MorphologyCandidate, MorphologyProperty } from './morphology.js';
import { MorphologyReader } from './morphology.js';
import { materializeAnalyzerCounter } from './analyzer-counters.js';
import {
  type AnalyzerSupportCounterVariant,
  type AnalyzerSupportRoute,
  type AnalyzerSupportSplit,
  type AnalyzerSupportSplitPart,
  type AnalyzerSupportSuffixMatch,
  type AnalyzerSupportSuffixForm,
  AnalyzerSupportReader
} from './analyzer-support.js';
import { joinRomanizedParts, romanizeWord, type RomanizationName } from './romanization.js';
import { RootPayloadReader } from './root-payload.js';
import { SurfaceIndex, type SurfaceMatch } from './surface-index.js';
import {
  serializePortableLegacyCompact,
  serializePortableLegacyDetailed,
  type PortableLegacyPresentationFacts,
  type PortableLegacyPresentationValue,
  type PortableLegacySerializeOptions,
  type PortableLegacyTransformedResult
} from './analyzer-legacy.js';
import type { DetailStoreReader } from './details.js';
import type {
  AnalyzerGeneratedFacts,
  AnalyzerGeneratedMember
} from './analyzer-annotations.js';

export interface PortableAnalyzerAnnotations {
  split(
    definitionSeq: number,
    route: AnalyzerSupportRoute,
    surface: string,
    kind?: 'split' | 'segsplit'
  ): AnalyzerSupportSplit | null;
  hint(
    definitionSeq: number,
    route: AnalyzerSupportRoute,
    surface: string,
    reading: string
  ): string | null;
  generated?(
    rootSeq: number,
    aliases: readonly [number] | readonly [number, number]
  ): AnalyzerGeneratedFacts | null;
  lookupOrder?(
    route: AnalyzerSupportRoute,
    surface: string,
    rootSeq: number,
    aliases: readonly [number] | readonly [number, number] | null
  ): number | null;
}

export interface PortableAnalyzerSource {
  readonly surface: SurfaceIndex;
  readonly roots: RootPayloadReader;
  readonly morphology: MorphologyReader;
  readonly support: AnalyzerSupportReader;
  /** Defaults to the hot support reader in tests/legacy packs. */
  readonly annotations?: PortableAnalyzerAnnotations;
}

export interface PortableAnalyzeOptions {
  readonly limit?: number;
  readonly entities?: readonly AnalyzerEntityHint[];
  /** Match romanize* defaults: punctuation is preserved unless explicitly normalized. */
  readonly normalizePunctuation?: boolean;
}

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

interface CandidateComponent {
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

interface CandidateSemanticMember {
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

interface MaterializedCandidate {
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

interface ScoredCandidate {
  readonly candidate: MaterializedCandidate;
  readonly score: number;
  readonly info: AnalyzerScoreInfo;
}

const EMPTY_SEQUENCE_FACTS: AnalyzerSequenceFacts = Object.freeze({
  allArchived: false,
  preferKana: false,
  preferKanaOnOrdinalZero: false
});

const FORCE_KANJI_BREAK = new Set(['です']);
const NO_KANJI_BREAK = new Set(['日置']);
const UNIQUE_SUFFIXES = new Set([
  ':ra', ':nai-n', ':dewanai', ':eba', ':teba', ':reba', ':keba', ':geba',
  ':neba', ':beba', ':meba', ':seba', ':ii', ':mo', ':nikui', ':gai'
]);
const CONDITIONAL_ABBREVIATIONS: Readonly<Record<string, string>> = Object.freeze({
  ':teba': 'てば',
  ':reba': 'れば',
  ':keba': 'けば',
  ':geba': 'げば',
  ':neba': 'ねば',
  ':beba': 'べば',
  ':meba': 'めば',
  ':seba': 'せば'
});

function union<T>(...lists: readonly (readonly T[])[]): T[] {
  return [...new Set(lists.flat())];
}

function scoreModifier(multiplier = 0, constant = 0): AnalyzerScoreModifier {
  return { multiplier, constant };
}

function monotonicNow(): number {
  const clock = (globalThis as unknown as {
    readonly performance?: { now(): number };
  }).performance;
  return clock?.now() ?? Date.now();
}

function sequenceFacts(
  roots: RootPayloadReader,
  entryIndex: number | null
): AnalyzerSequenceFacts {
  return entryIndex === null ? EMPTY_SEQUENCE_FACTS : {
    allArchived: roots.entryArchived(entryIndex),
    preferKana: roots.entryPreferKana(entryIndex),
    preferKanaOnOrdinalZero: roots.entryPreferKanaOnOrdinalZero(entryIndex)
  };
}

function positions(roots: RootPayloadReader, entryIndex: number | null): string[] {
  if (entryIndex === null) return [];
  const result: string[] = [];
  for (let index = 0; index < roots.entryPosCount(entryIndex); index++) {
    result.push(roots.string(roots.entryPosStringIdAt(entryIndex, index)));
  }
  return result;
}

function inflectionProperty(property: MorphologyProperty): PortableAnalysisInflection {
  return {
    pos: property.pos,
    type: property.type,
    negative: property.negative,
    formal: property.formal,
    ordinal: property.ordinal
  };
}

function analyzerConjugation(
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

function withKey(candidate: Omit<MaterializedCandidate, 'key'>): MaterializedCandidate {
  return { ...candidate, key: candidateKey(candidate) };
}

function stickyPositions(input: string): Set<number> {
  const modifiers = new Set([
    ...Object.keys(MODIFIER_CHARACTERS),
    ...Object.keys(ITERATION_CHARACTERS)
  ]);
  const kana = new Set(Object.keys(KANA_CHARACTERS));
  const result = new Set<number>();
  for (let offset = 0; offset < input.length; offset++) {
    const charClass = getCharClass(input[offset]!);
    if (charClass === 'sokuon' && offset + 1 < input.length) {
      if (kana.has(getCharClass(input[offset + 1]!))) result.add(offset + 1);
    } else if (modifiers.has(charClass)) {
      const permittedEnd = offset === input.length - 1 && (
        charClass === 'longVowel'
        || (offset > 0 && longVowelModifierP(charClass, input[offset - 1]!))
      );
      if (!permittedEnd) result.add(offset);
    }
  }
  return result;
}

/**
 * Self-contained analyzer over the resident indexes plus seekable annotations.
 *
 * Lookup and scoring are synchronous. Cold details remain a separate concern;
 * the Worker keeps generated blocks resident and retries the rare split/hint
 * miss after loading it through the small synchronous interface above.
 */
export class PortableAnalyzer {
  readonly #surface: SurfaceIndex;
  readonly #roots: RootPayloadReader;
  readonly #morphology: MorphologyReader;
  readonly #support: AnalyzerSupportReader;
  readonly #annotations: PortableAnalyzerAnnotations;
  readonly #lexicalCache = new Map<string, readonly MaterializedCandidate[]>();
  readonly #fullCache = new Map<string, readonly MaterializedCandidate[]>();
  readonly #rootFormCache = new Map<string, number | null>();
  readonly #scoreSplitCache = new Map<string, AnalyzerWordScoreFacts['split']>();
  readonly #scoreSplitInProgress = new Set<string>();
  readonly #presentation = new WeakMap<
    PortableLegacyPresentationValue,
    PortableLegacyPresentationFacts
  >();

  constructor(source: PortableAnalyzerSource) {
    this.#surface = source.surface;
    this.#roots = source.roots;
    this.#morphology = source.morphology;
    this.#support = source.support;
    this.#annotations = source.annotations ?? source.support;
    if (this.#surface.manifest.directCount !== this.#roots.surfaceCount) {
      throw new Error(
        `Surface/root rank mismatch: ${this.#surface.manifest.directCount} != ${this.#roots.surfaceCount}`
      );
    }
  }

  analyze(input: string, options: PortableAnalyzeOptions = {}): PortableAnalysisResult {
    const started = monotonicNow();
    // These caches deduplicate recursive suffix/split lookup within one request.
    // Keeping them across requests would retain decoded annotation objects and
    // eventually defeat the bounded missing-block preload/retry protocol.
    this.#lexicalCache.clear();
    this.#fullCache.clear();
    this.#rootFormCache.clear();
    this.#scoreSplitCache.clear();
    this.#scoreSplitInProgress.clear();
    const normalized = normalize(input, undefined, !(options.normalizePunctuation ?? false));
    const entities = options.entities ?? [];
    const limit = options.limit ?? 5;
    const chunks: PortableAnalysisChunk[] = [];
    let paths: PortableAnalysisPath[] = [{ score: 0, tokens: [] }];
    let offset = 0;

    for (const segment of basicSplit(normalized)) {
      const start = offset;
      const end = start + segment.text.length;
      if (segment.type === 'misc') {
        chunks.push({ type: 'misc', start, end, text: segment.text });
        const token = this.#gap(normalized, start, end);
        paths = paths.map(path => ({ ...path, tokens: [...path.tokens, token] }));
      } else {
        const localEntities = entities
          .filter(entity => entity.start >= start && entity.end <= end)
          .map(entity => ({
            start: entity.start - start,
            end: entity.end - start,
            boost: entity.boost
          }));
        const localPaths = this.#analyzeWord(segment.text, limit, localEntities)
          .map(path => ({
            score: path.score,
            tokens: path.tokens.map(token => this.#shiftToken(token, start))
          }));
        chunks.push({ type: 'word', start, end, text: segment.text, paths: localPaths });
        paths = this.#mergePaths(paths, localPaths, limit);
      }
      offset = end;
    }

    return {
      input,
      normalized,
      computeMs: monotonicNow() - started,
      chunks,
      paths
    };
  }

  romanize(
    input: string,
    options: PortableAnalyzeOptions & { readonly method?: RomanizationName } = {}
  ): string {
    const analysis = this.analyze(input, {
      limit: 1,
      entities: options.entities,
      normalizePunctuation: options.normalizePunctuation
    });
    if (!analysis.paths[0]) {
      const normalized = normalize(input, undefined, !(options.normalizePunctuation ?? false));
      return romanizeWord(normalized, { method: options.method });
    }
    return joinRomanizedParts(analysis.chunks.flatMap(chunk => chunk.type === 'misc'
      ? [chunk.text]
      : (chunk.paths[0]?.tokens ?? []).map(token => romanizeWord(token.reading, {
          method: options.method,
          originalSpelling: token.text
        }))));
  }

  /**
   * Compact raw WordInfo-like romanize* nesting. This deliberately does not
   * claim transformRomanizeStarResult gloss/conjugation presentation parity.
   */
  serializeLegacy(
    result: PortableAnalysisResult,
    options: PortableLegacySerializeOptions = {}
  ): unknown {
    return serializePortableLegacyCompact(result, options, {
      presentationFacts: value => this.#presentation.get(value) ?? null
    });
  }

  /** Cold detail hydration for transformed romanize* output. */
  serializeLegacyDetailed(
    result: PortableAnalysisResult,
    details: DetailStoreReader,
    options: PortableLegacySerializeOptions = {}
  ): Promise<PortableLegacyTransformedResult> {
    return serializePortableLegacyDetailed(result, details, {
      roots: this.#roots,
      support: this.#support,
      directSurface: rank => this.#surface.directSurface(rank),
      hint: (definitionSeq, route, surface, reading) =>
        this.#annotations.hint(definitionSeq, route, surface, reading),
      presentationFacts: value => this.#presentation.get(value) ?? null
    }, options);
  }

  #analyzeWord(
    text: string,
    limit: number,
    entities: readonly AnalyzerEntityHint[]
  ): PortableAnalysisPath[] {
    const { groups, candidates } = this.#groups(text);
    const withEntities = addAnalyzerEntityGroups(groups, entities, text);
    return findAnalyzerPaths(withEntities, text.length, { limit, entities }).map(path => ({
      score: path.score,
      tokens: this.#tokens(text, path.parts, candidates, entities)
    }));
  }

  #mergePaths(
    left: readonly PortableAnalysisPath[],
    right: readonly PortableAnalysisPath[],
    limit: number
  ): PortableAnalysisPath[] {
    const merged: PortableAnalysisPath[] = [];
    for (const prefix of left) {
      for (const suffix of right) {
        merged.push({
          score: prefix.score + suffix.score,
          tokens: [...prefix.tokens, ...suffix.tokens]
        });
      }
    }
    // Array.sort is stable; nested generation preserves each chunk's path tie order.
    merged.sort((a, b) => b.score - a.score);
    return merged.slice(0, limit);
  }

  #shiftToken(token: PortableAnalysisToken, offset: number): PortableAnalysisToken {
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

  #recordPresentation<T extends PortableLegacyPresentationValue>(
    value: T,
    source: Pick<
      MaterializedCandidate,
      | 'physicalGroup'
      | 'suffixClass'
      | 'definitionSeq'
      | 'semanticMembers'
      | 'identityRoots'
      | 'conjugationSelection'
    >
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

  #publicComponent(component: CandidateComponent): PortableAnalysisComponent {
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

  #groups(text: string): {
    groups: AnalyzerSegmentGroup[];
    candidates: Map<number, ScoredCandidate>;
  } {
    const sticky = stickyPositions(text);
    const katakanaEnds = new Map(consecutiveCharGroups('katakana', text).map(value => [value[0], value[1]]));
    const numberEnds = new Map(consecutiveCharGroups('number', text).map(value => [value[0], value[1]]));
    const suffixesByStart: AnalyzerSupportSuffixMatch[][] = Array.from(
      { length: text.length },
      () => []
    );
    // Decode candidate suffix strings once per input end, not once for every
    // possible root start. References are then distributed over the 50-unit
    // analyzer window without allocating speculative substrings.
    for (let end = 1; end <= text.length; end++) {
      for (const match of this.#support.suffixMatchesEndingAt(text, end)) {
        for (let start = Math.max(0, end - 50); start < match.start; start++) {
          suffixesByStart[start]!.push(match);
        }
      }
    }
    const raw: Array<{
      start: number;
      end: number;
      candidates: MaterializedCandidate[];
    }> = [];
    const reachableEnds = new Set<number>();
    const kanjiBreak = new Set<number>();

    for (let start = 0; start < text.length; start++) {
      if (sticky.has(start)) continue;
      const byEnd = new Map<number, MaterializedCandidate[]>();
      for (const match of this.#surface.scan(text, start)) {
        if (sticky.has(match.end)) continue;
        const surface = text.slice(start, match.end);
        const values = this.#lexical(surface, match);
        if (values.length > 0) byEnd.set(match.end, values);
      }

      // Suffix-only composites need not be accepted by the direct/morphology FST.
      const maxEnd = Math.min(text.length, start + 50);
      const suffixCandidates = new Map<number, MaterializedCandidate[]>();
      for (const match of suffixesByStart[start]!) {
        if (sticky.has(match.end)) continue;
        const direct = byEnd.get(match.end) ?? [];
        const suffixes: MaterializedCandidate[] = [];
        const root = text.slice(start, match.start);
        const surface = text.slice(start, match.end);
        for (const value of match.values) {
          const suffixClass = value.form
            ? this.#support.suffixClass(value.form.seq) ?? value.keyword
            : value.keyword;
          if (direct.length > 0 && this.#uniqueSuffix(suffixClass, direct)) continue;
          suffixes.push(...this.#applySuffix(
            value.keyword, root, match.text, value.form, surface, 0
          ));
        }
        if (suffixes.length > 0) {
          const current = suffixCandidates.get(match.end) ?? [];
          current.push(...suffixes);
          suffixCandidates.set(match.end, current);
        }
      }
      for (const [end, suffixes] of suffixCandidates) {
        byEnd.set(end, this.#dedupe([...(byEnd.get(end) ?? []), ...suffixes]));
      }

      const katakanaEnd = katakanaEnds.get(start);
      if (katakanaEnd !== undefined && katakanaEnd <= maxEnd && !sticky.has(katakanaEnd)) {
        const surface = text.slice(start, katakanaEnd);
        const existing = byEnd.get(katakanaEnd) ?? [];
        const simpleMatches = existing.filter(value => value.kind === 'simple');
        const proxies = this.#katakanaProxy(surface, simpleMatches);
        if (proxies.length > 0) byEnd.set(katakanaEnd, this.#dedupe([...existing, ...proxies]));
      }

      const numberEnd = numberEnds.get(start);
      if (numberEnd !== undefined) {
        const numberText = text.slice(start, numberEnd);
        // The empty counter key is the core `NumberText` class. It competes
        // with a raw gap even when no counter suffix follows the number.
        if (!sticky.has(numberEnd)) {
          const direct = byEnd.get(numberEnd) ?? [];
          const numbers = this.#counters(
            numberText,
            '',
            direct.length === 0,
            this.#support.counters('')
          );
          if (numbers.length > 0) byEnd.set(numberEnd, [...direct, ...numbers]);
        }
        for (const match of this.#support.counterMatchesStartingAt(
          text,
          numberEnd,
          maxEnd - numberEnd
        )) {
          const end = match.end;
          if (sticky.has(end)) continue;
          const direct = byEnd.get(end) ?? [];
          const counters = this.#counters(
            numberText,
            match.text,
            direct.length === 0,
            match.values
          );
          if (counters.length > 0) byEnd.set(end, [...direct, ...counters]);
        }
      }

      for (const [end, values] of byEnd) {
        if (values.length === 0) continue;
        if (start === 0 || reachableEnds.has(start)) {
          const surface = text.slice(start, end);
          if (FORCE_KANJI_BREAK.has(surface)) {
            for (let offset = start + 1; offset < end; offset++) kanjiBreak.add(offset);
          } else if (!NO_KANJI_BREAK.has(surface)) {
            for (const offset of sequentialKanjiPositions(surface, start)) kanjiBreak.add(offset);
          }
        }
        reachableEnds.add(end);
        raw.push({ start, end, candidates: this.#dedupe(values) });
      }
    }

    const candidates = new Map<number, ScoredCandidate>();
    const groups: AnalyzerSegmentGroup[] = [];
    let candidateId = 1;
    let groupId = 1;
    for (const group of raw) {
      const kb = [group.start, group.end]
        .filter(value => kanjiBreak.has(value))
        .map(value => value - group.start);
      const segments: AnalyzerSegment[] = [];
      let matches = group.candidates.length;
      for (const candidate of group.candidates) {
        const scoreFacts = kb.length > 0
          ? this.#withSuruBreak(candidate.scoreFacts)
          : candidate.scoreFacts;
        const scored = scoreAnalyzerCandidate(scoreFacts, {
          final: group.end === text.length || (text.endsWith('ー') && group.end === text.length - 1),
          kanjiBreak: kb
        });
        const currentId = candidateId++;
        candidates.set(currentId, { candidate, score: scored.score, info: scored.info });
        segments.push({
          candidateId: currentId,
          start: group.start,
          end: group.end,
          score: scored.score,
          common: scored.info.common,
          entity: false,
          rules: {
            text: candidate.text,
            wordKind: candidate.kind,
            scoreInfo: scored.info,
            compoundEndSeq: candidate.kind === 'compound'
              ? candidate.components[candidate.components.length - 1]?.publicSeq ?? null
              : null,
            compoundEndText: candidate.kind === 'compound'
              ? candidate.components[candidate.components.length - 1]?.text ?? null
              : null
          }
        });

        const segsplit = this.#segmentSplit(candidate);
        if (segsplit) {
          const segScore = scored.score + segsplit.addedScore;
          const segId = candidateId++;
          const segInfo = scoreAnalyzerCandidate(segsplit.candidate.scoreFacts).info;
          candidates.set(segId, { candidate: segsplit.candidate, score: segScore, info: segInfo });
          segments.push({
            candidateId: segId,
            start: group.start,
            end: group.end,
            score: segScore,
            common: segInfo.common,
            entity: false,
            rules: {
              text: segsplit.candidate.text,
              wordKind: 'compound',
              scoreInfo: segInfo,
              compoundEndSeq: segsplit.candidate.components.at(-1)?.publicSeq ?? null,
              compoundEndText: segsplit.candidate.components.at(-1)?.text ?? null
            }
          });
          matches++;
        }
      }
      const retained = filterAndCullAnalyzerSegments(segments);
      if (retained.length > 0) {
        groups.push({
          groupId: groupId++,
          start: group.start,
          end: group.end,
          segments: retained,
          matches
        });
      }
    }
    return { groups, candidates };
  }

  #lexical(surface: string, known?: SurfaceMatch | null): MaterializedCandidate[] {
    const cached = this.#lexicalCache.get(surface);
    if (cached) return [...cached];
    const match = known === undefined ? this.#surface.lookup(surface) : known;
    if (!match) return [];
    const result: MaterializedCandidate[] = [];
    if (match.direct && match.directRank !== null) {
      const start = this.#roots.surfaceFormStart(match.directRank);
      const count = this.#roots.surfaceFormCount(match.directRank);
      for (let offset = 0; offset < count; offset++) {
        result.push(this.#direct(surface, start + offset));
      }
    }
    let hasMorphologyCandidate = false;
    if (match.morphology) {
      const morphology = this.#morphology.lookup(surface, match.route);
      hasMorphologyCandidate = morphology.length > 0;
      for (const value of morphology) {
        result.push(this.#morph(value));
      }
    }
    const grouped = this.#groupPhysical(this.#dedupe(result));
    const ordered = hasMorphologyCandidate && grouped.length > 1
      ? this.#sortLookupOrder(match.route, surface, grouped)
      : grouped;
    this.#lexicalCache.set(surface, ordered);
    return [...ordered];
  }

  #sortLookupOrder(
    route: AnalyzerSupportRoute,
    surface: string,
    values: readonly MaterializedCandidate[]
  ): MaterializedCandidate[] {
    const lookupOrder = this.#annotations.lookupOrder?.bind(this.#annotations);
    if (!lookupOrder) return [...values];
    const ranked = values.map((value, index) => {
      if (value.lookupLocators.length === 0) {
        throw new Error(`Incomplete analyzer lookup order for ${JSON.stringify(surface)}`);
      }
      const ranks = new Set<number>();
      for (const locator of value.lookupLocators) {
        const rank = lookupOrder(route, surface, locator.rootSeq, locator.aliases);
        if (rank === null) {
          throw new Error(
            `Incomplete analyzer lookup order for ${JSON.stringify(surface)} at `
            + `${locator.rootSeq}:${locator.aliases?.join(',') ?? 'direct'}`
          );
        }
        ranks.add(rank);
      }
      if (ranks.size !== 1) {
        throw new Error(`Physical analyzer group has conflicting lookup orders: ${[...ranks].join(', ')}`);
      }
      return { value, index, rank: ranks.values().next().value! };
    });
    return ranked
      .sort((left, right) => left.rank - right.rank || left.index - right.index)
      .map(({ value }) => value);
  }

  #lookupLocators(values: readonly MaterializedCandidate[]): MaterializedCandidate['lookupLocators'] {
    const seen = new Set<string>();
    const result: Array<MaterializedCandidate['lookupLocators'][number]> = [];
    for (const value of values) {
      for (const locator of value.lookupLocators) {
        const key = `${locator.rootSeq}\u0000${locator.aliases?.join(',') ?? ''}`;
        if (seen.has(key)) continue;
        seen.add(key);
        result.push(locator);
      }
    }
    return result;
  }

  #memberSequenceFacts(
    allMembers: readonly CandidateSemanticMember[],
    selectedMembers: readonly CandidateSemanticMember[]
  ): AnalyzerSequenceFacts[] {
    const archivedIntermediates = new Set(allMembers.flatMap(member => {
      if (member.viaSeq === null || member.entryIndex === null) return [];
      return this.#roots.entryArchived(member.entryIndex) ? [member.viaSeq] : [];
    }));
    return selectedMembers.map(member => {
      const facts = sequenceFacts(this.#roots, member.entryIndex);
      const rootSeq = member.root?.seq ?? member.publicSeq;
      return rootSeq !== null && archivedIntermediates.has(rootSeq)
        ? { ...facts, allArchived: true }
        : facts;
    });
  }

  #groupPhysical(values: readonly MaterializedCandidate[]): MaterializedCandidate[] {
    const groups = new Map<string, MaterializedCandidate[]>();
    values.forEach((value, index) => {
      const key = value.physicalGroup !== null
        ? `group:${value.physicalGroup}`
        : value.physicalKey.length > 0
          ? value.physicalKey
          : `unique:${index}`;
      const members = groups.get(key) ?? [];
      members.push(value);
      groups.set(key, members);
    });
    return [...groups.values()].map(group => group.length === 1
      ? group[0]!
      : this.#mergePhysical(group));
  }

  #mergePhysical(values: readonly MaterializedCandidate[]): MaterializedCandidate {
    const ordered = [...values].sort((left, right) => {
      const leftDirect = left.inflection.length === 0 ? 0 : 1;
      const rightDirect = right.inflection.length === 0 ? 0 : 1;
      return leftDirect - rightDirect
        || (left.memberOrd ?? Number.MAX_SAFE_INTEGER)
          - (right.memberOrd ?? Number.MAX_SAFE_INTEGER);
    });
    const base = ordered[0]!;
    const lookupLocators = this.#lookupLocators(values);
    const wordValues = ordered.filter((value): value is MaterializedCandidate & {
      readonly scoreFacts: AnalyzerWordScoreFacts;
    } => value.scoreFacts.kind !== 'compound');
    const wordFacts = wordValues.map(value => value.scoreFacts);
    if (wordFacts.length !== ordered.length) return base;

    // A generated dictionary row can be reached by several semantic roots.
    // Core scores the one physical row, whose getConjData call first drops all
    // via rows whenever at least one direct row exists. Facts inherited from a
    // discarded via root must not leak into commonness, POS, or sequence flags.
    const allConjugations = wordFacts.flatMap(value => value.conjugations);
    const secondaryOnly = allConjugations.length > 0
      && allConjugations.every(value => value.via !== null);
    const selectedWordValues = wordValues.filter(value =>
      value.scoreFacts.conjugations.some(conjugation =>
        secondaryOnly ? conjugation.via !== null : conjugation.via === null));
    const targetValue = wordValues.find(value => value.inflection.length === 0) ?? null;
    const scoringWordValues = [
      ...(targetValue ? [targetValue] : []),
      ...selectedWordValues
    ];
    const scoringWordFacts = scoringWordValues.map(value => value.scoreFacts);

    const compareCommon = (left: number, right: number): number =>
      left === 0 ? -1 : right === 0 ? 1 : left - right;
    const inheritedCommon = selectedWordValues.map(value => value.scoreFacts)
      .flatMap(value => value.inheritedCommon === null ? [] : [value.inheritedCommon])
      .sort(compareCommon)[0] ?? null;
    const inheritedOrd = Math.min(...selectedWordValues.map(value => value.scoreFacts)
      .flatMap(value => value.inheritedOrd === null ? [] : [value.inheritedOrd]));
    const baseFacts = wordFacts[0]!;
    const entries = wordFacts.flatMap(value => value.entry ? [value.entry] : []);

    // isArch propagates from an archived root to each generated target. When
    // that target is itself a lexical root, the pack's lexical sense flag is
    // false but core still treats it as archived. Two-stage members retain the
    // exact lexical intermediate so this one-hop closure does not conflate
    // unrelated roots that happen to share a conjugation property.
    const allMembers = ordered.flatMap(value => value.semanticMembers);
    const selectedMembers = ordered
      .flatMap(value => value.semanticMembers)
      .filter(member => member.inflection.length > 0
        && (secondaryOnly
          ? member.inflection.length > 1
          : member.inflection.length === 1));
    const selectedSequenceFacts = this.#memberSequenceFacts(allMembers, selectedMembers);
    const rawRootFacts = ordered
      .flatMap(value => value.semanticMembers)
      .filter(member => member.inflection.length > 0)
      .map(member => sequenceFacts(this.#roots, member.entryIndex));
    const targetFacts = targetValue?.scoreFacts.self ?? baseFacts.self;
    const self: AnalyzerSequenceFacts = {
      allArchived: targetFacts.allArchived
        || rawRootFacts.some(value => value.allArchived),
      preferKana: targetFacts.preferKana,
      preferKanaOnOrdinalZero: targetFacts.preferKanaOnOrdinalZero
    };
    const lineage: AnalyzerSequenceFacts = {
      allArchived: self.allArchived
        && selectedSequenceFacts.length > 0
        && selectedSequenceFacts.every(value => value.allArchived),
      preferKana: self.preferKana
        || selectedSequenceFacts.some(value => value.preferKana),
      preferKanaOnOrdinalZero: self.preferKanaOnOrdinalZero
        || selectedSequenceFacts.some(value => value.preferKanaOnOrdinalZero)
    };
    const scoreFacts: AnalyzerWordScoreFacts = {
      ...baseFacts,
      seq: base.physicalSeq,
      ord: Math.min(...wordFacts.map(value => value.ord)),
      common: targetValue?.scoreFacts.common ?? null,
      nokanji: targetValue?.scoreFacts.nokanji ?? baseFacts.nokanji,
      entry: entries.length === 0 ? null : {
        root: entries.some(value => value.root),
        nKanji: entries[0]!.nKanji,
        primaryNokanji: entries.some(value => value.primaryNokanji)
      },
      conjugationOnly: wordFacts.every(value => value.conjugationOnly),
      conjugations: allConjugations,
      positions: union(...scoringWordFacts.map(value => value.positions)),
      self,
      lineage,
      inheritedCommon,
      inheritedOrd: Number.isFinite(inheritedOrd) ? inheritedOrd : null,
      split: wordFacts.find(value => value.split !== null)?.split ?? null
    };
    const seenMembers = new Set<string>();
    const semanticMembers = ordered
      .flatMap(value => value.semanticMembers)
      .sort((left, right) =>
        (left.memberOrd ?? Number.MAX_SAFE_INTEGER)
          - (right.memberOrd ?? Number.MAX_SAFE_INTEGER))
      .filter(member => {
        const key = JSON.stringify([
          member.publicSeq,
          member.inflection,
          member.stageGroups,
          member.stageKeys ?? [],
          member.stageMemberOrds,
          member.stagePropOrds
        ]);
        if (seenMembers.has(key)) return false;
        seenMembers.add(key);
        return true;
      });
    return withKey({
      ...base,
      physicalGroup: ordered.find(value => value.physicalGroup !== null)?.physicalGroup ?? null,
      lookupLocators,
      memberOrd: ordered
        .flatMap(value => value.memberOrd === null ? [] : [value.memberOrd])
        .sort((left, right) => left - right)[0] ?? null,
      scoreFacts,
      semanticMembers,
      identityRoots: [...new Set(semanticMembers.flatMap(member =>
        member.root === null ? [] : [member.root.seq]))]
    });
  }

  #direct(surface: string, form: number): MaterializedCandidate {
    const entryIndex = this.#roots.formEntryIndex(form);
    const seq = this.#roots.entrySeq(entryIndex);
    const route = this.#roots.formRoute(form);
    const best = this.#roots.resolveSurfaceReference(
      this.#roots.formBestReference(form),
      value => this.#surface.directSurface(value)
    );
    const root = route === 'kanji'
      ? { seq, form: surface, reading: best ?? surface }
      : { seq, form: best ?? surface, reading: surface };
    const facts = sequenceFacts(this.#roots, entryIndex);
    const split = this.#scoreSplit(seq, route, surface);
    const word: AnalyzerWordScoreFacts = {
      kind: 'word',
      text: surface,
      trueText: surface,
      trueTextFollowsText: true,
      route,
      seq,
      ord: this.#roots.formOrdinal(form),
      common: this.#roots.formCommon(form),
      nokanji: this.#roots.formNokanji(form),
      entry: {
        root: true,
        nKanji: this.#roots.entryNKanji(entryIndex),
        primaryNokanji: this.#roots.entryPrimaryNokanji(entryIndex)
      },
      conjugationOnly: false,
      conjugations: [],
      positions: positions(this.#roots, entryIndex),
      self: facts,
      lineage: facts,
      inheritedCommon: null,
      inheritedOrd: null,
      split,
      suruBreak: null
    };
    let reading = root.reading;
    reading = this.#annotations.hint(seq, route, surface, reading) ?? reading;
    return withKey({
      kind: 'simple', text: surface, trueText: surface, route, reading,
      publicSeq: seq, physicalSeq: seq, physicalKey: `seq:${seq}`,
      physicalGroup: null,
      lookupLocators: [{ rootSeq: seq, aliases: null }],
      memberOrd: null,
      entryIndex, root,
      inflection: [], scoreFacts: word, components: [], counter: null,
      suffixClass: this.#support.suffixClass(seq), definitionSeq: seq,
      conjugationSelection: 'default',
      semanticMembers: [{
        entryIndex,
        root,
        inflection: [],
        publicSeq: seq,
        physicalGroup: null,
        memberOrd: null,
        targetNKanji: this.#roots.entryNKanji(entryIndex),
        targetNKana: this.#roots.entryNKana(entryIndex),
        viaSeq: null,
        stageGroups: [],
        stageMemberOrds: [],
        stagePropOrds: []
      }]
    });
  }

  #morph(value: MorphologyCandidate): MaterializedCandidate {
    const entryIndex = this.#roots.findEntryIndex(value.rootSeq);
    const collision = this.#support.collision(
      value.rootSeq, value.route, value.surface, value.ruleIds
    );
    let generated: AnalyzerGeneratedFacts | null = null;
    let generatedVia: AnalyzerGeneratedFacts | null = null;
    const aliases = this.#support.generatedAliases(value.ruleIds);
    if (collision === null && this.#annotations.generated) {
      generated = this.#annotations.generated(value.rootSeq, aliases);
      if (aliases.length === 2) {
        generatedVia = this.#annotations.generated(value.rootSeq, [aliases[0]]);
      }
    }
    const physicalSeq = collision?.collisionSeq ?? -value.rootSeq;
    const rootFacts = sequenceFacts(this.#roots, entryIndex >= 0 ? entryIndex : null);
    const self = collision ? {
      allArchived: collision.archived,
      preferKana: collision.preferKana,
      preferKanaOnOrdinalZero: collision.preferKanaOnOrdinalZero
    } : {
      ...EMPTY_SEQUENCE_FACTS,
      // isArch includes every physical target generated from an archived root.
      allArchived: rootFacts.allArchived
    };
    const lineage = collision ? {
      allArchived: self.allArchived && rootFacts.allArchived,
      preferKana: self.preferKana || rootFacts.preferKana,
      preferKanaOnOrdinalZero: self.preferKanaOnOrdinalZero || rootFacts.preferKanaOnOrdinalZero
    } : rootFacts;
    const rootPos = positions(this.#roots, entryIndex >= 0 ? entryIndex : null);
    const sourceForm = this.#rootForm(value.sourceText, value.rootSeq, value.route);
    const fallbackInflection = value.path.map(inflectionProperty);
    const root: PortableAnalysisRoot = {
      seq: value.rootSeq,
      form: value.sourceForm,
      reading: value.sourceReading
    };
    const targetNKanji = collision?.nKanji
      ?? generated?.nKanji
      ?? (entryIndex >= 0 ? this.#roots.entryNKanji(entryIndex) : null);
    const targetNKana = collision?.nKana
      ?? generated?.nKana
      ?? (entryIndex >= 0 ? this.#roots.entryNKana(entryIndex) : null);
    const exactProperty = (
      member: AnalyzerGeneratedMember,
      fallback: PortableAnalysisInflection
    ): PortableAnalysisInflection => ({
      pos: this.#morphology.position(member.property.posId),
      type: member.property.type,
      negative: member.property.negative,
      formal: member.property.formal,
      ordinal: fallback.ordinal
    });
    const semanticStageKey = (stageAliases: readonly number[]): string =>
      `${value.rootSeq}:${stageAliases.join(',')}`;
    const finalStageKey = semanticStageKey(aliases);
    const prefixStageKey = aliases.length === 2
      ? semanticStageKey([aliases[0]])
      : null;
    const prefixCollision = value.ruleIds.length === 2 && value.intermediate !== null
      ? this.#support.collision(
          value.rootSeq, value.route, value.intermediate, [value.ruleIds[0]]
        )
      : null;
    const viaSeq = prefixCollision?.collisionSeq ?? null;
    const semanticMembers: CandidateSemanticMember[] = [];
    const finalMembers = collision === null ? generated?.members ?? null : null;
    if (finalMembers && finalMembers.length > 0) {
      for (const finalMember of finalMembers) {
        const finalProperty = exactProperty(finalMember, fallbackInflection.at(-1)!);
        if (fallbackInflection.length === 1) {
          semanticMembers.push({
            entryIndex: entryIndex >= 0 ? entryIndex : null,
            root,
            inflection: [finalProperty],
            publicSeq: value.rootSeq,
            physicalGroup: generated?.physicalGroup ?? null,
            memberOrd: finalMember.memberOrd,
            targetNKanji,
            targetNKana,
            viaSeq: null,
            stageGroups: [generated?.physicalGroup ?? null],
            stageKeys: [finalStageKey],
            stageMemberOrds: [finalMember.memberOrd],
            stagePropOrds: [finalMember.propOrd]
          });
          continue;
        }

        const prefixMembers = (generatedVia?.members ?? []).filter(prefix =>
          prefix.memberOrd === finalMember.viaMemberOrd);
        if (prefixMembers.length === 0) {
          semanticMembers.push({
            entryIndex: entryIndex >= 0 ? entryIndex : null,
            root,
            inflection: [fallbackInflection[0]!, finalProperty],
            publicSeq: value.rootSeq,
            physicalGroup: generated?.physicalGroup ?? null,
            memberOrd: finalMember.memberOrd,
            targetNKanji,
            targetNKana,
            viaSeq,
            stageGroups: [
              generatedVia?.physicalGroup ?? null,
              generated?.physicalGroup ?? null
            ],
            stageKeys: [prefixStageKey, finalStageKey],
            stageMemberOrds: [finalMember.viaMemberOrd, finalMember.memberOrd],
            stagePropOrds: [null, finalMember.propOrd]
          });
          continue;
        }
        for (const prefixMember of prefixMembers) {
          semanticMembers.push({
            entryIndex: entryIndex >= 0 ? entryIndex : null,
            root,
            inflection: [
              exactProperty(prefixMember, fallbackInflection[0]!),
              finalProperty
            ],
            publicSeq: value.rootSeq,
            physicalGroup: generated?.physicalGroup ?? null,
            memberOrd: finalMember.memberOrd,
            targetNKanji,
            targetNKana,
            viaSeq,
            stageGroups: [
              generatedVia?.physicalGroup ?? null,
              generated?.physicalGroup ?? null
            ],
            stageKeys: [prefixStageKey, finalStageKey],
            stageMemberOrds: [prefixMember.memberOrd, finalMember.memberOrd],
            stagePropOrds: [prefixMember.propOrd, finalMember.propOrd]
          });
        }
      }
    } else if (
      fallbackInflection.length === 2
      && generatedVia?.members
      && generatedVia.members.length > 0
    ) {
      // The final row can use the semantic default and therefore need no
      // overlay while its one physical prefix member still owns overridden or
      // multiple conj_prop rows. Preserve every exact prefix property; a final
      // overlay is present whenever several prefix members require an explicit
      // via-member binding.
      for (const prefixMember of generatedVia.members) {
        semanticMembers.push({
          entryIndex: entryIndex >= 0 ? entryIndex : null,
          root,
          inflection: [
            exactProperty(prefixMember, fallbackInflection[0]!),
            fallbackInflection[1]!
          ],
          publicSeq: value.rootSeq,
          physicalGroup: generated?.physicalGroup ?? null,
          memberOrd: null,
          targetNKanji,
          targetNKana,
          viaSeq,
          stageGroups: [
            generatedVia.physicalGroup,
            generated?.physicalGroup ?? null
          ],
          stageKeys: [prefixStageKey, finalStageKey],
          stageMemberOrds: [prefixMember.memberOrd, 0],
          stagePropOrds: [prefixMember.propOrd, 0]
        });
      }
    } else {
      semanticMembers.push({
        entryIndex: entryIndex >= 0 ? entryIndex : null,
        root,
        inflection: fallbackInflection,
        publicSeq: value.rootSeq,
        physicalGroup: generated?.physicalGroup ?? null,
        memberOrd: null,
        targetNKanji,
        targetNKana,
        viaSeq: fallbackInflection.length === 2 ? viaSeq : null,
        stageGroups: fallbackInflection.length === 1
          ? [generated?.physicalGroup ?? null]
          : [generatedVia?.physicalGroup ?? null, generated?.physicalGroup ?? null],
        stageKeys: fallbackInflection.length === 1
          ? [finalStageKey]
          : [prefixStageKey, finalStageKey],
        stageMemberOrds: fallbackInflection.map(() => 0),
        stagePropOrds: fallbackInflection.map(() => 0)
      });
    }
    const inflection = semanticMembers[0]?.inflection ?? fallbackInflection;
    const conjugations = semanticMembers.map(member => analyzerConjugation(
      physicalSeq,
      value.rootSeq,
      member.inflection.at(-1)!,
      member.inflection.length > 1
    ));
    // Core asks the materialized reading first, then walks its conjugation
    // ancestors. A lexical collision therefore keeps a split registered on
    // the physical target while still falling back to its semantic root.
    const split = collision
      ? this.#scoreSplit(collision.collisionSeq, value.route, value.surface)
        ?? this.#scoreSplit(value.rootSeq, value.route, value.surface)
      : this.#scoreSplit(value.rootSeq, value.route, value.surface);
    const word: AnalyzerWordScoreFacts = {
      kind: 'word',
      text: value.surface,
      trueText: value.surface,
      trueTextFollowsText: true,
      route: value.route,
      seq: physicalSeq,
      ord: value.ord,
      common: null,
      nokanji: sourceForm === null
        ? value.route === 'kana' && value.sourceForm === value.sourceReading
        : this.#roots.formNokanji(sourceForm),
      entry: {
        root: collision !== null,
        // Pinned generated entries carry the number of materialized target
        // forms; root cardinality is the common case and the compact support
        // overlay replaces exceptions.
        nKanji: targetNKanji ?? 0,
        primaryNokanji: collision?.primaryNokanji ?? false
      },
      conjugationOnly: true,
      conjugations,
      positions: union(
        rootPos,
        collision?.pos ?? [],
        semanticMembers.flatMap(member => member.inflection.map(property => property.pos))
      ),
      self,
      lineage,
      inheritedCommon: value.common,
      inheritedOrd: value.ord,
      split,
      suruBreak: null
    };
    let reading = value.reading;
    reading = this.#annotations.hint(
      collision?.collisionSeq ?? value.rootSeq,
      value.route,
      value.surface,
      reading
    ) ?? reading;
    return withKey({
      kind: 'simple', text: value.surface, trueText: value.surface, route: value.route,
      reading, publicSeq: value.rootSeq, physicalSeq,
      physicalKey: collision
        ? `seq:${collision.collisionSeq}`
        : `semantic:${value.rootSeq}:${aliases.join(',')}`,
      physicalGroup: generated?.physicalGroup ?? null,
      lookupLocators: [collision
        ? { rootSeq: collision.collisionSeq, aliases: null }
        : { rootSeq: value.rootSeq, aliases }],
      memberOrd: semanticMembers[0]?.memberOrd ?? null,
      entryIndex: entryIndex >= 0 ? entryIndex : null, root, inflection,
      scoreFacts: word, components: [], counter: null,
      suffixClass: this.#support.suffixClass(collision?.collisionSeq ?? value.rootSeq),
      definitionSeq: collision?.collisionSeq ?? value.rootSeq,
      conjugationSelection: 'default',
      semanticMembers
    });
  }

  #rootForm(
    surface: string,
    seq: number,
    route: AnalyzerSupportRoute
  ): number | null {
    const key = `${route}\u0000${seq}\u0000${surface}`;
    const cached = this.#rootFormCache.get(key);
    if (cached !== undefined) return cached;
    const match = this.#surface.lookup(surface);
    if (!match?.direct || match.directRank === null) {
      this.#rootFormCache.set(key, null);
      return null;
    }
    const first = this.#roots.surfaceFormStart(match.directRank);
    const count = this.#roots.surfaceFormCount(match.directRank);
    for (let offset = 0; offset < count; offset++) {
      const form = first + offset;
      const entry = this.#roots.formEntryIndex(form);
      if (this.#roots.entrySeq(entry) === seq && this.#roots.formRoute(form) === route) {
        this.#rootFormCache.set(key, form);
        return form;
      }
    }
    this.#rootFormCache.set(key, null);
    return null;
  }

  #katakanaProxy(
    surface: string,
    existing: readonly MaterializedCandidate[]
  ): MaterializedCandidate[] {
    const hiragana = asHiragana(surface);
    if (hiragana === surface) return [];
    const excluded = new Set(existing.map(value => value.publicSeq));
    return this.#lexical(hiragana)
      .filter(value => value.kind === 'simple'
        && value.inflection.length === 0
        && !excluded.has(value.publicSeq))
      .map(source => {
        const base = source.scoreFacts as AnalyzerWordScoreFacts;
        return withKey({
          ...source,
          kind: 'proxy' as const,
          text: surface,
          reading: surface,
          scoreFacts: {
            ...base,
            text: surface,
            trueText: base.trueText,
            trueTextFollowsText: false
          },
          components: []
        });
      });
  }

  #scoreSplit(
    definitionSeq: number,
    route: AnalyzerSupportRoute,
    surface: string
  ): AnalyzerWordScoreFacts['split'] {
    const key = `${definitionSeq}\u0000${route}\u0000${surface}`;
    if (this.#scoreSplitCache.has(key)) return this.#scoreSplitCache.get(key)!;
    // Registered split definitions can refer back to their own lexical row.
    // Core reaches that row through an already-populated word cache; the
    // portable lookup needs an explicit cycle boundary while materializing.
    if (this.#scoreSplitInProgress.has(key)) return null;
    this.#scoreSplitInProgress.add(key);
    try {
      const split = this.#annotations.split(definitionSeq, route, surface, 'split');
      let result: AnalyzerWordScoreFacts['split'] = null;
      if (split) {
        if (split.parts.includes(':score')) result = { kind: 'add', score: split.score };
        else if (split.parts.includes(':pscore')) {
          result = { kind: 'proportional', score: split.score };
        } else {
          const parts = split.parts
            .map(value => this.#splitPart(value))
            .filter((value): value is MaterializedCandidate => value !== null);
          if (parts.length > 0 && parts.length === split.parts.length) {
            result = {
              kind: 'parts',
              score: split.score,
              parts: parts.map(value => value.scoreFacts)
            };
          }
        }
      }
      this.#scoreSplitCache.set(key, result);
      return result;
    } finally {
      this.#scoreSplitInProgress.delete(key);
    }
  }

  #splitPart(part: AnalyzerSupportSplitPart): MaterializedCandidate | null {
    if (typeof part === 'string') return null;
    const values = this.#lexical(part.text).filter(value => value.route === part.route);
    const exact = values.find(value =>
      value.publicSeq === part.seq || value.physicalSeq === part.seq);
    if (exact) return exact;
    if (!part.generated || part.generated.length === 0) return null;

    const signature = (rows: readonly {
      readonly from: number;
      readonly via: boolean;
      readonly pos: string;
      readonly type: number;
      readonly negative: boolean | null;
      readonly formal: boolean | null;
    }[]): string => JSON.stringify([...new Set(rows.map(value => JSON.stringify([
      value.from,
      value.via,
      value.pos,
      value.type,
      value.negative,
      value.formal
    ])))].sort());
    const wanted = signature(part.generated);
    const candidate = values.find(value => signature(value.semanticMembers.flatMap(member => {
      const property = member.inflection.at(-1);
      const from = member.root?.seq ?? member.publicSeq;
      if (!property || from === null) return [];
      return [{
        from,
        via: member.inflection.length > 1,
        pos: property.pos,
        type: property.type,
        negative: property.negative,
        formal: property.formal
      }];
    })) === wanted);
    return candidate ?? null;
  }

  #segmentSplit(candidate: MaterializedCandidate): {
    candidate: MaterializedCandidate;
    addedScore: number;
  } | null {
    if (candidate.kind !== 'simple' || candidate.definitionSeq === null) return null;
    const split = this.#annotations.split(
      candidate.definitionSeq, candidate.route, candidate.trueText, 'segsplit'
    );
    if (!split || split.parts.some(value => typeof value === 'string')) return null;
    const parts = split.parts.map(value => this.#splitPart(value));
    if (parts.some(value => value === null)) return null;
    const values = parts as MaterializedCandidate[];
    const primary = values[split.primary] ?? values[0];
    if (!primary) return null;
    const components = values.map(value => ({
      text: value.text,
      trueText: value.trueText === value.text ? null : value.trueText,
      route: value.route,
      reading: value.reading,
      entryIndex: value.entryIndex,
      root: value.root,
      inflection: value.inflection,
      primary: value.physicalKey === primary.physicalKey,
      publicSeq: value.publicSeq,
      physicalKey: value.physicalKey,
      physicalGroup: value.physicalGroup,
      suffixClass: value.suffixClass,
      definitionSeq: value.definitionSeq,
      semanticMembers: value.semanticMembers,
      conjugationSelection: value.conjugationSelection
    }));
    const compound = withKey({
      kind: 'compound' as const,
      text: values.map(value => value.text).join(''),
      trueText: candidate.trueText,
      route: candidate.route,
      reading: values.map(value => value.reading).join(split.connector),
      publicSeq: primary.publicSeq,
      physicalSeq: primary.physicalSeq,
      physicalKey: primary.physicalKey,
      physicalGroup: primary.physicalGroup,
      lookupLocators: [],
      memberOrd: primary.memberOrd,
      entryIndex: primary.entryIndex,
      root: primary.root,
      inflection: values.at(-1)?.inflection ?? [],
      scoreFacts: primary.scoreFacts,
      components,
      counter: null,
      suffixClass: values.at(-1)?.suffixClass ?? null,
      definitionSeq: candidate.definitionSeq,
      conjugationSelection: 'default',
      semanticMembers: primary.semanticMembers
    });
    return { candidate: compound, addedScore: split.score };
  }

  #suffixes(
    surface: string,
    directMatches: readonly MaterializedCandidate[],
    depth = 0
  ): MaterializedCandidate[] {
    const cached = directMatches.length === 0 ? this.#fullCache.get(surface) : undefined;
    if (cached) return [...cached];
    const result: MaterializedCandidate[] = [];
    for (const match of this.#support.suffixMatchesEndingAt(surface, surface.length, surface.length - 1)) {
      if (match.start <= 0) continue;
      const root = surface.slice(0, match.start);
      for (const value of match.values) {
        const suffixClass = value.form
          ? this.#support.suffixClass(value.form.seq) ?? value.keyword
          : value.keyword;
        if (directMatches.length > 0 && this.#uniqueSuffix(suffixClass, directMatches)) continue;
        result.push(...this.#applySuffix(
          value.keyword, root, match.text, value.form, surface, depth
        ));
      }
    }
    const deduped = this.#dedupe(result);
    if (directMatches.length === 0) this.#fullCache.set(surface, deduped);
    return [...deduped];
  }

  #uniqueSuffix(
    suffixClass: string,
    matches: readonly MaterializedCandidate[]
  ): boolean {
    if (UNIQUE_SUFFIXES.has(suffixClass)) return true;
    if (suffixClass === ':sa') {
      return matches.some(value => {
        const facts = value.scoreFacts;
        return facts.kind !== 'compound' && facts.entry?.root === true && !facts.conjugationOnly;
      });
    }
    if (suffixClass === ':desu') {
      return !matches.every(value => this.#conjugations(value)
        .some(conjugation => conjugation.from === 2_755_350));
    }
    return false;
  }

  #conjugations(candidate: MaterializedCandidate): readonly AnalyzerConjugation[] {
    return candidate.scoreFacts.conjugations;
  }

  /**
   * Core resolves the first `:suru` suffix only when a v5s/vs-s candidate is
   * scored across a kanji boundary. Keep that lookup off the normal path and
   * reproduce `getSuffixes`' shortest-to-longest order here.
   */
  #withSuruBreak(candidate: AnalyzerScoreCandidate): AnalyzerScoreCandidate {
    if (
      candidate.suruBreak !== null
      || candidate.kind !== 'word'
      || (!candidate.positions.includes('vs-s') && !candidate.positions.includes('v5s'))
    ) {
      return candidate;
    }
    const matches = this.#support.suffixMatchesEndingAt(
      candidate.text,
      candidate.text.length,
      Math.max(0, candidate.text.length - 1)
    );
    for (let index = matches.length - 1; index >= 0; index--) {
      const match = matches[index]!;
      for (const value of match.values) {
        if (value.keyword !== ':suru' || value.form === null) continue;
        const suffix = this.#suffixComponent(value.form);
        if (suffix === null) return candidate;
        return {
          ...candidate,
          suruBreak: { suffixText: match.text, candidate: suffix.scoreFacts }
        };
      }
    }
    return candidate;
  }

  #full(surface: string, depth: number): MaterializedCandidate[] {
    const direct = this.#lexical(surface);
    const suffixes = this.#suffixes(surface, direct, depth);
    return this.#dedupe([...direct, ...suffixes]);
  }

  #withTypes(surface: string, types: readonly number[], depth: number): MaterializedCandidate[] {
    return this.#full(surface, depth)
      .map(value => this.#selectConjugations(
        value,
        conjugation => types.includes(conjugation.property.type),
        false
      ))
      .filter((value): value is MaterializedCandidate => value !== null);
  }

  #withProperty(
    surface: string,
    predicate: (value: AnalyzerConjugation) => boolean,
    allowRoot: boolean,
    depth: number
  ): MaterializedCandidate[] {
    return this.#full(surface, depth)
      .map(value => this.#selectConjugations(value, predicate, allowRoot))
      .filter((value): value is MaterializedCandidate => value !== null);
  }

  /**
   * `findWordWithConjProp` selects physical conjugation rows, not individual
   * semantic properties: selecting one property keeps every property attached
   * to that same legacy `conjugation.id`. Compounds and proxies intentionally
   * ignore the outer clone's selector because core delegates their conjugation
   * data to the final component/source.
   */
  #selectConjugations(
    candidate: MaterializedCandidate,
    predicate: (value: AnalyzerConjugation) => boolean,
    allowRoot: boolean
  ): MaterializedCandidate | null {
    const current = this.#conjugations(candidate);
    if (current.length === 0) return allowRoot ? candidate : null;
    if (!current.some(predicate)) return null;
    if (candidate.kind !== 'simple' || candidate.scoreFacts.kind === 'compound') {
      return candidate;
    }
    const wordFacts = candidate.scoreFacts;

    const rowKey = (member: CandidateSemanticMember): string => {
      const group = member.stageGroups.at(-1) ?? null;
      const ordinal = member.stageMemberOrds.at(-1) ?? null;
      return group !== null && ordinal !== null
        ? `${group}:${ordinal}`
        : JSON.stringify([member.publicSeq, member.inflection]);
    };
    const conjugationFor = (member: CandidateSemanticMember): AnalyzerConjugation | null => {
      const property = member.inflection.at(-1);
      const from = member.root?.seq ?? member.publicSeq;
      if (!property || from === null) return null;
      return {
        seq: wordFacts.seq ?? -from,
        from,
        via: member.inflection.length > 1 ? -from : null,
        property: {
          pos: property.pos,
          type: property.type,
          negative: property.negative,
          formal: property.formal
        }
      };
    };
    const selectedRows = new Set(candidate.semanticMembers.flatMap(member => {
      const conjugation = conjugationFor(member);
      return conjugation && predicate(conjugation) ? [rowKey(member)] : [];
    }));
    if (selectedRows.size === 0) return candidate;
    const semanticMembers = candidate.semanticMembers.filter(member =>
      selectedRows.has(rowKey(member)));
    const seenConjugations = new Set<string>();
    const conjugations = semanticMembers.flatMap(member => {
      const conjugation = conjugationFor(member);
      if (!conjugation) return [];
      const key = JSON.stringify([
        rowKey(member), conjugation.from, conjugation.via, conjugation.property
      ]);
      if (seenConjugations.has(key)) return [];
      seenConjugations.add(key);
      return [conjugation];
    });
    if (conjugations.length === 0) return candidate;

    const selected = semanticMembers[0]!;
    // Core clones a lexical target and attaches the selected conjugation ids;
    // it does not replace that target's public identity with the semantic
    // source row. Generated-only candidates still expose the selected source.
    const preserveDirectTarget = candidate.inflection.length === 0;
    const targetEntryValue = candidate.physicalSeq !== null && candidate.physicalSeq > 0
      ? this.#roots.findEntryIndex(candidate.physicalSeq)
      : -1;
    const targetEntry = targetEntryValue >= 0 ? targetEntryValue : null;
    const facts = wordFacts;
    const secondaryOnly = conjugations.every(value => value.via !== null);
    const scoringMembers = semanticMembers.filter(member => secondaryOnly
      ? member.inflection.length > 1
      : member.inflection.length === 1);
    const lineageFacts = this.#memberSequenceFacts(candidate.semanticMembers, scoringMembers);
    const targetFacts = facts.self;
    return withKey({
      ...candidate,
      publicSeq: preserveDirectTarget ? candidate.publicSeq : selected.publicSeq,
      entryIndex: preserveDirectTarget ? candidate.entryIndex : selected.entryIndex,
      root: preserveDirectTarget ? candidate.root : selected.root,
      inflection: preserveDirectTarget ? candidate.inflection : selected.inflection,
      memberOrd: preserveDirectTarget ? candidate.memberOrd : selected.memberOrd,
      scoreFacts: {
        ...facts,
        conjugationOnly: true,
        conjugations,
        positions: union(
          positions(this.#roots, targetEntry),
          ...semanticMembers.map(member => positions(this.#roots, member.entryIndex)),
          conjugations.map(value => value.property.pos)
        ),
        lineage: {
          allArchived: targetFacts.allArchived
            && lineageFacts.length > 0
            && lineageFacts.every(value => value.allArchived),
          preferKana: targetFacts.preferKana
            || lineageFacts.some(value => value.preferKana),
          preferKanaOnOrdinalZero: targetFacts.preferKanaOnOrdinalZero
            || lineageFacts.some(value => value.preferKanaOnOrdinalZero)
        },
        entry: facts.entry === null ? null : {
          ...facts.entry,
          nKanji: preserveDirectTarget
            ? facts.entry.nKanji
            : selected.targetNKanji ?? facts.entry.nKanji
        }
      },
      semanticMembers,
      identityRoots: preserveDirectTarget && candidate.publicSeq !== null
        ? [candidate.publicSeq]
        : candidate.identityRoots,
      conjugationSelection: 'explicit'
    });
  }

  #withPos(surface: string, wanted: readonly string[], depth: number): MaterializedCandidate[] {
    void depth;
    return this.#lexical(surface).filter(value => {
      const facts = value.scoreFacts;
      return value.kind === 'simple'
        && facts.kind !== 'compound'
        && facts.entry?.root === true
        && !facts.conjugationOnly
        && wanted.some(pos => facts.positions.includes(pos));
    });
  }

  #withSeq(surface: string, seqs: readonly number[]): MaterializedCandidate[] {
    return this.#lexical(surface).filter(value => {
      const facts = value.scoreFacts;
      return value.kind === 'simple'
        && facts.kind !== 'compound'
        && facts.entry?.root === true
        && !facts.conjugationOnly
        && value.physicalSeq !== null
        && seqs.includes(value.physicalSeq);
    });
  }

  #conjOf(surface: string, seqs: readonly number[], depth: number): MaterializedCandidate[] {
    void depth;
    return this.#lexical(surface).filter(value =>
      (value.physicalSeq !== null && seqs.includes(value.physicalSeq))
      || this.#conjugations(value).some(conjugation => seqs.includes(conjugation.from)));
  }

  #applySuffix(
    keyword: string,
    root: string,
    suffix: string,
    form: AnalyzerSupportSuffixForm | null,
    surface: string,
    depth: number
  ): MaterializedCandidate[] {
    let stem = 0;
    let connector = '';
    let modifier = scoreModifier();
    let patch: readonly [string, string] | null = null;
    let primary: MaterializedCandidate[] = [];
    const next = depth + 1;
    const te = (): MaterializedCandidate[] => root !== 'で' && (root.endsWith('て') || root.endsWith('で'))
      ? this.#withTypes(root, [3], next) : [];
    const negative = (value: AnalyzerConjugation): boolean => value.property.negative !== false;

    switch (keyword) {
      case ':tai':
        if (root !== 'い') primary = this.#withTypes(root, [13], next);
        modifier = scoreModifier(5); break;
      case ':ren': primary = this.#withTypes(root, [13], next); modifier = scoreModifier(5); break;
      case ':ren-': primary = this.#withTypes(root, [13], next); break;
      case ':neg': primary = this.#withTypes(root, [13, 52], next); modifier = scoreModifier(5); break;
      case ':te': primary = te(); break;
      case ':teiru': if (root !== 'いて') primary = te(); modifier = scoreModifier(3); break;
      case ':teiru+': if (root !== 'いて') primary = te(); modifier = scoreModifier(6); break;
      case ':te+space': primary = te(); connector = ' '; modifier = scoreModifier(3); break;
      case ':kudasai': primary = te(); connector = ' '; modifier = scoreModifier(0, 360); break;
      case ':teren':
        if (root !== 'で') primary = root.endsWith('て') || root.endsWith('で')
          ? this.#withTypes(root, [3], next)
          : root !== 'い' ? this.#withTypes(root, [13], next) : [];
        modifier = scoreModifier(4); break;
      case ':teii':
        if (root.endsWith('て') || root.endsWith('で')) primary = this.#withTypes(root, [3], next);
        connector = ' '; modifier = scoreModifier(1); break;
      case ':chau': {
        stem = 1;
        const restored = suffix.startsWith('じ') ? 'で' : suffix.startsWith('ち') ? 'て' : null;
        if (restored) primary = this.#withTypes(root + restored, [3], next);
        modifier = scoreModifier(5); break;
      }
      case ':to': {
        stem = 1;
        const restored = suffix.startsWith('と') ? 'て' : suffix.startsWith('ど') ? 'で' : null;
        if (restored) primary = this.#withTypes(root + restored, [3], next);
        break;
      }
      case ':suru': primary = this.#withPos(root, ['vs'], next); connector = ' '; modifier = scoreModifier(5); break;
      case ':sou':
      case ':sou+': {
        if (root.endsWith('なさ')) {
          patch = ['い', 'さ'];
          primary = this.#withProperty(root.slice(0, -1) + 'い', negative, false, next);
        } else if (!['な', 'よ', 'よさ', 'に', 'き'].includes(root)) {
          primary = this.#withTypes(root, [13, 51, 50], next);
        }
        modifier = keyword === ':sou+'
          ? scoreModifier(1)
          : scoreModifier(0, root === 'から' ? 40 : root === 'い' ? 0 : root === '出来' ? 100 : 70);
        break;
      }
      case ':rou': primary = this.#withTypes(root, [2], next); modifier = scoreModifier(1); break;
      case ':adv': primary = this.#withTypes(root, [50], next); modifier = scoreModifier(1); break;
      case ':sugiru': {
        stem = 1;
        if (root !== 'い') {
          if (root.endsWith('なさ') || root.endsWith('無さ')) {
            patch = ['い', 'さ'];
            const modified = root.slice(0, -1) + 'い';
            primary = modified.length > 2
              ? this.#withProperty(modified, negative, false, next)
              : this.#withPos(modified, ['adj-i'], next);
          } else primary = this.#withPos(root + 'い', ['adj-i'], next);
        }
        modifier = scoreModifier(5); break;
      }
      case ':sa': primary = [
        ...this.#withTypes(root, [51], next), ...this.#withPos(root, ['adj-na'], next)
      ]; modifier = scoreModifier(2); break;
      case ':iadj': primary = this.#withTypes(root, [51], next); modifier = scoreModifier(1); break;
      case ':garu':
        if (!['な', 'い', 'よ'].includes(root)) {
          primary = this.#withTypes(root, [51], next);
          if (primary.length === 0 && root.endsWith('そ')) {
            patch = ['う', ''];
            primary = this.#full(root + 'う', next).filter(value =>
              value.kind === 'compound' && value.suffixClass === ':sou');
          }
        }
        break;
      case ':ra':
        if (!root.endsWith('ら')) {
          primary = this.#withPos(root, ['pn'], next);
          if (primary.length === 0) primary = this.#withPos(asHiragana(root), ['pn'], next);
          if (primary.length === 0) primary = this.#withSeq(root, [1_580_640]);
        }
        modifier = scoreModifier(1); break;
      case ':rashii': {
        const first = this.#withTypes(root, [2], next);
        const second = this.#withTypes(root + 'ら', [11], next);
        // pairWordsByConj groups equal lineage sets for presentation, then the
        // caller flattens every group; semantic candidates therefore retain
        // both paired and unpaired roots.
        primary = this.#dedupe([...first, ...second]);
        modifier = scoreModifier(3); break;
      }
      case ':desu':
        if (root.endsWith('ない') || root.endsWith('なかった')) primary = this.#withProperty(root, negative, false, next);
        connector = ' '; modifier = scoreModifier(0, 200); break;
      case ':desho':
        if (root.endsWith('ない')) primary = this.#withProperty(root, negative, false, next);
        connector = ' '; modifier = scoreModifier(0, 300); break;
      case ':tosuru': primary = this.#withTypes(root, [9], next); connector = ' '; modifier = scoreModifier(3); break;
      case ':kurai': primary = this.#withTypes(root, [2], next); connector = ' '; modifier = scoreModifier(3); break;

      case ':nai': primary = this.#withProperty(root + 'ない', value =>
        value.from !== 1_577_980 && value.from !== 1_547_720 && negative(value), true, next);
        return this.#abbreviations(primary, root, suffix, surface, 2, patch);
      case ':nai-x':
        if (root === 'せ') {
          patch = ['しない', 'せ'];
          primary = this.#conjOf('しない', [1_157_170], next);
        } else primary = this.#withProperty(root + 'ない', value =>
          value.from !== 1_157_170 && negative(value), false, next);
        return this.#abbreviations(primary, root, suffix, surface, 2, patch);
      case ':nai-n': primary = this.#withProperty(root + 'ない', value =>
        value.from !== 1_577_980 && value.from !== 1_547_720 && negative(value), false, next);
        return this.#abbreviations(primary, root, suffix, surface, 2, patch);
      case ':nakereba': primary = this.#full(root + 'なければ', next);
        return this.#abbreviations(primary, root, suffix, surface, 4, null);
      case ':shimashou': primary = this.#full(root + 'しましょう', next);
        return this.#abbreviations(primary, root, suffix, surface, 5, null);
      case ':dewanai': primary = this.#full(root + 'ではない', next);
        return this.#abbreviations(primary, root, suffix, surface, 4, null);
      case ':teba': case ':reba': case ':keba': case ':geba':
      case ':neba': case ':beba': case ':meba': case ':seba':
        primary = this.#full(root + CONDITIONAL_ABBREVIATIONS[keyword]!, next);
        return this.#abbreviations(primary, root, suffix, surface, 2, null);
      case ':ii': primary = this.#full(root + 'いい', next);
        return this.#abbreviations(primary, root, suffix, surface, 2, null);
      default: return [];
    }

    if (!form) return [];
    const suffixCandidate = this.#suffixComponent(form);
    if (!suffixCandidate) return [];
    return primary.map(value => this.#compound(
      value,
      suffixCandidate,
      suffix,
      surface,
      stem,
      connector,
      modifier,
      patch,
      keyword === ':suru'
    ));
  }

  #suffixComponent(form: AnalyzerSupportSuffixForm): MaterializedCandidate | null {
    const values = this.#lexical(form.text);
    const conjugations = form.conjugations === ':root' || form.conjugations === null
      ? []
      : form.conjugations;
    const rootSeqs = new Set(conjugations.map(value => value.from));
    const selectedBy = (member: CandidateSemanticMember): boolean => {
      const property = member.inflection.at(-1);
      const rootSeq = member.root?.seq ?? member.publicSeq;
      if (!property || rootSeq === null) return false;
      return conjugations.some(conjugation =>
        conjugation.from === rootSeq
        && member.inflection.length === (conjugation.via === null ? 1 : 2)
        && conjugation.property.pos === property.pos
        && conjugation.property.type === property.type
        && conjugation.property.negative === property.negative
        && conjugation.property.formal === property.formal);
    };
    // A suffix-cache form can select one physical conjugation member even when
    // an earlier generated candidate has the same semantic root. Locate the
    // physical group containing that exact member before applying root/target
    // fallbacks; otherwise lookup order can substitute a different property.
    const explicitlySelected = conjugations.length > 0
      ? values.find(value => value.semanticMembers.some(selectedBy))
      : undefined;
    if (conjugations.length > 0 && !explicitlySelected) {
      throw new Error(`Explicit suffix member is unavailable for ${JSON.stringify(form.text)}`);
    }
    let candidate = explicitlySelected;
    candidate ??= values.find(value => value.physicalSeq === form.seq)
      ?? values.find(value => value.publicSeq !== null && rootSeqs.has(value.publicSeq))
      ?? values.find(value => value.publicSeq === form.seq)
      ?? values[0];
    if (!candidate) return null;
    if (candidate.scoreFacts.kind === 'compound') return candidate;

    const selection: MaterializedCandidate['conjugationSelection'] =
      form.conjugations === ':root'
        ? 'root'
        : form.conjugations === null ? 'default' : 'explicit';
    const semanticMembers = selection === 'root'
      ? []
      : selection === 'default'
        ? candidate.semanticMembers
        : candidate.semanticMembers.filter(selectedBy);
    const selectedMember = semanticMembers[0] ?? null;
    const rootSeq = selectedMember?.root?.seq
      ?? selectedMember?.publicSeq
      ?? conjugations[0]?.from
      ?? candidate.publicSeq
      ?? form.seq;
    const rootEntryValue = selectedMember?.entryIndex
      ?? this.#roots.findEntryIndex(rootSeq);
    const rootEntry = rootEntryValue >= 0 ? rootEntryValue : null;
    const targetEntryValue = this.#roots.findEntryIndex(form.seq);
    const targetEntry = targetEntryValue >= 0 ? targetEntryValue : null;
    const lexicalTarget = targetEntry === null ? null : values.find(value =>
      value.publicSeq === form.seq
      && value.entryIndex === targetEntry
      && value.inflection.length === 0) ?? null;
    const rootFacts = sequenceFacts(this.#roots, rootEntry);
    const self = sequenceFacts(this.#roots, targetEntry);
    const lineage = targetEntry === null ? rootFacts : {
      allArchived: self.allArchived && rootFacts.allArchived,
      preferKana: self.preferKana || rootFacts.preferKana,
      preferKanaOnOrdinalZero: self.preferKanaOnOrdinalZero || rootFacts.preferKanaOnOrdinalZero
    };
    const scoreFacts: AnalyzerWordScoreFacts = {
      kind: 'word',
      text: form.text,
      trueText: form.text,
      trueTextFollowsText: true,
      route: 'kana',
      seq: form.conjugations === ':root' ? rootSeq : form.seq,
      ord: form.ord,
      common: form.conjugations === ':root' ? form.common : null,
      nokanji: form.nokanji,
      entry: targetEntry === null ? {
        root: false, nKanji: 0, primaryNokanji: false
      } : {
        root: true,
        nKanji: this.#roots.entryNKanji(targetEntry),
        primaryNokanji: this.#roots.entryPrimaryNokanji(targetEntry)
      },
      conjugationOnly: form.conjugations !== ':root' && conjugations.length > 0,
      conjugations,
      positions: union(
        positions(this.#roots, rootEntry),
        positions(this.#roots, targetEntry),
        conjugations.map(value => value.property.pos)
      ),
      self,
      lineage,
      inheritedCommon: candidate.scoreFacts.inheritedCommon,
      inheritedOrd: candidate.scoreFacts.inheritedOrd,
      split: this.#scoreSplit(form.seq, 'kana', form.text),
      suruBreak: null
    };
    return withKey({
      ...candidate,
      kind: 'simple',
      text: form.text,
      trueText: form.text,
      route: 'kana',
      reading: form.text,
      publicSeq: lexicalTarget?.publicSeq ?? rootSeq,
      physicalSeq: form.conjugations === ':root' ? rootSeq : form.seq,
      physicalKey: `seq:${form.conjugations === ':root' ? rootSeq : form.seq}`,
      physicalGroup: candidate.physicalGroup,
      memberOrd: selectedMember?.memberOrd ?? candidate.memberOrd,
      entryIndex: lexicalTarget?.entryIndex ?? rootEntry,
      root: lexicalTarget?.root ?? selectedMember?.root
        ?? (candidate.root?.seq === rootSeq ? candidate.root : {
          seq: rootSeq,
          form: candidate.root?.form ?? form.bestKanji ?? form.text,
          reading: candidate.root?.reading ?? form.text
        }),
      scoreFacts,
      inflection: lexicalTarget ? [] : selectedMember?.inflection
        ?? (candidate.root?.seq === rootSeq ? candidate.inflection : []),
      suffixClass: this.#support.suffixClass(form.seq),
      definitionSeq: form.seq,
      semanticMembers,
      identityRoots: lexicalTarget ? [form.seq] : candidate.identityRoots,
      conjugationSelection: selection
    });
  }

  #compound(
    primary: MaterializedCandidate,
    suffix: MaterializedCandidate,
    suffixText: string,
    surface: string,
    stem: number,
    connector: string,
    modifier: AnalyzerScoreModifier,
    patch: readonly [string, string] | null,
    suruBreak: boolean
  ): MaterializedCandidate {
    let reading = primary.reading;
    reading = patch
      ? reading.slice(0, Math.max(0, reading.length - patch[0].length)) + patch[1]
      : reading.slice(0, Math.max(0, reading.length - stem));
    reading += connector + suffixText;
    const primaryComponents = primary.kind === 'compound'
      ? primary.components.map(component => ({
          ...component,
          primary: component.physicalKey === primary.physicalKey
        }))
      : [{
          text: primary.text,
          trueText: primary.trueText === primary.text ? null : primary.trueText,
          route: primary.route,
          reading: primary.reading,
          entryIndex: primary.entryIndex,
          root: primary.root,
          inflection: primary.inflection,
          primary: true,
          publicSeq: primary.publicSeq,
          physicalKey: primary.physicalKey,
          physicalGroup: primary.physicalGroup,
          suffixClass: primary.suffixClass,
          definitionSeq: primary.definitionSeq,
          semanticMembers: primary.semanticMembers,
          identityRoots: primary.identityRoots,
          conjugationSelection: primary.conjugationSelection
        }];
    const components = [
      ...primaryComponents,
      {
        text: suffix.text,
        trueText: suffix.trueText === suffix.text ? null : suffix.trueText,
        route: suffix.route,
        reading: suffix.reading,
        entryIndex: suffix.entryIndex,
        root: suffix.root,
        inflection: suffix.inflection,
        primary: suffix.physicalKey === primary.physicalKey,
        publicSeq: suffix.publicSeq,
        physicalKey: suffix.physicalKey,
        physicalGroup: suffix.physicalGroup,
        suffixClass: suffix.suffixClass,
        definitionSeq: suffix.definitionSeq,
        semanticMembers: suffix.semanticMembers,
        identityRoots: suffix.identityRoots,
        conjugationSelection: suffix.conjugationSelection
      }
    ];
    // `adjoinWord` extends a compound in place: it keeps the original simple
    // score base and accumulates score modifiers. Nesting score candidates
    // would lose the outer use-length bonus because compound scoring ignores a
    // caller's `useLength` in favor of its own text.
    const previousCompound = primary.scoreFacts.kind === 'compound'
      ? primary.scoreFacts
      : null;
    const base = previousCompound?.base ?? primary.scoreFacts;
    const combinedModifier = previousCompound === null ? modifier : {
      multiplier: previousCompound.modifier.multiplier + modifier.multiplier,
      constant: previousCompound.modifier.constant + modifier.constant
    };
    const scoreFacts: AnalyzerScoreCandidate = {
      kind: 'compound',
      text: surface,
      base,
      modifier: combinedModifier,
      conjugations: suffix.scoreFacts.conjugations,
      suruBreak: suruBreak
        ? { suffixText, candidate: suffix.scoreFacts }
        : previousCompound?.suruBreak ?? null
    };
    return withKey({
      kind: 'compound', text: surface, trueText: surface,
      route: testWord(surface, 'kana') ? 'kana' : 'kanji', reading,
      publicSeq: primary.publicSeq, physicalSeq: primary.physicalSeq,
      physicalKey: primary.physicalKey,
      physicalGroup: primary.physicalGroup,
      lookupLocators: [],
      memberOrd: primary.memberOrd,
      entryIndex: primary.entryIndex, root: primary.root,
      inflection: suffix.inflection, scoreFacts, components, counter: null,
      suffixClass: this.#support.suffixClass(suffix.physicalSeq ?? suffix.publicSeq ?? 0),
      definitionSeq: primary.definitionSeq,
      conjugationSelection: 'default',
      semanticMembers: primary.semanticMembers,
      identityRoots: primary.identityRoots
    });
  }

  #abbreviations(
    primary: readonly MaterializedCandidate[],
    root: string,
    suffix: string,
    surface: string,
    stem: number,
    patch: readonly [string, string] | null
  ): MaterializedCandidate[] {
    return primary.map(value => {
      let reading = value.reading;
      reading = patch
        ? reading.slice(0, Math.max(0, reading.length - patch[0].length)) + patch[1]
        : reading.slice(0, Math.max(0, reading.length - stem));
      reading += suffix;
      if (value.kind === 'compound') {
        return withKey({
          ...value,
          text: surface,
          trueText: surface,
          reading,
          scoreFacts: { ...value.scoreFacts, text: surface }
        });
      }
      const facts = value.scoreFacts as AnalyzerWordScoreFacts;
      return withKey({
        ...value,
        kind: 'proxy',
        text: root + suffix,
        reading,
        scoreFacts: {
          ...facts,
          text: root + suffix,
          trueText: facts.trueText,
          trueTextFollowsText: false
        }
      });
    });
  }

  #counters(
    numberText: string,
    counterText: string,
    unique: boolean,
    variants: readonly AnalyzerSupportCounterVariant[] = this.#support.counters(counterText)
  ): MaterializedCandidate[] {
    const result: MaterializedCandidate[] = [];
    for (const variant of variants) {
      const rendered = materializeAnalyzerCounter(numberText, variant, unique);
      if (!rendered) continue;
      result.push(this.#counter(rendered, variant));
    }
    return this.#dedupe(result);
  }

  #counter(
    rendered: NonNullable<ReturnType<typeof materializeAnalyzerCounter>>,
    variant: AnalyzerSupportCounterVariant
  ): MaterializedCandidate {
    const sourceSeq = variant.source?.seq ?? null;
    const entryIndexValue = sourceSeq === null ? -1 : this.#roots.findEntryIndex(sourceSeq);
    const entryIndex = entryIndexValue >= 0 ? entryIndexValue : null;
    const facts = sequenceFacts(this.#roots, entryIndex);
    let common = variant.common;
    let ord = variant.source?.ord ?? 0;
    let nokanji = false;
    let root: PortableAnalysisRoot | null = null;
    if (variant.source && entryIndex !== null) {
      const direct = this.#lexical(variant.source.text)
        .find(value => value.publicSeq === sourceSeq && value.inflection.length === 0);
      common ??= direct?.scoreFacts.kind !== 'compound' ? direct?.scoreFacts.common ?? null : null;
      nokanji = direct?.scoreFacts.kind !== 'compound' ? direct?.scoreFacts.nokanji ?? false : false;
      root = direct?.root ?? {
        seq: variant.source.seq,
        form: variant.source.text,
        reading: variant.source.text
      };
      ord = variant.source.ord;
    }
    const word: AnalyzerWordScoreFacts = {
      kind: 'counter', text: rendered.text, trueText: rendered.text,
      trueTextFollowsText: true, route: rendered.route, seq: sourceSeq,
      ord, common, nokanji,
      entry: entryIndex === null ? null : {
        root: true,
        nKanji: this.#roots.entryNKanji(entryIndex),
        primaryNokanji: this.#roots.entryPrimaryNokanji(entryIndex)
      },
      conjugationOnly: false, conjugations: [], positions: ['ctr'],
      self: facts, lineage: facts, inheritedCommon: null, inheritedOrd: null,
      split: null, suruBreak: null
    };
    return withKey({
      kind: 'counter', text: rendered.text, trueText: rendered.text,
      route: rendered.route, reading: rendered.reading,
      publicSeq: sourceSeq, physicalSeq: sourceSeq,
      physicalKey: sourceSeq === null ? `counter:${rendered.text}` : `seq:${sourceSeq}`,
      physicalGroup: null, entryIndex, root,
      lookupLocators: [],
      memberOrd: null,
      inflection: [], scoreFacts: word, components: [],
      counter: [rendered.value, rendered.ordinal], suffixClass: null,
      definitionSeq: sourceSeq,
      conjugationSelection: 'default',
      semanticMembers: [{
        entryIndex,
        root,
        inflection: [],
        publicSeq: sourceSeq,
        physicalGroup: null,
        memberOrd: null,
        targetNKanji: entryIndex === null ? null : this.#roots.entryNKanji(entryIndex),
        targetNKana: entryIndex === null ? null : this.#roots.entryNKana(entryIndex),
        viaSeq: null,
        stageGroups: [],
        stageMemberOrds: [],
        stagePropOrds: []
      }]
    });
  }

  #dedupe(values: readonly MaterializedCandidate[]): MaterializedCandidate[] {
    const seen = new Set<string>();
    const result: MaterializedCandidate[] = [];
    for (const value of values) {
      if (seen.has(value.key)) continue;
      seen.add(value.key);
      result.push(value);
    }
    return result;
  }

  #tokens(
    text: string,
    parts: readonly ({ readonly segments: readonly AnalyzerSegment[] } | unknown)[],
    candidates: ReadonlyMap<number, ScoredCandidate>,
    entities: readonly AnalyzerEntityHint[]
  ): PortableAnalysisToken[] {
    const groups = parts.filter((part): part is AnalyzerSegmentGroup =>
      typeof part === 'object' && part !== null && 'segments' in part);
    const tokens: PortableAnalysisToken[] = [];
    let offset = 0;
    for (const group of groups) {
      if (group.start > offset) tokens.push(this.#gap(text, offset, group.start));
      const first = group.segments[0];
      if (!first) continue;
      if (first.entity) {
        tokens.push({
          candidateId: first.candidateId,
          start: group.start, end: group.end,
          text: text.slice(group.start, group.end), trueText: null,
          route: testWord(text.slice(group.start, group.end), 'kana') ? 'kana' : 'kanji',
          reading: text.slice(group.start, group.end),
          romanized: romanizeWord(text.slice(group.start, group.end)),
          pos: ['proper-noun'], score: first.score, entryIndex: null, root: null,
          inflection: [], components: [], alternatives: [], skipped: 0,
          entity: true, counter: null
        });
        offset = group.end;
        continue;
      }
      const retained = selectAnalyzerAlternatives(group.segments);
      // Katakana proxies come from a separate root-only lookup in core rather
      // than the substring cache used by ordinary words. Its stable tie order
      // selects the lowest dictionary sequence for the clean scalar candidate
      // (the legacy alternative bag below intentionally retains lookup order).
      const firstScored = candidates.get(first.candidateId);
      const primarySegment = firstScored?.candidate.kind === 'proxy'
        ? retained
          .filter(segment => segment.score === first.score)
          .map(segment => ({ segment, value: candidates.get(segment.candidateId) }))
          .filter((value): value is {
            segment: AnalyzerSegment;
            value: ScoredCandidate;
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
          components: value.candidate.components.map(component =>
            this.#publicComponent(component)),
          counter: value.candidate.counter
        };
        return [this.#recordPresentation(alternative, value.candidate)];
      });
      const token: PortableAnalysisToken = {
        candidateId: primarySegment.candidateId,
        start: group.start, end: group.end,
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
    if (offset < text.length) tokens.push(this.#gap(text, offset, text.length));
    this.#fixNani(tokens);
    return tokens;
  }

  #gap(text: string, start: number, end: number): PortableAnalysisToken {
    const value = text.slice(start, end);
    return {
      candidateId: null, start, end, text: value, trueText: null,
      route: 'gap', reading: value, romanized: romanizeWord(value), pos: [], score: 0,
      entryIndex: null, root: null, inflection: [], components: [], alternatives: [],
      skipped: 0, entity: false, counter: null
    };
  }

  #fixNani(tokens: PortableAnalysisToken[]): void {
    const nanClasses = new Set([
      'ba', 'bi', 'bu', 'be', 'bo', 'pa', 'pi', 'pu', 'pe', 'po',
      'da', 'dji', 'dzu', 'de', 'do', 'za', 'ji', 'zu', 'ze', 'zo',
      'ta', 'chi', 'tsu', 'te', 'to', 'na', 'nu', 'ne', 'no',
      'ra', 'ri', 'ru', 're', 'ro'
    ]);
    for (let index = 0; index + 1 < tokens.length; index++) {
      const token = tokens[index]!;
      const next = tokens[index + 1]!;
      if (token.text !== '何') continue;
      let nan = false;
      let nani = false;
      for (const value of [next.reading, ...next.alternatives.map(value => value.reading)]) {
        if (value.length === 0) continue;
        if (nanClasses.has(getCharClass(value[0]!))) nan = true;
        else nani = true;
      }
      const reading = nani ? 'なに' : nan ? 'なん' : null;
      if (!reading) continue;
      (token as { reading: string }).reading = reading;
      (token as { romanized: string }).romanized = romanizeWord(reading);
      const facts = this.#presentation.get(token);
      if (facts) this.#presentation.set(token, { ...facts, contextualReading: true });
    }
  }
}

export function openPortableAnalyzer(source: PortableAnalyzerSource): PortableAnalyzer {
  return new PortableAnalyzer(source);
}
