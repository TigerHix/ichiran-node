import {
  addAnalyzerEntityGroups,
  findAnalyzerPaths
} from './analyzer-paths.js';
import {
  filterAndCullAnalyzerSegments,
  scoreAnalyzerCandidate
} from './analyzer-scoring.js';
import type {
  AnalyzerEntityHint,
  AnalyzerSegment,
  AnalyzerSegmentGroup,
  AnalyzerWordScoreFacts
} from './analyzer-types.js';
import {
  ITERATION_CHARACTERS,
  KANA_CHARACTERS,
  MODIFIER_CHARACTERS,
  basicSplit,
  consecutiveCharGroups,
  getCharClass,
  longVowelModifierP,
  normalize,
  sequentialKanjiPositions
} from './characters.js';
import { MorphologyReader } from './morphology.js';
import { materializeAnalyzerCounter } from './analyzer-counters.js';
import {
  type AnalyzerSupportCounterVariant,
  type AnalyzerSupportSuffixMatch,
  AnalyzerSupportReader
} from './analyzer-support.js';
import { joinRomanizedParts, romanizeWord, type RomanizationName } from './romanization.js';
import { RootPayloadReader } from './root-payload.js';
import { SurfaceIndex } from './surface-index.js';
import {
  type PortableLegacySerializeOptions,
  type PortableLegacyTransformedResult
} from './analyzer-legacy.js';
import type { DictionaryReader } from './dictionary.js';
import {
  validatePortableAnalyzeRequest,
  type PortableAnalyzeOptions
} from './analyzer-options.js';
import { AnalyzerResultProjector } from './analyzer-result.js';
import type {
  PortableAnalysisChunk,
  PortableAnalysisPath,
  PortableAnalysisResult,
  PortableAnalysisRoot,
  PortableAnalysisToken
} from './analyzer-result.js';
import {
  dedupeCandidates,
  sequenceFacts,
  withKey,
  type MaterializedCandidate,
  type ScoredCandidate
} from './analyzer-candidate.js';
import { AnalyzerLexicon } from './analyzer-lexicon.js';
import { AnalyzerSuffixResolver } from './analyzer-suffixes.js';
import type {
  PortableAnalyzerAnnotations,
  PortableAnalyzerSource
} from './analyzer-source.js';

export {
  AnalyzerInputError,
  MAX_ANALYZER_ENTITIES,
  MAX_ANALYZER_ENTITY_ABS_BOOST,
  MAX_ANALYZER_LIMIT,
  MAX_ANALYZER_TEXT_LENGTH,
  MAX_ANALYZER_WORD_LENGTH,
  validateAnalyzerEntities,
  validateAnalyzerLimit,
  validatePortableAnalyzeRequest
} from './analyzer-options.js';
export type { PortableAnalyzeOptions } from './analyzer-options.js';
export type { PortableAnalyzerAnnotations, PortableAnalyzerSource } from './analyzer-source.js';
export type {
  PortableAnalysisAlternative,
  PortableAnalysisChunk,
  PortableAnalysisComponent,
  PortableAnalysisInflection,
  PortableAnalysisPath,
  PortableAnalysisResult,
  PortableAnalysisRoot,
  PortableAnalysisToken
} from './analyzer-result.js';

const FORCE_KANJI_BREAK = new Set(['です']);
const NO_KANJI_BREAK = new Set(['日置']);

interface AccumulatedTokenBatch {
  readonly previous: AccumulatedTokenBatch | null;
  readonly tokens: readonly PortableAnalysisToken[];
  readonly length: number;
}

interface AccumulatedAnalysisPath {
  readonly score: number;
  readonly tail: AccumulatedTokenBatch | null;
}

function appendTokenBatch(
  path: AccumulatedAnalysisPath,
  tokens: readonly PortableAnalysisToken[]
): AccumulatedAnalysisPath {
  if (tokens.length === 0) return path;
  return {
    score: path.score,
    tail: {
      previous: path.tail,
      tokens,
      length: (path.tail?.length ?? 0) + tokens.length
    }
  };
}

function materializeAccumulatedPath(path: AccumulatedAnalysisPath): PortableAnalysisPath {
  const tokens = new Array<PortableAnalysisToken>(path.tail?.length ?? 0);
  let end = tokens.length;
  for (let batch = path.tail; batch; batch = batch.previous) {
    end -= batch.tokens.length;
    for (let index = 0; index < batch.tokens.length; index++) {
      tokens[end + index] = batch.tokens[index]!;
    }
  }
  return { score: path.score, tokens };
}

function monotonicNow(): number {
  const clock = (globalThis as unknown as {
    readonly performance?: { now(): number };
  }).performance;
  return clock?.now() ?? Date.now();
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
  readonly #result: AnalyzerResultProjector;
  readonly #lexicon: AnalyzerLexicon;
  readonly #suffixes: AnalyzerSuffixResolver;

  constructor(source: PortableAnalyzerSource) {
    this.#surface = source.surface;
    this.#roots = source.roots;
    this.#morphology = source.morphology;
    this.#support = source.support;
    this.#annotations = source.annotations ?? source.support;
    const resolverSource = {
      surface: this.#surface,
      roots: this.#roots,
      morphology: this.#morphology,
      support: this.#support,
      annotations: this.#annotations
    };
    this.#lexicon = new AnalyzerLexicon(
      resolverSource,
      (definitionSeq, route, surface) =>
        this.#suffixes.scoreSplit(definitionSeq, route, surface)
    );
    this.#suffixes = new AnalyzerSuffixResolver(resolverSource, this.#lexicon);
    this.#result = new AnalyzerResultProjector({
      roots: this.#roots,
      support: this.#support,
      directSurface: rank => this.#surface.directSurface(rank),
      hint: (definitionSeq, route, surface, reading) =>
        this.#annotations.hint(definitionSeq, route, surface, reading)
    });
    if (this.#surface.manifest.directCount !== this.#roots.surfaceCount) {
      throw new Error(
        `Surface/root rank mismatch: ${this.#surface.manifest.directCount} != ${this.#roots.surfaceCount}`
      );
    }
  }

  analyze(input: string, options: PortableAnalyzeOptions = {}): PortableAnalysisResult {
    const started = monotonicNow();
    const validated = validatePortableAnalyzeRequest(input, options);
    // These caches deduplicate recursive suffix/split lookup within one request.
    // Keeping them across requests would retain decoded annotation objects and
    // eventually defeat the bounded missing-block preload/retry protocol.
    this.#lexicon.reset();
    this.#suffixes.reset();
    const normalized = normalize(
      validated.input,
      undefined,
      !validated.options.normalizePunctuation
    );
    const entities = validated.options.entities;
    const limit = validated.options.limit;
    const chunks: PortableAnalysisChunk[] = [];
    let paths: AccumulatedAnalysisPath[] = [{ score: 0, tail: null }];
    let offset = 0;

    for (const segment of basicSplit(normalized)) {
      const start = offset;
      const end = start + segment.text.length;
      if (segment.type === 'misc') {
        chunks.push({ type: 'misc', start, end, text: segment.text });
        const token = this.#result.gap(normalized, start, end);
        const tokens = [token];
        paths = paths.map(path => appendTokenBatch(path, tokens));
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
            tokens: path.tokens.map(token => this.#result.shiftToken(token, start))
          }));
        chunks.push({ type: 'word', start, end, text: segment.text, paths: localPaths });
        paths = this.#mergePaths(paths, localPaths, limit);
      }
      offset = end;
    }

    const materializedPaths = paths.map(materializeAccumulatedPath);
    return {
      input,
      normalized,
      computeMs: monotonicNow() - started,
      chunks,
      paths: materializedPaths
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
    return this.#result.serializeLegacy(result, options);
  }

  /** Cold detail hydration for transformed romanize* output. */
  serializeLegacyDetailed(
    result: PortableAnalysisResult,
    dictionary: DictionaryReader,
    options: PortableLegacySerializeOptions = {}
  ): Promise<PortableLegacyTransformedResult> {
    return this.#result.serializeLegacyDetailed(result, dictionary, options);
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
      tokens: this.#result.tokens(text, path.parts, candidates, entities)
    }));
  }

  #mergePaths(
    left: readonly AccumulatedAnalysisPath[],
    right: readonly PortableAnalysisPath[],
    limit: number
  ): AccumulatedAnalysisPath[] {
    const merged: AccumulatedAnalysisPath[] = [];
    for (const prefix of left) {
      for (const suffix of right) {
        merged.push(appendTokenBatch({
          score: prefix.score + suffix.score,
          tail: prefix.tail
        }, suffix.tokens));
      }
    }
    // Array.sort is stable; nested generation preserves each chunk's path tie order.
    merged.sort((a, b) => b.score - a.score);
    return merged.slice(0, limit);
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
        const values = this.#lexicon.lexical(surface, match);
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
          if (direct.length > 0 && this.#suffixes.uniqueSuffix(suffixClass, direct)) continue;
          suffixes.push(...this.#suffixes.applySuffix(
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
        byEnd.set(end, dedupeCandidates([...(byEnd.get(end) ?? []), ...suffixes]));
      }

      const katakanaEnd = katakanaEnds.get(start);
      if (katakanaEnd !== undefined && katakanaEnd <= maxEnd && !sticky.has(katakanaEnd)) {
        const surface = text.slice(start, katakanaEnd);
        const existing = byEnd.get(katakanaEnd) ?? [];
        const simpleMatches = existing.filter(value => value.kind === 'simple');
        const proxies = this.#lexicon.katakanaProxy(surface, simpleMatches);
        if (proxies.length > 0) byEnd.set(katakanaEnd, dedupeCandidates([...existing, ...proxies]));
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
        raw.push({ start, end, candidates: dedupeCandidates(values) });
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
          ? this.#suffixes.withSuruBreak(candidate.scoreFacts)
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

        const segsplit = this.#suffixes.segmentSplit(candidate);
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
    return dedupeCandidates(result);
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
      const direct = this.#lexicon.lexical(variant.source.text)
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

}

export function openPortableAnalyzer(source: PortableAnalyzerSource): PortableAnalyzer {
  return new PortableAnalyzer(source);
}
