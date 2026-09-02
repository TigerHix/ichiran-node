import type {
  PortableAnalysisAlternative,
  PortableAnalysisComponent,
  PortableAnalysisInflection,
  PortableAnalysisPath,
  PortableAnalysisResult,
  PortableAnalysisRoot,
  PortableAnalysisToken
} from '../src/analyzer.js';
import { testWord } from '../src/characters.js';

export interface CanonicalDifference {
  readonly path: string;
  readonly kind: 'type' | 'missing' | 'length' | 'value';
  readonly expected: unknown;
  readonly actual: unknown;
}

export interface IdentityResolver {
  roots(
    seq: number,
    surface?: string,
    sources?: readonly IdentitySource[]
  ): Promise<readonly number[]>;
}

export interface IdentitySource {
  readonly form: string | null;
  readonly reading: string | null;
}

export interface IdentityNormalization {
  readonly value: unknown;
  readonly rewritten: number;
  readonly multipleRoots: Readonly<Record<string, readonly number[]>>;
}

function jsonValue(value: unknown): unknown {
  return JSON.parse(JSON.stringify(value)) as unknown;
}

function score(value: unknown): number | null {
  if (typeof value !== 'object' || value === null || Array.isArray(value)) return null;
  const found = (value as Record<string, unknown>).score;
  return typeof found === 'number' ? found : null;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function object(value: unknown): Record<string, unknown> | null {
  return typeof value === 'object' && value !== null && !Array.isArray(value)
    ? value as Record<string, unknown>
    : null;
}

function string(value: unknown): string | null {
  return typeof value === 'string' ? value : null;
}

function number(value: unknown): number | readonly number[] | null {
  if (typeof value === 'number') return value;
  return Array.isArray(value) && value.every(item => typeof item === 'number')
    ? value as number[]
    : null;
}

function readingParts(value: unknown): readonly [string | null, string | null] {
  if (typeof value !== 'string') return [null, null];
  const match = /^(.*?) 【(.*?)】$/.exec(value);
  return match ? [match[1] ?? null, match[2] ?? null] : [value, value];
}

function legacyConjugationSources(value: unknown): readonly IdentitySource[] {
  const sources = new Map<string, IdentitySource>();
  const visit = (candidate: unknown): void => {
    if (Array.isArray(candidate)) {
      for (const item of candidate) visit(item);
      return;
    }
    const source = object(candidate);
    if (!source) return;
    if (typeof source.reading === 'string') {
      const [form, reading] = readingParts(source.reading);
      const identity = { form, reading };
      sources.set(JSON.stringify([form, reading]), identity);
    }
    if (Array.isArray(source.via)) visit(source.via);
  };
  visit(value);
  return [...sources.values()].sort((left, right) =>
    compareText(JSON.stringify([left.form, left.reading]), JSON.stringify([right.form, right.reading])));
}

function propertyTuple(value: unknown): readonly unknown[] | null {
  const source = object(value);
  if (!source) return null;
  const pos = string(source.pos);
  const type = typeof source.type === 'number' || typeof source.type === 'string'
    ? source.type
    : null;
  if (pos === null || type === null) return null;
  return [
    pos,
    type,
    source.negative ?? source.neg ?? null,
    source.formal ?? source.fml ?? null,
    typeof source.ordinal === 'number' ? source.ordinal : null
  ];
}

function cleanInflection(value: unknown): readonly (readonly unknown[])[] {
  if (!Array.isArray(value)) return [];
  return value.flatMap(item => {
    const tuple = propertyTuple(item);
    return tuple ? [tuple] : [];
  });
}

function legacyConjugation(value: unknown): {
  readonly sourceForm: string | null;
  readonly sourceReading: string | null;
  readonly inflection: readonly (readonly unknown[])[];
} {
  if (!Array.isArray(value) || value.length === 0) {
    return { sourceForm: null, sourceReading: null, inflection: [] };
  }
  const selected = object(value[0]);
  if (!selected) return { sourceForm: null, sourceReading: null, inflection: [] };
  const own = Array.isArray(selected.prop)
    ? selected.prop.flatMap(item => {
        const tuple = propertyTuple(item);
        return tuple ? [tuple] : [];
      })
    : [];
  if (Array.isArray(selected.via)) {
    const prefix = legacyConjugation(selected.via);
    return {
      sourceForm: prefix.sourceForm,
      sourceReading: prefix.sourceReading,
      inflection: [...prefix.inflection, ...own]
    };
  }
  const [sourceForm, sourceReading] = readingParts(selected.reading);
  return { sourceForm, sourceReading, inflection: own };
}

/**
 * Explicit analyzer tie key from docs/browser-alpha/ACCEPTANCE.md. Presentation
 * metadata (glosses, descriptions, romanization and object layout) is never a
 * tiebreaker, so canonicalization cannot hide a presentation regression.
 */
export function semanticCandidateKey(value: unknown): string {
  const source = object(value);
  if (!source) return JSON.stringify([null, null, null, null, null, [], []]);
  const root = object(source.root);
  const legacy = legacyConjugation(source.conj);
  const surface = string(source.surface) ?? string(source.text);
  const explicitRoute = string(source.route);
  const displayReading = string(source.reading);
  const route = explicitRoute
    ?? (displayReading?.includes('【') ? 'kanji' : surface === displayReading ? 'kana' : null);
  const rootSeq = number(root?.seq) ?? number(source.rootSeq) ?? number(source.seq);
  const [directForm, directReading] = readingParts(displayReading);
  const sourceForm = string(root?.form)
    ?? string(source.sourceForm)
    ?? legacy.sourceForm
    ?? directForm;
  const sourceReading = string(root?.reading)
    ?? string(source.sourceReading)
    ?? legacy.sourceReading
    ?? string(source.kana)
    ?? directReading;
  const inflection = Array.isArray(source.inflection)
    ? cleanInflection(source.inflection)
    : legacy.inflection;
  const components = Array.isArray(source.components)
    ? source.components.map(semanticCandidateKey)
    : [];
  return JSON.stringify([
    route, surface, rootSeq, sourceForm, sourceReading, inflection, components
  ]);
}

function sortEqualScoreRuns(values: readonly unknown[]): unknown[] {
  const result = [...values];
  for (let start = 0; start < result.length;) {
    const currentScore = score(result[start]);
    if (currentScore === null) {
      start++;
      continue;
    }
    let end = start + 1;
    while (end < result.length && score(result[end]) === currentScore) end++;
    if (end - start > 1) {
      result.splice(start, end - start, ...result.slice(start, end).sort((left, right) =>
        compareText(semanticCandidateKey(left), semanticCandidateKey(right))));
    }
    start = end;
  }
  return result;
}

function pathScore(value: unknown): number | null {
  if (Array.isArray(value)
    && value.length === 2
    && Array.isArray(value[0])
    && typeof value[1] === 'number') return value[1];
  const source = object(value);
  return source && Array.isArray(source.tokens) && typeof source.score === 'number'
    ? source.score
    : null;
}

function pathSemanticKey(value: unknown): string {
  if (Array.isArray(value) && Array.isArray(value[0])) {
    return JSON.stringify(value[0].map(token =>
      Array.isArray(token) && token.length >= 2 ? semanticCandidateKey(token[1]) : semanticCandidateKey(token)));
  }
  const source = object(value);
  return JSON.stringify(Array.isArray(source?.tokens)
    ? source.tokens.map(semanticCandidateKey)
    : []);
}

function sortEqualPathRuns(values: readonly unknown[]): unknown[] {
  if (values.length === 0 || values.some(value => pathScore(value) === null)) return [...values];
  const result = [...values];
  for (let start = 0; start < result.length;) {
    const currentScore = pathScore(result[start])!;
    let end = start + 1;
    while (end < result.length && pathScore(result[end]) === currentScore) end++;
    if (end - start > 1) {
      result.splice(start, end - start, ...result.slice(start, end).sort((left, right) =>
        compareText(pathSemanticKey(left), pathSemanticKey(right))));
    }
    start = end;
  }
  return result;
}

/**
 * Canonical comparison form from the alpha contract: object keys are sorted,
 * array order is retained, and only equal-score path/alternative runs may move.
 */
export function canonicalizeAnalyzerOutput(input: unknown): unknown {
  const visit = (value: unknown): unknown => {
    if (Array.isArray(value)) {
      return sortEqualPathRuns(value.map(visit));
    }
    if (typeof value !== 'object' || value === null) return value;
    const source = value as Record<string, unknown>;
    const output: Record<string, unknown> = {};
    for (const key of Object.keys(source).sort()) {
      const child = visit(source[key]);
      output[key] = (key === 'alternative' || key === 'alternatives') && Array.isArray(child)
        ? sortEqualScoreRuns(child)
        : child;
    }
    return output;
  };
  return visit(jsonValue(input));
}

function kind(value: unknown): string {
  if (value === null) return 'null';
  return Array.isArray(value) ? 'array' : typeof value;
}

function childPath(parent: string, key: string | number): string {
  return typeof key === 'number' ? `${parent}[${key}]` : `${parent}.${key}`;
}

/** Return the first exact semantic difference without creating enormous diffs. */
export function firstCanonicalDifference(expectedInput: unknown, actualInput: unknown): CanonicalDifference | null {
  const expected = canonicalizeAnalyzerOutput(expectedInput);
  const actual = canonicalizeAnalyzerOutput(actualInput);
  const visit = (left: unknown, right: unknown, path: string): CanonicalDifference | null => {
    if (Object.is(left, right)) return null;
    if (kind(left) !== kind(right)) {
      return { path, kind: 'type', expected: left, actual: right };
    }
    if (Array.isArray(left) && Array.isArray(right)) {
      if (left.length !== right.length) {
        return { path, kind: 'length', expected: left.length, actual: right.length };
      }
      for (let index = 0; index < left.length; index++) {
        const difference = visit(left[index], right[index], childPath(path, index));
        if (difference) return difference;
      }
      return null;
    }
    if (typeof left === 'object' && left !== null && typeof right === 'object' && right !== null) {
      const leftObject = left as Record<string, unknown>;
      const rightObject = right as Record<string, unknown>;
      const keys = [...new Set([...Object.keys(leftObject), ...Object.keys(rightObject)])].sort();
      for (const key of keys) {
        if (!(key in leftObject) || !(key in rightObject)) {
          return {
            path: childPath(path, key),
            kind: 'missing',
            expected: leftObject[key],
            actual: rightObject[key]
          };
        }
        const difference = visit(leftObject[key], rightObject[key], childPath(path, key));
        if (difference) return difference;
      }
      return null;
    }
    return { path, kind: 'value', expected: left, actual: right };
  };
  return visit(expected, actual, '$');
}

function skeletonWord(input: unknown): unknown {
  if (typeof input !== 'object' || input === null || Array.isArray(input)) return input;
  const word = input as Record<string, unknown>;
  if (Array.isArray(word.alternative)) {
    return { alternative: word.alternative.map(skeletonWord) };
  }
  const output: Record<string, unknown> = {};
  for (const key of ['reading', 'text', 'kana', 'score', 'compound', 'counter'] as const) {
    if (key in word) output[key] = word[key];
  }
  if (Array.isArray(word.components)) output.components = word.components.map(skeletonWord);
  return output;
}

/** Segmentation/score projection used to separate analyzer failures from presentation-only failures. */
export function legacyPathSkeleton(input: unknown): unknown {
  if (!Array.isArray(input)) return input;
  return input.map(chunk => {
    if (typeof chunk === 'string' || !Array.isArray(chunk)) return chunk;
    return chunk.map(path => {
      if (!Array.isArray(path) || path.length !== 2 || !Array.isArray(path[0])) return path;
      return [path[0].map(token => {
        if (!Array.isArray(token) || token.length !== 3) return token;
        return [token[0], skeletonWord(token[1]), token[2]];
      }), path[1]];
    });
  });
}

/**
 * Replace generated entry identities with terminal root identities. A physical
 * generated entry can represent multiple roots; those are intentionally made
 * loud as a sorted array and recorded instead of choosing one arbitrarily.
 */
export async function normalizeLegacyIdentities(
  input: unknown,
  resolver: IdentityResolver
): Promise<IdentityNormalization> {
  let rewritten = 0;
  const multipleRoots = new Map<string, readonly number[]>();
  const visit = async (value: unknown): Promise<unknown> => {
    if (Array.isArray(value)) return Promise.all(value.map(visit));
    if (typeof value !== 'object' || value === null) return value;
    const source = value as Record<string, unknown>;
    const output: Record<string, unknown> = {};
    for (const [key, child] of Object.entries(source)) output[key] = await visit(child);
    if (typeof source.seq === 'number') {
      const surface = typeof source.text === 'string' ? source.text : undefined;
      const sources = legacyConjugationSources(source.conj);
      const roots = [...new Set(await resolver.roots(source.seq, surface, sources))]
        .sort((left, right) => left - right);
      if (roots.length === 1 && roots[0] !== source.seq) {
        output.seq = roots[0];
        rewritten++;
      } else if (roots.length > 1) {
        output.seq = roots;
        multipleRoots.set(surface === undefined ? String(source.seq) : `${source.seq}:${surface}`, roots);
        rewritten++;
      }
    }
    return output;
  };
  const value = await visit(jsonValue(input));
  return {
    value,
    rewritten,
    multipleRoots: Object.fromEntries(multipleRoots)
  };
}

export interface CleanAnalysisRoot {
  readonly seq: number;
  readonly form: string;
  readonly reading: string;
}

export interface CleanAnalysisInflection {
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
  readonly ordinal: number;
}

export interface CleanAnalysisComponent {
  readonly text: string;
  readonly trueText: string | null;
  readonly route: 'kanji' | 'kana';
  readonly reading: string;
  readonly readings: readonly string[];
  readonly root: CleanAnalysisRoot | null;
  readonly inflection: readonly CleanAnalysisInflection[];
  readonly primary: boolean;
}

export interface CleanAnalysisCandidate {
  readonly text: string;
  readonly trueText: string | null;
  readonly route: 'kanji' | 'kana';
  readonly reading: string;
  readonly readings: readonly string[];
  readonly score: number;
  readonly root: CleanAnalysisRoot | null;
  readonly inflection: readonly CleanAnalysisInflection[];
  readonly components: readonly CleanAnalysisComponent[];
  readonly counter: readonly [string, boolean] | null;
}

export interface CleanAnalysisToken {
  readonly start: number;
  readonly end: number;
  readonly text: string;
  readonly trueText: string | null;
  readonly route: 'kanji' | 'kana' | 'gap';
  readonly reading: string;
  readonly readings: readonly string[];
  readonly score: number;
  readonly root: CleanAnalysisRoot | null;
  readonly inflection: readonly CleanAnalysisInflection[];
  readonly components: readonly CleanAnalysisComponent[];
  readonly alternatives: readonly CleanAnalysisCandidate[];
  readonly skipped: number;
  readonly entity: boolean;
  readonly counter: readonly [string, boolean] | null;
}

export interface CleanAnalysisPath {
  readonly score: number;
  readonly tokens: readonly CleanAnalysisToken[];
}

export type CleanAnalysisChunk = {
  readonly type: 'misc';
  readonly start: number;
  readonly end: number;
  readonly text: string;
} | {
  readonly type: 'word';
  readonly start: number;
  readonly end: number;
  readonly text: string;
  readonly paths: readonly CleanAnalysisPath[];
};

export interface CleanAnalysisResult {
  readonly input: string;
  readonly normalized: string;
  readonly chunks: readonly CleanAnalysisChunk[];
  readonly paths: readonly CleanAnalysisPath[];
}

export interface CoreWordLike {
  readonly type: string;
  readonly text: string;
  readonly trueText?: string | null;
  readonly kana: string | readonly string[];
  readonly seq?: number | readonly number[] | null;
  readonly conjugations?: readonly number[] | ':root' | null;
  readonly score?: number;
  readonly components?: readonly CoreWordLike[];
  readonly alternative?: boolean;
  readonly primary?: boolean;
  readonly start?: number;
  readonly end?: number;
  readonly counter?: readonly [string, boolean] | null;
  readonly skipped?: number;
  readonly isEntity?: boolean;
}

export interface CoreResolvedWord {
  readonly root: CleanAnalysisRoot | null;
  readonly inflection: readonly CleanAnalysisInflection[];
}

export type CoreWordResolver = (word: CoreWordLike) => Promise<CoreResolvedWord>;

export interface CoreRawAnalysisInput {
  readonly input: string;
  readonly normalized: string;
  readonly limit: number;
  readonly segments: readonly { readonly type: string; readonly text: string }[];
  readonly raw: unknown;
  readonly resolveWord: CoreWordResolver;
}

function readings(value: string | readonly string[]): string[] {
  return [...new Set(Array.isArray(value) ? value : [value])];
}

function normalizedTrueText(text: string, trueText: string | null | undefined): string | null {
  return trueText === undefined || trueText === null || trueText === text ? null : trueText;
}

function portableRoot(value: PortableAnalysisRoot | null): CleanAnalysisRoot | null {
  return value && { seq: value.seq, form: value.form, reading: value.reading };
}

function portableInflection(
  values: readonly PortableAnalysisInflection[]
): CleanAnalysisInflection[] {
  return values.map(value => ({
    pos: value.pos,
    type: value.type,
    negative: value.negative,
    formal: value.formal,
    ordinal: value.ordinal
  }));
}

function portableComponent(value: PortableAnalysisComponent): CleanAnalysisComponent {
  return {
    text: value.text,
    trueText: value.trueText,
    route: value.route,
    reading: value.reading,
    readings: [value.reading],
    root: portableRoot(value.root),
    inflection: portableInflection(value.inflection),
    primary: value.primary
  };
}

function portableCandidate(value: PortableAnalysisAlternative): CleanAnalysisCandidate {
  return {
    text: value.text,
    trueText: value.trueText,
    route: value.route,
    reading: value.reading,
    readings: [value.reading],
    score: value.score,
    root: portableRoot(value.root),
    inflection: portableInflection(value.inflection),
    components: value.components.map(portableComponent),
    counter: value.counter
  };
}

function uniqueCandidateReadings(
  values: readonly CleanAnalysisCandidate[],
  fallback: string
): string[] {
  const result = [...new Set(values.flatMap(value => value.readings))];
  return result.length > 0 ? result : [fallback];
}

function portableToken(value: PortableAnalysisToken): CleanAnalysisToken {
  // candidateId is deliberately absent from the PostgreSQL/Lisp clean projection:
  // it is a request-local reference with no oracle counterpart. Rust same-pack
  // qualification compares the full public DTO, including these IDs, byte-for-byte.
  const alternatives = value.alternatives.map(portableCandidate);
  return {
    start: value.start,
    end: value.end,
    text: value.text,
    trueText: value.trueText,
    route: value.route,
    reading: value.reading,
    readings: uniqueCandidateReadings(alternatives, value.reading),
    score: value.score,
    root: portableRoot(value.root),
    inflection: portableInflection(value.inflection),
    components: value.components.map(portableComponent),
    alternatives,
    skipped: value.skipped,
    entity: value.entity,
    counter: value.counter
  };
}

function portablePath(value: PortableAnalysisPath): CleanAnalysisPath {
  return { score: value.score, tokens: value.tokens.map(portableToken) };
}

/** Presentation-free projection of the public portable analyzer result. */
export function projectPortableCleanAnalysis(result: PortableAnalysisResult): CleanAnalysisResult {
  return {
    input: result.input,
    normalized: result.normalized,
    chunks: result.chunks.map(chunk => chunk.type === 'misc' ? {
      type: 'misc', start: chunk.start, end: chunk.end, text: chunk.text
    } : {
      type: 'word', start: chunk.start, end: chunk.end, text: chunk.text,
      paths: chunk.paths.map(portablePath)
    }),
    paths: result.paths.map(portablePath)
  };
}

async function coreComponents(
  values: readonly CoreWordLike[],
  resolveWord: CoreWordResolver
): Promise<CleanAnalysisComponent[]> {
  return Promise.all(values.map(async value => {
    const identity = await coreIdentity(value, resolveWord);
    const valueReadings = readings(value.kana);
    return {
      text: value.text,
      trueText: normalizedTrueText(value.text, value.trueText),
      route: value.type.toLowerCase() === 'kana' ? 'kana' : 'kanji',
      reading: valueReadings[0] ?? '',
      readings: valueReadings,
      root: identity.root,
      inflection: identity.inflection,
      primary: value.primary ?? true
    };
  }));
}

async function coreIdentity(
  value: CoreWordLike,
  resolveWord: CoreWordResolver
): Promise<CoreResolvedWord> {
  if (value.components && value.components.length > 0 && !value.alternative) {
    const components = await coreComponents(value.components, resolveWord);
    return {
      root: components.find(component => component.primary)?.root
        ?? components[0]?.root
        ?? null,
      inflection: components.at(-1)?.inflection ?? []
    };
  }
  return resolveWord(value);
}

async function coreCandidate(
  value: CoreWordLike,
  resolveWord: CoreWordResolver
): Promise<CleanAnalysisCandidate> {
  const identity = await coreIdentity(value, resolveWord);
  const valueReadings = readings(value.kana);
  return {
    text: value.text,
    // Core uses the counter suffix alone as internal trueText while the public
    // analyzer treats the rendered number+counter as the surface itself.
    trueText: value.counter ? null : normalizedTrueText(value.text, value.trueText),
    route: value.type.toLowerCase() === 'kana' ? 'kana' : 'kanji',
    reading: valueReadings[0] ?? '',
    readings: valueReadings,
    score: value.score ?? 0,
    root: identity.root,
    inflection: identity.inflection,
    components: await coreComponents(value.components ?? [], resolveWord),
    counter: value.counter ? [value.counter[0], value.counter[1]] : null
  };
}

async function coreToken(
  value: CoreWordLike,
  chunkStart: number,
  resolveWord: CoreWordResolver
): Promise<CleanAnalysisToken> {
  const route = value.type.toLowerCase();
  const isGap = route === 'gap';
  const syntheticEntity = value.isEntity === true
    && !(typeof value.seq === 'number' && value.seq >= 0);
  const candidateWords = isGap || syntheticEntity
    ? []
    : value.alternative ? value.components ?? [] : [value];
  const alternatives = await Promise.all(candidateWords.map(candidate =>
    coreCandidate(candidate, resolveWord)));
  const primary = alternatives[0];
  const outerReadings = readings(value.kana);
  const reading = outerReadings[0] ?? '';
  return {
    start: chunkStart + (value.start ?? 0),
    end: chunkStart + (value.end ?? value.text.length),
    text: value.text,
    trueText: value.counter
      ? null
      : primary?.trueText ?? normalizedTrueText(value.text, value.trueText),
    route: isGap
      ? 'gap'
      : syntheticEntity
        ? (testWord(value.text, 'kana') ? 'kana' : 'kanji')
        : primary?.route ?? (route === 'kana' ? 'kana' : 'kanji'),
    reading,
    readings: uniqueCandidateReadings(alternatives, reading),
    score: value.score ?? 0,
    root: primary?.root ?? null,
    inflection: primary?.inflection ?? [],
    components: primary?.components ?? [],
    alternatives,
    skipped: value.skipped ?? 0,
    entity: value.isEntity ?? false,
    counter: primary?.counter ?? null
  };
}

function corePathTuple(value: unknown): readonly [readonly unknown[], number] {
  if (
    !Array.isArray(value)
    || value.length !== 2
    || !Array.isArray(value[0])
    || typeof value[1] !== 'number'
  ) throw new Error('Fresh core romanizeStar returned an invalid local path');
  return [value[0], value[1]];
}

async function corePath(
  value: unknown,
  chunkStart: number,
  resolveWord: CoreWordResolver
): Promise<CleanAnalysisPath> {
  const [tuples, pathScoreValue] = corePathTuple(value);
  const tokens = await Promise.all(tuples.map(tuple => {
    if (!Array.isArray(tuple) || tuple.length < 2 || !object(tuple[1])) {
      throw new Error('Fresh core romanizeStar returned an invalid token tuple');
    }
    return coreToken(tuple[1] as unknown as CoreWordLike, chunkStart, resolveWord);
  }));
  return { score: pathScoreValue, tokens };
}

function cleanGap(text: string, start: number, end: number): CleanAnalysisToken {
  return {
    start, end, text, trueText: null, route: 'gap', reading: text, readings: [text],
    score: 0, root: null, inflection: [], components: [], alternatives: [],
    skipped: 0, entity: false, counter: null
  };
}

function mergeCleanPaths(
  left: readonly CleanAnalysisPath[],
  right: readonly CleanAnalysisPath[],
  limit: number
): CleanAnalysisPath[] {
  const result: CleanAnalysisPath[] = [];
  for (const prefix of left) {
    for (const suffix of right) {
      result.push({
        score: prefix.score + suffix.score,
        tokens: [...prefix.tokens, ...suffix.tokens]
      });
    }
  }
  result.sort((a, b) => b.score - a.score);
  return result.slice(0, limit);
}

/**
 * Derive analyzer semantics directly from fresh romanizeStar WordInfo objects.
 * No transformed legacy JSON is consumed here.
 */
export async function projectCoreCleanAnalysis(
  input: CoreRawAnalysisInput
): Promise<CleanAnalysisResult> {
  if (!Array.isArray(input.raw) || input.raw.length !== input.segments.length) {
    throw new Error(
      `Fresh core chunk count ${Array.isArray(input.raw) ? input.raw.length : 'invalid'} does not match basicSplit ${input.segments.length}`
    );
  }
  const chunks: CleanAnalysisChunk[] = [];
  let paths: CleanAnalysisPath[] = [{ score: 0, tokens: [] }];
  let offset = 0;
  for (let index = 0; index < input.segments.length; index++) {
    const segment = input.segments[index]!;
    const rawChunk = input.raw[index];
    const start = offset;
    const end = start + segment.text.length;
    if (segment.type.toLowerCase() === 'misc') {
      if (typeof rawChunk !== 'string' || rawChunk !== segment.text) {
        throw new Error(`Fresh core misc chunk ${index} does not match basicSplit`);
      }
      chunks.push({ type: 'misc', start, end, text: segment.text });
      const gap = cleanGap(segment.text, start, end);
      paths = paths.map(path => ({ ...path, tokens: [...path.tokens, gap] }));
    } else {
      if (!Array.isArray(rawChunk)) {
        throw new Error(`Fresh core word chunk ${index} is not a path array`);
      }
      const localPaths = await Promise.all(rawChunk.map(path =>
        corePath(path, start, input.resolveWord)));
      chunks.push({ type: 'word', start, end, text: segment.text, paths: localPaths });
      paths = mergeCleanPaths(paths, localPaths, input.limit);
    }
    offset = end;
  }
  return { input: input.input, normalized: input.normalized, chunks, paths };
}
