import { createHash, randomUUID } from 'node:crypto';
import { join } from 'node:path';
import { unlinkSync } from 'node:fs';
import type { AnalyzerSupportSplitConjugationSource } from '../browser-pack/analyzer-support.js';
import type { ConjugationProperty } from './model.js';
import {
  generatedPathSpoolRows,
  readGeneratedOccurrenceSpool,
  readGeneratedPathSpool,
  writeGeneratedOccurrenceSpool,
  type GeneratedOccurrenceSpoolRow
} from './generated-projection-spool.js';

const NULL_U16 = 0xffff;
const NULL_U32 = 0xffff_ffff;

export interface GeneratedPathTable {
  readonly length: number;
  readonly rootSeqs: Uint32Array;
  readonly firstAliases: Uint16Array;
  readonly secondAliases: Uint16Array;
  readonly targetSeqs: Uint32Array;
  readonly viaTargetSeqs: Uint32Array;
}

export interface ResolvedGeneratedOccurrence extends GeneratedOccurrenceSpoolRow {
  readonly rootSeq: number;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
}

export interface GeneratedOccurrenceSurface {
  readonly route: 'kana' | 'kanji';
  readonly surface: string;
  readonly occurrences: readonly ResolvedGeneratedOccurrence[];
}

export interface GeneratedOccurrenceReductionSummary {
  readonly rows: number;
  readonly chunks: number;
  readonly surfaces: number;
  readonly maxSurfaceRows: number;
}

export interface GeneratedLookupLocator {
  readonly rootSeq: number;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
}

export interface GeneratedLookupClass {
  readonly targetSeq: number;
  readonly precedence: number;
  readonly locators: readonly GeneratedLookupLocator[];
}

export interface GeneratedRulePathTarget {
  readonly rootSeq: number;
  readonly firstRule: number;
  readonly secondRule: number | null;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
}

export interface GeneratedPhysicalProperty {
  readonly alias: number;
  readonly propOrd: number;
  readonly firstOrdinal: number;
}

export interface GeneratedPhysicalMember {
  readonly rootSeq: number;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
  readonly memberOrd: number;
  readonly firstOrdinal: number;
  readonly properties: readonly GeneratedPhysicalProperty[];
}

export interface GeneratedPhysicalTargetMembers {
  readonly targetSeq: number;
  readonly paths: number;
  readonly members: readonly GeneratedPhysicalMember[];
}

export interface GeneratedPhysicalMemberSummary {
  readonly paths: number;
  readonly targets: number;
  readonly members: number;
  readonly properties: number;
  readonly maxTargetPaths: number;
}

export interface GeneratedSemanticPath {
  readonly ordinal: number;
  readonly rootSeq: number;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
}

export interface GeneratedSemanticPathSummary {
  readonly paths: number;
  readonly roots: number;
  readonly rootTargets: number;
  readonly sha256: string;
}

interface RunCursor {
  readonly chunk: number;
  readonly iterator: Generator<GeneratedOccurrenceSpoolRow>;
  readonly row: GeneratedOccurrenceSpoolRow;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function nullable(value: number | null): number {
  return value ?? -1;
}

function pathValue(values: Uint32Array | Uint16Array, ordinal: number, label: string): number {
  if (!Number.isSafeInteger(ordinal) || ordinal < 0 || ordinal >= values.length) {
    throw new Error(`Generated occurrence references missing path ${ordinal}`);
  }
  const value = values[ordinal];
  if (value === undefined) throw new Error(`Generated path ${ordinal} has no ${label}`);
  return value;
}

export function loadGeneratedPathTable(path: string): GeneratedPathTable {
  const length = generatedPathSpoolRows(path);
  const rootSeqs = new Uint32Array(length);
  const firstAliases = new Uint16Array(length);
  const secondAliases = new Uint16Array(length);
  const targetSeqs = new Uint32Array(length);
  const viaTargetSeqs = new Uint32Array(length);
  let loaded = 0;
  for (const row of readGeneratedPathSpool(path)) {
    rootSeqs[row.ordinal] = row.rootSeq;
    firstAliases[row.ordinal] = row.firstAlias;
    secondAliases[row.ordinal] = row.secondAlias ?? NULL_U16;
    targetSeqs[row.ordinal] = row.targetSeq;
    viaTargetSeqs[row.ordinal] = row.viaTargetSeq ?? NULL_U32;
    loaded++;
  }
  if (loaded !== length) throw new Error(`Generated path spool loaded ${loaded}/${length} rows`);
  return { length, rootSeqs, firstAliases, secondAliases, targetSeqs, viaTargetSeqs };
}

function resolveOccurrence(
  row: GeneratedOccurrenceSpoolRow,
  paths: GeneratedPathTable
): ResolvedGeneratedOccurrence {
  const secondAlias = pathValue(paths.secondAliases, row.pathOrdinal, 'second alias');
  const viaTargetSeq = pathValue(paths.viaTargetSeqs, row.pathOrdinal, 'via target');
  return {
    ...row,
    rootSeq: pathValue(paths.rootSeqs, row.pathOrdinal, 'root'),
    firstAlias: pathValue(paths.firstAliases, row.pathOrdinal, 'first alias'),
    secondAlias: secondAlias === NULL_U16 ? null : secondAlias,
    targetSeq: pathValue(paths.targetSeqs, row.pathOrdinal, 'target'),
    viaTargetSeq: viaTargetSeq === NULL_U32 ? null : viaTargetSeq
  };
}

function pathOrdinals(length: number): Uint32Array {
  const values = new Uint32Array(length);
  for (let ordinal = 0; ordinal < length; ordinal++) values[ordinal] = ordinal;
  return values;
}

function compareOccurrences(
  left: GeneratedOccurrenceSpoolRow,
  right: GeneratedOccurrenceSpoolRow,
  paths: GeneratedPathTable
): number {
  const route = Number(left.route === 'kanji') - Number(right.route === 'kanji');
  if (route !== 0) return route;
  const surface = compareText(left.surface, right.surface);
  if (surface !== 0) return surface;
  const target = pathValue(paths.targetSeqs, left.pathOrdinal, 'target')
    - pathValue(paths.targetSeqs, right.pathOrdinal, 'target');
  if (target !== 0) return target;
  const root = pathValue(paths.rootSeqs, left.pathOrdinal, 'root')
    - pathValue(paths.rootSeqs, right.pathOrdinal, 'root');
  if (root !== 0) return root;
  return left.firstRule - right.firstRule
    || nullable(left.secondRule) - nullable(right.secondRule)
    || left.pathOrdinal - right.pathOrdinal
    || left.precedence - right.precedence
    || Number(left.kind === 'patch') - Number(right.kind === 'patch');
}

function compareRuns(left: RunCursor, right: RunCursor, paths: GeneratedPathTable): number {
  return compareOccurrences(left.row, right.row, paths) || left.chunk - right.chunk;
}

function pushRun(heap: RunCursor[], value: RunCursor, paths: GeneratedPathTable): void {
  heap.push(value);
  let index = heap.length - 1;
  while (index > 0) {
    const parent = (index - 1) >>> 1;
    if (compareRuns(heap[parent]!, value, paths) <= 0) break;
    heap[index] = heap[parent]!;
    index = parent;
  }
  heap[index] = value;
}

function popRun(heap: RunCursor[], paths: GeneratedPathTable): RunCursor | undefined {
  const first = heap[0];
  const last = heap.pop();
  if (!first || !last || heap.length === 0) return first;
  let index = 0;
  while (true) {
    const left = index * 2 + 1;
    if (left >= heap.length) break;
    const right = left + 1;
    const child = right < heap.length
      && compareRuns(heap[right]!, heap[left]!, paths) < 0 ? right : left;
    if (compareRuns(last, heap[child]!, paths) <= 0) break;
    heap[index] = heap[child]!;
    index = child;
  }
  heap[index] = last;
  return first;
}

/**
 * Sort the high-volume occurrence spool in fixed-size runs, then visit one
 * generated surface at a time. Temporary runs use the same exact row schema
 * and are deleted on success or failure.
 */
export function reduceGeneratedOccurrenceSurfaces(
  input: {
    readonly pathsPath: string;
    readonly occurrencesPath: string;
    readonly temporaryDirectory: string;
    readonly maxChunkRows?: number;
  },
  visit: (surface: GeneratedOccurrenceSurface) => void
): GeneratedOccurrenceReductionSummary {
  const maxChunkRows = input.maxChunkRows ?? 100_000;
  if (!Number.isSafeInteger(maxChunkRows) || maxChunkRows < 1) {
    throw new Error('Generated occurrence chunk size must be a positive integer');
  }
  const paths = loadGeneratedPathTable(input.pathsPath);
  const prefix = `generated-occurrences-${process.pid}-${randomUUID()}`;
  const chunkPaths: string[] = [];
  const iterators: Generator<GeneratedOccurrenceSpoolRow>[] = [];
  const chunk: GeneratedOccurrenceSpoolRow[] = [];
  let rows = 0;
  const flush = (): void => {
    if (chunk.length === 0) return;
    chunk.sort((left, right) => compareOccurrences(left, right, paths));
    const path = join(input.temporaryDirectory, `${prefix}-${chunkPaths.length}.bin`);
    writeGeneratedOccurrenceSpool(path, chunk);
    chunkPaths.push(path);
    chunk.length = 0;
  };

  try {
    for (const row of readGeneratedOccurrenceSpool(input.occurrencesPath)) {
      if (!row.installed) continue;
      resolveOccurrence(row, paths);
      chunk.push(row);
      rows++;
      if (chunk.length === maxChunkRows) flush();
    }
    flush();

    const heap: RunCursor[] = [];
    for (const [index, path] of chunkPaths.entries()) {
      const iterator = readGeneratedOccurrenceSpool(path);
      iterators.push(iterator);
      const first = iterator.next();
      if (!first.done) pushRun(heap, { chunk: index, iterator, row: first.value }, paths);
    }
    let group: ResolvedGeneratedOccurrence[] = [];
    let route: 'kana' | 'kanji' | null = null;
    let surface: string | null = null;
    let surfaces = 0;
    let maxSurfaceRows = 0;
    const emit = (): void => {
      if (route === null || surface === null) return;
      visit({ route, surface, occurrences: group });
      surfaces++;
      maxSurfaceRows = Math.max(maxSurfaceRows, group.length);
      group = [];
    };
    while (heap.length > 0) {
      const current = popRun(heap, paths)!;
      const resolved = resolveOccurrence(current.row, paths);
      if (route !== resolved.route || surface !== resolved.surface) {
        emit();
        route = resolved.route;
        surface = resolved.surface;
      }
      group.push(resolved);
      const next = current.iterator.next();
      if (!next.done) {
        pushRun(heap, { chunk: current.chunk, iterator: current.iterator, row: next.value }, paths);
      }
    }
    emit();
    return { rows, chunks: chunkPaths.length, surfaces, maxSurfaceRows };
  } finally {
    for (const iterator of iterators) iterator.return(undefined);
    for (const path of chunkPaths) unlinkSync(path);
  }
}

/** Collapse one sorted surface to the physical classes needed by lookup order. */
export function generatedLookupClasses(
  surface: GeneratedOccurrenceSurface
): readonly GeneratedLookupClass[] {
  const mutable = new Map<number, {
    precedence: number;
    locators: Map<string, GeneratedLookupLocator>;
  }>();
  for (const occurrence of surface.occurrences) {
    const value = mutable.get(occurrence.targetSeq) ?? {
      precedence: occurrence.precedence,
      locators: new Map<string, GeneratedLookupLocator>()
    };
    value.precedence = Math.max(value.precedence, occurrence.precedence);
    const locator = {
      rootSeq: occurrence.rootSeq,
      firstAlias: occurrence.firstAlias,
      secondAlias: occurrence.secondAlias
    };
    value.locators.set(JSON.stringify([
      locator.rootSeq, locator.firstAlias, locator.secondAlias
    ]), locator);
    mutable.set(occurrence.targetSeq, value);
  }
  return [...mutable].map(([targetSeq, value]) => ({
    targetSeq,
    precedence: value.precedence,
    locators: [...value.locators.values()].sort((left, right) =>
      left.rootSeq - right.rootSeq
      || left.firstAlias - right.firstAlias
      || nullable(left.secondAlias) - nullable(right.secondAlias))
  })).sort((left, right) =>
    right.precedence - left.precedence || left.targetSeq - right.targetSeq);
}

/**
 * Reduce physical members in explicit source schedule order. Member and
 * property ordinals are the first path ordinal that introduced each identity;
 * no target id, JSON text, or hash iteration decides observable order.
 */
export function reduceGeneratedPhysicalMembers(
  pathsPath: string,
  visit: (target: GeneratedPhysicalTargetMembers) => void
): GeneratedPhysicalMemberSummary {
  const paths = loadGeneratedPathTable(pathsPath);
  const ordinals = pathOrdinals(paths.length);
  ordinals.sort((left, right) =>
    paths.targetSeqs[left]! - paths.targetSeqs[right]! || left - right);
  let targets = 0;
  let memberCount = 0;
  let propertyCount = 0;
  let maxTargetPaths = 0;
  let at = 0;
  while (at < ordinals.length) {
    const first = ordinals[at]!;
    const targetSeq = paths.targetSeqs[first]!;
    const members = new Map<string, {
      rootSeq: number;
      viaTargetSeq: number | null;
      memberOrd: number;
      firstOrdinal: number;
      properties: Map<number, GeneratedPhysicalProperty>;
    }>();
    const start = at;
    while (at < ordinals.length && paths.targetSeqs[ordinals[at]!] === targetSeq) {
      const ordinal = ordinals[at++]!;
      const rootSeq = paths.rootSeqs[ordinal]!;
      const storedVia = paths.viaTargetSeqs[ordinal]!;
      const viaTargetSeq = storedVia === NULL_U32 ? null : storedVia;
      const key = `${rootSeq}\u0000${viaTargetSeq ?? -1}`;
      const member = members.get(key) ?? {
        rootSeq,
        viaTargetSeq,
        memberOrd: members.size,
        firstOrdinal: ordinal,
        properties: new Map<number, GeneratedPhysicalProperty>()
      };
      const storedSecond = paths.secondAliases[ordinal]!;
      const alias = storedSecond === NULL_U16 ? paths.firstAliases[ordinal]! : storedSecond;
      if (!member.properties.has(alias)) {
        member.properties.set(alias, {
          alias,
          propOrd: member.properties.size,
          firstOrdinal: ordinal
        });
      }
      members.set(key, member);
    }
    const output = [...members.values()].map(member => ({
      rootSeq: member.rootSeq,
      targetSeq,
      viaTargetSeq: member.viaTargetSeq,
      memberOrd: member.memberOrd,
      firstOrdinal: member.firstOrdinal,
      properties: [...member.properties.values()]
    }));
    visit({ targetSeq, paths: at - start, members: output });
    targets++;
    memberCount += output.length;
    propertyCount += output.reduce((total, member) => total + member.properties.length, 0);
    maxTargetPaths = Math.max(maxTargetPaths, at - start);
  }
  return {
    paths: paths.length,
    targets,
    members: memberCount,
    properties: propertyCount,
    maxTargetPaths
  };
}

/**
 * Visit semantic records in canonical numeric order and prove the compact
 * `(root, first alias, second alias)` identity remains unique.
 */
export function reduceGeneratedSemanticPaths(
  pathsPath: string,
  visit: (path: GeneratedSemanticPath) => void,
  includeTarget: (targetSeq: number) => boolean = () => true
): GeneratedSemanticPathSummary {
  const paths = loadGeneratedPathTable(pathsPath);
  const ordinals = pathOrdinals(paths.length);
  const second = (ordinal: number): number => {
    const value = paths.secondAliases[ordinal]!;
    return value === NULL_U16 ? -1 : value;
  };
  ordinals.sort((left, right) =>
    paths.rootSeqs[left]! - paths.rootSeqs[right]!
    || paths.firstAliases[left]! - paths.firstAliases[right]!
    || second(left) - second(right)
    || left - right);
  const digest = createHash('sha256');
  const bytes = Buffer.allocUnsafe(20);
  let priorRoot = -1;
  let priorFirst = -1;
  let priorSecond = -2;
  let roots = 0;
  let rootTargets = 0;
  let includedPaths = 0;
  let targets = new Set<number>();
  for (const ordinal of ordinals) {
    const rootSeq = paths.rootSeqs[ordinal]!;
    const firstAlias = paths.firstAliases[ordinal]!;
    const secondAlias = second(ordinal);
    const targetSeq = paths.targetSeqs[ordinal]!;
    if (!includeTarget(targetSeq)) continue;
    if (rootSeq === priorRoot && firstAlias === priorFirst && secondAlias === priorSecond) {
      throw new Error(`Duplicate generated semantic path ${rootSeq}/${firstAlias}/${secondAlias}`);
    }
    if (rootSeq !== priorRoot) {
      if (priorRoot !== -1) rootTargets += targets.size;
      roots++;
      targets = new Set<number>();
    }
    const storedVia = paths.viaTargetSeqs[ordinal]!;
    const value = {
      ordinal,
      rootSeq,
      firstAlias,
      secondAlias: secondAlias === -1 ? null : secondAlias,
      targetSeq,
      viaTargetSeq: storedVia === NULL_U32 ? null : storedVia
    };
    visit(value);
    includedPaths++;
    targets.add(targetSeq);
    bytes.writeUInt32LE(rootSeq, 0);
    bytes.writeUInt16LE(firstAlias, 4);
    bytes.writeUInt16LE(secondAlias === -1 ? NULL_U16 : secondAlias, 6);
    bytes.writeUInt32LE(targetSeq, 8);
    bytes.writeUInt32LE(storedVia, 12);
    bytes.writeUInt32LE(ordinal, 16);
    digest.update(bytes);
    priorRoot = rootSeq;
    priorFirst = firstAlias;
    priorSecond = secondAlias;
  }
  if (priorRoot !== -1) rootTargets += targets.size;
  return { paths: includedPaths, roots, rootTargets, sha256: digest.digest('hex') };
}

function rulePathKey(rootSeq: number, firstRule: number, secondRule: number | null): string {
  return `${rootSeq}\u0000${firstRule}\u0000${secondRule ?? -1}`;
}

/** Narrow path-to-target join for split/hint declaration roots only. */
export function collectGeneratedRulePathTargets(
  pathsPath: string,
  occurrencesPath: string,
  roots: ReadonlySet<number>
): readonly GeneratedRulePathTarget[] {
  const paths = loadGeneratedPathTable(pathsPath);
  const selected = new Set<number>();
  for (let ordinal = 0; ordinal < paths.length; ordinal++) {
    if (roots.has(paths.rootSeqs[ordinal]!)) selected.add(ordinal);
  }
  const values = new Map<string, GeneratedRulePathTarget>();
  for (const occurrence of readGeneratedOccurrenceSpool(occurrencesPath)) {
    if (!occurrence.installed) continue;
    if (occurrence.pathOrdinal >= paths.length) {
      throw new Error(`Generated occurrence references missing path ${occurrence.pathOrdinal}`);
    }
    if (!selected.has(occurrence.pathOrdinal)) continue;
    const rootSeq = paths.rootSeqs[occurrence.pathOrdinal]!;
    const targetSeq = paths.targetSeqs[occurrence.pathOrdinal]!;
    const storedVia = paths.viaTargetSeqs[occurrence.pathOrdinal]!;
    const value = {
      rootSeq,
      firstRule: occurrence.firstRule,
      secondRule: occurrence.secondRule,
      targetSeq,
      viaTargetSeq: storedVia === NULL_U32 ? null : storedVia
    };
    const key = rulePathKey(rootSeq, occurrence.firstRule, occurrence.secondRule);
    const prior = values.get(key);
    if (prior && (prior.targetSeq !== targetSeq || prior.viaTargetSeq !== value.viaTargetSeq)) {
      throw new Error(`Generated rule path maps to multiple targets ${key}`);
    }
    values.set(key, value);
  }
  return [...values.values()].sort((left, right) =>
    left.rootSeq - right.rootSeq
    || left.firstRule - right.firstRule
    || nullable(left.secondRule) - nullable(right.secondRule));
}

/** Narrow generated-member locators for split parts that were actually selected. */
export function collectGeneratedLocatorsForTargets(
  pathsPath: string,
  targetSeqs: ReadonlySet<number>,
  properties: readonly ConjugationProperty[]
): ReadonlyMap<number, readonly AnalyzerSupportSplitConjugationSource[]> {
  const mutable = new Map<number, Map<string, AnalyzerSupportSplitConjugationSource>>();
  for (const path of readGeneratedPathSpool(pathsPath)) {
    if (!targetSeqs.has(path.targetSeq)) continue;
    const alias = path.secondAlias ?? path.firstAlias;
    const property = properties[alias];
    if (!property) throw new Error(`Generated path has unknown property alias ${alias}`);
    const locator = {
      from: path.rootSeq,
      via: path.viaTargetSeq !== null,
      ...property
    };
    const values = mutable.get(path.targetSeq) ?? new Map<string, AnalyzerSupportSplitConjugationSource>();
    values.set(JSON.stringify([
      locator.from, locator.via, locator.pos, locator.type,
      locator.negative, locator.formal
    ]), locator);
    mutable.set(path.targetSeq, values);
  }
  return new Map([...mutable].map(([targetSeq, values]) => [
    targetSeq,
    [...values.values()].sort((left, right) =>
      left.from - right.from
      || Number(left.via) - Number(right.via)
      || compareText(left.pos, right.pos)
      || left.type - right.type
      || nullable(left.negative === null ? null : Number(left.negative))
        - nullable(right.negative === null ? null : Number(right.negative))
      || nullable(left.formal === null ? null : Number(left.formal))
        - nullable(right.formal === null ? null : Number(right.formal)))
  ]));
}
