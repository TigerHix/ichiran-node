import { createHash } from 'node:crypto';
import { createReadStream, createWriteStream } from 'node:fs';
import { once } from 'node:events';
import { createInterface } from 'node:readline';
import { surfaceRoute, type MorphologyCandidate } from '@ichiran/core';
import type { CanonicalEntry, ConjugationProperty } from './model.js';
import {
  emitCanonicalConjugations,
  type ConjugationEmission,
  type EmissionForm
} from './conjugation-emissions.js';

export interface ConjugationRelationKey {
  readonly rootSeq: number;
  readonly route: 'kana' | 'kanji';
  readonly surface: string;
  readonly sourceText: string;
  readonly sourceForm: string;
  readonly sourceReading: string;
  readonly first: ConjugationProperty;
  readonly second: ConjugationProperty | null;
  readonly intermediate: string | null;
  readonly sourceOrdinal: number;
  readonly sourceCommon: number | null;
}

export interface ReviewedRelationDelta {
  readonly side: 'omission' | 'packed-only';
  readonly key: string;
  readonly category: string;
  readonly provenance: string;
  readonly preservedBehavior: string;
}

export interface RelationSideSummary {
  readonly rows: number;
  readonly unique: number;
  readonly duplicates: number;
  readonly sha256: string;
}

export interface RelationDeltaCategory {
  readonly side: 'omission' | 'packed-only';
  readonly category: string;
  readonly reviewed: boolean;
  readonly count: number;
  readonly sha256: string;
  readonly examples: readonly string[];
}

export interface ConjugationRelationReport {
  readonly forward: RelationSideSummary;
  readonly packed: RelationSideSummary;
  readonly common: number;
  readonly omissions: number;
  readonly packedOnly: number;
  readonly reviewedDeltas: number;
  readonly unreviewedDeltas: number;
  readonly unusedReviewedRows: number;
  readonly differenceSha256: string;
  readonly categories: readonly RelationDeltaCategory[];
  readonly passed: boolean;
}

interface RelationGroup {
  readonly line: string;
  readonly count: number;
}

interface MutableSideSummary {
  rows: number;
  unique: number;
  duplicates: number;
  readonly hash: ReturnType<typeof createHash>;
}

interface MutableCategory {
  readonly side: 'omission' | 'packed-only';
  readonly category: string;
  readonly reviewed: boolean;
  count: number;
  readonly hash: ReturnType<typeof createHash>;
  readonly examples: string[];
}

function propertyTuple(property: ConjugationProperty): readonly unknown[] {
  return [property.pos, property.type, property.negative, property.formal];
}

export function canonicalConjugationRelationKey(key: ConjugationRelationKey): string {
  return JSON.stringify([
    key.rootSeq,
    key.route,
    key.surface,
    key.sourceText,
    key.sourceForm,
    key.sourceReading,
    propertyTuple(key.first),
    key.second === null ? null : propertyTuple(key.second),
    key.intermediate,
    key.sourceOrdinal,
    key.sourceCommon
  ]);
}

function propertyFromTuple(value: unknown, label: string): ConjugationProperty {
  if (!Array.isArray(value) || value.length !== 4
    || typeof value[0] !== 'string' || !Number.isSafeInteger(value[1])
    || (value[2] !== null && typeof value[2] !== 'boolean')
    || (value[3] !== null && typeof value[3] !== 'boolean')) {
    throw new Error(`${label} is not a conjugation property tuple`);
  }
  return { pos: value[0], type: Number(value[1]), negative: value[2], formal: value[3] };
}

export function parseConjugationRelationKey(line: string): ConjugationRelationKey {
  const value: unknown = JSON.parse(line);
  if (!Array.isArray(value) || value.length !== 11 || !Number.isSafeInteger(value[0])
    || (value[1] !== 'kana' && value[1] !== 'kanji')
    || typeof value[2] !== 'string' || typeof value[3] !== 'string'
    || typeof value[4] !== 'string' || typeof value[5] !== 'string'
    || (value[8] !== null && typeof value[8] !== 'string')
    || !Number.isSafeInteger(value[9])
    || (value[10] !== null && !Number.isSafeInteger(value[10]))) {
    throw new Error('Invalid canonical conjugation relation key');
  }
  return {
    rootSeq: Number(value[0]),
    route: value[1],
    surface: value[2],
    sourceText: value[3],
    sourceForm: value[4],
    sourceReading: value[5],
    first: propertyFromTuple(value[6], 'First property'),
    second: value[7] === null ? null : propertyFromTuple(value[7], 'Second property'),
    intermediate: value[8],
    sourceOrdinal: Number(value[9]),
    sourceCommon: value[10] === null ? null : Number(value[10])
  };
}

export function forwardRelationKey(
  entry: CanonicalEntry,
  emission: ConjugationEmission,
  form: EmissionForm
): ConjugationRelationKey {
  const source = (form.route === 'kanji' ? entry.kanji : entry.kana)
    .find(value => value.text === form.sourceText);
  if (!source) throw new Error(`Root ${entry.seq} is missing source form ${JSON.stringify(form.sourceText)}`);
  return {
    rootSeq: emission.rootSeq,
    route: form.route,
    surface: form.surface,
    sourceText: form.sourceText,
    sourceForm: form.route === 'kanji' ? form.sourceText : source.best ?? form.sourceText,
    sourceReading: form.route === 'kana' ? form.sourceText : source.best ?? form.sourceText,
    first: emission.first,
    second: emission.second,
    intermediate: form.intermediate,
    sourceOrdinal: source.ordinal,
    sourceCommon: source.common
  };
}

export function packedRelationKey(candidate: MorphologyCandidate): ConjugationRelationKey {
  const first = candidate.path[0];
  if (!first) throw new Error('Packed morphology candidate has no first rule');
  const property = (value: typeof first): ConjugationProperty => ({
    pos: value.pos,
    type: value.type,
    negative: value.negative,
    formal: value.formal
  });
  return {
    rootSeq: candidate.rootSeq,
    route: candidate.route,
    surface: candidate.surface,
    sourceText: candidate.sourceText,
    sourceForm: candidate.sourceForm,
    sourceReading: candidate.sourceReading,
    first: property(first),
    second: candidate.path[1] ? property(candidate.path[1]) : null,
    intermediate: candidate.intermediate,
    sourceOrdinal: candidate.ord,
    sourceCommon: candidate.common
  };
}

const RELATION_WRITE_ROWS = 4_096;

async function flushLines(
  stream: ReturnType<typeof createWriteStream>,
  lines: string[]
): Promise<void> {
  if (lines.length === 0) return;
  const chunk = `${lines.join('\n')}\n`;
  lines.length = 0;
  if (!stream.write(chunk)) await once(stream, 'drain');
}

export async function writeForwardRelation(
  entries: AsyncIterable<CanonicalEntry> | Iterable<CanonicalEntry>,
  destination: string,
  options: {
    readonly rootLimit?: number;
    readonly onProgress?: (roots: number, rows: number) => void;
  } = {}
): Promise<{ readonly roots: number; readonly rows: number }> {
  const output = createWriteStream(destination, { flags: 'wx' });
  const lines: string[] = [];
  let roots = 0;
  let rows = 0;
  try {
    for await (const entry of entries) {
      if (options.rootLimit !== undefined && roots >= options.rootLimit) break;
      roots++;
      for (const emission of emitCanonicalConjugations(entry)) {
        for (const form of emission.forms) {
          lines.push(canonicalConjugationRelationKey(forwardRelationKey(entry, emission, form)));
          rows++;
          if (lines.length === RELATION_WRITE_ROWS) await flushLines(output, lines);
        }
      }
      if (roots % 10_000 === 0) options.onProgress?.(roots, rows);
    }
  } finally {
    await flushLines(output, lines);
    const closed = once(output, 'close');
    output.end();
    await closed;
  }
  return { roots, rows };
}

/** Writes an already-complete compiler relation, including chronological edits. */
export async function writeConjugationRelationKeys(
  keys: AsyncIterable<ConjugationRelationKey> | Iterable<ConjugationRelationKey>,
  destination: string
): Promise<{ readonly rows: number }> {
  const output = createWriteStream(destination, { flags: 'wx' });
  const lines: string[] = [];
  let rows = 0;
  try {
    for await (const key of keys) {
      lines.push(canonicalConjugationRelationKey(key));
      rows++;
      if (lines.length === RELATION_WRITE_ROWS) await flushLines(output, lines);
    }
  } finally {
    await flushLines(output, lines);
    const closed = once(output, 'close');
    output.end();
    await closed;
  }
  return { rows };
}

function compareUtf8(left: string, right: string): number {
  return Buffer.compare(Buffer.from(left), Buffer.from(right));
}

async function* groupedSortedLines(lines: AsyncIterable<string>): AsyncGenerator<RelationGroup> {
  let prior: string | null = null;
  let count = 0;
  for await (const line of lines) {
    if (line.length === 0) continue;
    if (prior !== null && compareUtf8(line, prior) < 0) {
      throw new Error('Relation input is not sorted in UTF-8 byte order');
    }
    if (line === prior) {
      count++;
      continue;
    }
    if (prior !== null) yield { line: prior, count };
    prior = line;
    count = 1;
  }
  if (prior !== null) yield { line: prior, count };
}

function updateHash(hash: ReturnType<typeof createHash>, value: string): void {
  const bytes = Buffer.from(value);
  const length = Buffer.allocUnsafe(4);
  length.writeUInt32LE(bytes.length);
  hash.update(length).update(bytes);
}

function addSide(summary: MutableSideSummary, group: RelationGroup): void {
  summary.rows += group.count;
  summary.unique++;
  summary.duplicates += group.count - 1;
  updateHash(summary.hash, group.line);
}

function reviewKey(side: 'omission' | 'packed-only', key: string): string {
  return `${side}\u0000${key}`;
}

function structuralCategory(side: 'omission' | 'packed-only', line: string): string {
  if (side === 'omission') {
    const key = parseConjugationRelationKey(line);
    if (surfaceRoute(key.surface) !== key.route) return 'inactive-route';
  }
  return 'unreviewed';
}

export async function compareSortedRelations(
  forwardLines: AsyncIterable<string>,
  packedLines: AsyncIterable<string>,
  reviewed: readonly ReviewedRelationDelta[] = [],
  exampleLimit = 12
): Promise<ConjugationRelationReport> {
  const ledger = new Map<string, ReviewedRelationDelta>();
  for (const row of reviewed) {
    parseConjugationRelationKey(row.key);
    if (!row.category || !row.provenance || !row.preservedBehavior) {
      throw new Error('Reviewed relation rows require category, provenance and preserved behavior');
    }
    const key = reviewKey(row.side, row.key);
    if (ledger.has(key)) throw new Error(`Duplicate reviewed relation row ${key}`);
    ledger.set(key, row);
  }

  const forward = groupedSortedLines(forwardLines)[Symbol.asyncIterator]();
  const packed = groupedSortedLines(packedLines)[Symbol.asyncIterator]();
  let left = await forward.next();
  let right = await packed.next();
  const forwardSummary: MutableSideSummary = {
    rows: 0, unique: 0, duplicates: 0, hash: createHash('sha256')
  };
  const packedSummary: MutableSideSummary = {
    rows: 0, unique: 0, duplicates: 0, hash: createHash('sha256')
  };
  const differenceHash = createHash('sha256');
  const categories = new Map<string, MutableCategory>();
  const usedReviews = new Set<string>();
  let common = 0;
  let omissions = 0;
  let packedOnly = 0;
  let reviewedDeltas = 0;
  let unreviewedDeltas = 0;

  const addDelta = (side: 'omission' | 'packed-only', line: string): void => {
    const exactKey = reviewKey(side, line);
    const exact = ledger.get(exactKey);
    if (exact) usedReviews.add(exactKey);
    const category = exact?.category ?? structuralCategory(side, line);
    const reviewedRow = exact !== undefined;
    if (reviewedRow) reviewedDeltas++;
    else unreviewedDeltas++;
    const mapKey = `${side}\u0000${reviewedRow ? 'reviewed' : 'reported'}\u0000${category}`;
    const group = categories.get(mapKey) ?? {
      side,
      category,
      reviewed: reviewedRow,
      count: 0,
      hash: createHash('sha256'),
      examples: []
    };
    group.count++;
    updateHash(group.hash, line);
    if (group.examples.length < exampleLimit) group.examples.push(line);
    categories.set(mapKey, group);
    updateHash(differenceHash, JSON.stringify([side, category, line]));
  };

  while (!left.done || !right.done) {
    if (!left.done && (right.done || compareUtf8(left.value.line, right.value.line) < 0)) {
      addSide(forwardSummary, left.value);
      omissions++;
      addDelta('omission', left.value.line);
      left = await forward.next();
    } else if (!right.done && (left.done || compareUtf8(right.value.line, left.value.line) < 0)) {
      addSide(packedSummary, right.value);
      packedOnly++;
      addDelta('packed-only', right.value.line);
      right = await packed.next();
    } else if (!left.done && !right.done) {
      addSide(forwardSummary, left.value);
      addSide(packedSummary, right.value);
      common++;
      left = await forward.next();
      right = await packed.next();
    }
  }

  const unusedReviewedRows = ledger.size - usedReviews.size;
  const finishSide = (value: MutableSideSummary): RelationSideSummary => ({
    rows: value.rows,
    unique: value.unique,
    duplicates: value.duplicates,
    sha256: value.hash.digest('hex')
  });
  const categoryRows = [...categories.values()]
    .sort((a, b) => compareUtf8(a.side, b.side) || compareUtf8(a.category, b.category))
    .map(value => ({
      side: value.side,
      category: value.category,
      reviewed: value.reviewed,
      count: value.count,
      sha256: value.hash.digest('hex'),
      examples: value.examples
    }));
  const forwardResult = finishSide(forwardSummary);
  const packedResult = finishSide(packedSummary);
  return {
    forward: forwardResult,
    packed: packedResult,
    common,
    omissions,
    packedOnly,
    reviewedDeltas,
    unreviewedDeltas,
    unusedReviewedRows,
    differenceSha256: differenceHash.digest('hex'),
    categories: categoryRows,
    passed: forwardResult.duplicates === 0
      && packedResult.duplicates === 0
      && unreviewedDeltas === 0
      && unusedReviewedRows === 0
  };
}

export function relationFileLines(path: string): AsyncIterable<string> {
  return createInterface({ input: createReadStream(path), crlfDelay: Infinity });
}

export async function compareSortedRelationFiles(
  forwardPath: string,
  packedPath: string,
  reviewed: readonly ReviewedRelationDelta[] = [],
  exampleLimit = 12
): Promise<ConjugationRelationReport> {
  return compareSortedRelations(
    relationFileLines(forwardPath),
    relationFileLines(packedPath),
    reviewed,
    exampleLimit
  );
}
