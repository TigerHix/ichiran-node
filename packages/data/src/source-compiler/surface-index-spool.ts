import { randomUUID } from 'node:crypto';
import {
  closeSync,
  createReadStream,
  fstatSync,
  openSync,
  unlinkSync,
  writeFileSync,
  writeSync
} from 'node:fs';
import { join } from 'node:path';
import { createInterface } from 'node:readline';
import { isRootPayloadKanaSurface } from '../browser-pack/root-payload.js';
import { readGeneratedOccurrenceSpool } from './generated-projection-spool.js';
import type { PhysicalTarget } from './conjugation-emissions-physical.js';
import type { CanonicalEntry } from './model.js';

const KANA_DIRECT = 1;
const KANA_MORPHOLOGY = 2;
const KANJI_DIRECT = 4;
const KANJI_MORPHOLOGY = 8;
const MORPHOLOGY_PRESENT = 16;
const OUTPUT_BUFFER_BYTES = 4 * 1024 * 1024;

interface SurfaceFlagRow {
  readonly surface: string;
  readonly bytes: Buffer;
  flags: number;
}

interface SurfaceRunCursor {
  readonly chunk: number;
  readonly iterator: AsyncGenerator<SurfaceFlagRow>;
  readonly row: SurfaceFlagRow;
}

export interface SurfaceIndexTsvSpoolSummary {
  readonly inputRows: number;
  readonly surfaces: number;
  readonly direct: number;
  readonly morphology: number;
  readonly overlap: number;
  readonly chunks: number;
  readonly bytes: number;
}

function surfaceRow(surface: string, flags: number): SurfaceFlagRow {
  if (surface.length === 0) throw new Error('Surface index row has an empty surface');
  if (/[\u0000\t\r\n]/.test(surface)) {
    throw new Error(`Surface index row has an invalid TSV surface ${JSON.stringify(surface)}`);
  }
  const bytes = Buffer.from(surface, 'utf8');
  if (bytes.toString('utf8') !== surface) {
    throw new Error(`Surface index row is not valid Unicode ${JSON.stringify(surface)}`);
  }
  if (!Number.isSafeInteger(flags) || flags < 1 || flags > 31) {
    throw new Error(`Surface index row has invalid flags ${flags}`);
  }
  return { surface, bytes, flags };
}

function compareRows(left: SurfaceFlagRow, right: SurfaceFlagRow): number {
  return Buffer.compare(left.bytes, right.bytes);
}

function chunkLine(row: SurfaceFlagRow): string {
  return `${row.surface}\t${row.flags}\n`;
}

function writeChunk(path: string, rows: SurfaceFlagRow[]): void {
  rows.sort(compareRows);
  const unique: SurfaceFlagRow[] = [];
  for (const row of rows) {
    const prior = unique[unique.length - 1];
    if (prior && prior.bytes.equals(row.bytes)) prior.flags |= row.flags;
    else unique.push(row);
  }
  writeFileSync(path, unique.map(chunkLine).join(''), { flag: 'wx' });
}

async function* readChunk(path: string): AsyncGenerator<SurfaceFlagRow> {
  const input = createReadStream(path, { encoding: 'utf8' });
  const lines = createInterface({ input, crlfDelay: Infinity });
  try {
    for await (const line of lines) {
      const delimiter = line.lastIndexOf('\t');
      if (delimiter <= 0 || line.indexOf('\t') !== delimiter) {
        throw new Error('Invalid surface index run row');
      }
      const flags = Number(line.slice(delimiter + 1));
      yield surfaceRow(line.slice(0, delimiter), flags);
    }
  } finally {
    lines.close();
    input.destroy();
  }
}

function compareCursors(left: SurfaceRunCursor, right: SurfaceRunCursor): number {
  return compareRows(left.row, right.row) || left.chunk - right.chunk;
}

function pushCursor(heap: SurfaceRunCursor[], value: SurfaceRunCursor): void {
  heap.push(value);
  let index = heap.length - 1;
  while (index > 0) {
    const parent = (index - 1) >>> 1;
    if (compareCursors(heap[parent]!, value) <= 0) break;
    heap[index] = heap[parent]!;
    index = parent;
  }
  heap[index] = value;
}

function popCursor(heap: SurfaceRunCursor[]): SurfaceRunCursor | undefined {
  const first = heap[0];
  const last = heap.pop();
  if (!first || !last || heap.length === 0) return first;
  let index = 0;
  while (true) {
    const left = index * 2 + 1;
    if (left >= heap.length) break;
    const right = left + 1;
    const child = right < heap.length
      && compareCursors(heap[right]!, heap[left]!) < 0 ? right : left;
    if (compareCursors(last, heap[child]!) <= 0) break;
    heap[index] = heap[child]!;
    index = child;
  }
  heap[index] = last;
  return first;
}

class SurfaceTsvOutput {
  readonly #fd: number;
  readonly #buffer = Buffer.allocUnsafe(OUTPUT_BUFFER_BYTES);
  #used = 0;

  constructor(fd: number) {
    this.#fd = fd;
  }

  write(row: SurfaceFlagRow): void {
    const suffix = `\t${Number((row.flags & KANA_DIRECT) !== 0)}`
      + `\t${Number((row.flags & KANA_MORPHOLOGY) !== 0)}`
      + `\t${Number((row.flags & KANJI_DIRECT) !== 0)}`
      + `\t${Number((row.flags & KANJI_MORPHOLOGY) !== 0)}\n`;
    const bytes = row.bytes.byteLength + Buffer.byteLength(suffix);
    if (bytes > this.#buffer.byteLength) {
      this.flush();
      this.#write(row.bytes);
      this.#write(Buffer.from(suffix));
      return;
    }
    if (this.#used + bytes > this.#buffer.byteLength) this.flush();
    row.bytes.copy(this.#buffer, this.#used);
    this.#used += row.bytes.byteLength;
    this.#used += this.#buffer.write(suffix, this.#used, 'utf8');
  }

  flush(): void {
    if (this.#used === 0) return;
    this.#write(this.#buffer.subarray(0, this.#used));
    this.#used = 0;
  }

  #write(bytes: Buffer): void {
    let offset = 0;
    while (offset < bytes.byteLength) {
      const written = writeSync(this.#fd, bytes, offset, bytes.byteLength - offset);
      if (written === 0) throw new Error('Surface index TSV write made no progress');
      offset += written;
    }
  }
}

/**
 * Write the exact surface-index TSV without retaining the ~8.4 million
 * surfaces. Runs and the final merge are ordered by raw UTF-8 bytes, matching
 * the existing Rust surface-index compiler contract.
 */
export async function writeBoundedSurfaceIndexTsv(input: {
  readonly entries: readonly CanonicalEntry[];
  readonly physicalTargets: readonly PhysicalTarget[];
  readonly occurrencesPath: string;
  readonly temporaryDirectory: string;
  readonly destination: string;
  readonly maxChunkRows?: number;
}): Promise<SurfaceIndexTsvSpoolSummary> {
  const maxChunkRows = input.maxChunkRows ?? 100_000;
  if (!Number.isSafeInteger(maxChunkRows) || maxChunkRows < 1) {
    throw new Error('Surface index chunk size must be a positive integer');
  }
  const prefix = `surface-index-${process.pid}-${randomUUID()}`;
  const chunkPaths: string[] = [];
  const chunk: SurfaceFlagRow[] = [];
  const iterators: AsyncGenerator<SurfaceFlagRow>[] = [];
  let inputRows = 0;
  const add = (surface: string, flags: number): void => {
    chunk.push(surfaceRow(surface, flags));
    inputRows++;
    if (chunk.length === maxChunkRows) flush();
  };
  const flush = (): void => {
    if (chunk.length === 0) return;
    const path = join(input.temporaryDirectory, `${prefix}-${chunkPaths.length}.tsv`);
    writeChunk(path, chunk);
    chunkPaths.push(path);
    chunk.length = 0;
  };

  let outputFd: number | null = null;
  let outputCreated = false;
  try {
    for (const entry of input.entries) {
      for (const form of entry.kana) add(form.text, KANA_DIRECT);
      for (const form of entry.kanji) add(form.text, KANJI_DIRECT);
    }
    for (const occurrence of readGeneratedOccurrenceSpool(input.occurrencesPath)) {
      add(occurrence.surface, MORPHOLOGY_PRESENT);
    }
    for (const target of input.physicalTargets) {
      for (const text of target.kana) add(text, KANA_MORPHOLOGY);
      for (const text of target.kanji) add(text, KANJI_MORPHOLOGY);
    }
    flush();

    const heap: SurfaceRunCursor[] = [];
    for (const [index, path] of chunkPaths.entries()) {
      const iterator = readChunk(path);
      iterators.push(iterator);
      const first = await iterator.next();
      if (!first.done) pushCursor(heap, { chunk: index, iterator, row: first.value });
    }
    outputFd = openSync(input.destination, 'wx');
    outputCreated = true;
    const output = new SurfaceTsvOutput(outputFd);
    let current: SurfaceFlagRow | null = null;
    let surfaces = 0;
    let direct = 0;
    let morphology = 0;
    let overlap = 0;
    const emit = (): void => {
      if (current === null) return;
      const hasMorphology = (current.flags & MORPHOLOGY_PRESENT) !== 0;
      const physicalRoutes = current.flags & (KANA_MORPHOLOGY | KANJI_MORPHOLOGY);
      const fallbackRoute = isRootPayloadKanaSurface(current.surface)
        ? KANA_MORPHOLOGY : KANJI_MORPHOLOGY;
      const declaredFlags = (current.flags & (KANA_DIRECT | KANJI_DIRECT))
        | (hasMorphology ? physicalRoutes || fallbackRoute : 0);
      const routeMask = isRootPayloadKanaSurface(current.surface)
        ? KANA_DIRECT | KANA_MORPHOLOGY
        : KANJI_DIRECT | KANJI_MORPHOLOGY;
      const outputFlags = declaredFlags & routeMask;
      const hasDirectFact = (current.flags & (KANA_DIRECT | KANJI_DIRECT)) !== 0;
      if (!hasDirectFact && !hasMorphology) return;
      output.write({ ...current, flags: outputFlags });
      const hasDirect = (outputFlags & (KANA_DIRECT | KANJI_DIRECT)) !== 0;
      const selectedMorphology = (outputFlags & (KANA_MORPHOLOGY | KANJI_MORPHOLOGY)) !== 0;
      surfaces++;
      direct += Number(hasDirect);
      morphology += Number(selectedMorphology);
      overlap += Number(hasDirect && selectedMorphology);
    };
    while (heap.length > 0) {
      const cursor = popCursor(heap)!;
      if (current && current.bytes.equals(cursor.row.bytes)) current.flags |= cursor.row.flags;
      else {
        emit();
        current = cursor.row;
      }
      const next = await cursor.iterator.next();
      if (!next.done) {
        pushCursor(heap, { chunk: cursor.chunk, iterator: cursor.iterator, row: next.value });
      }
    }
    emit();
    output.flush();
    const bytes = fstatSync(outputFd).size;
    closeSync(outputFd);
    outputFd = null;
    return {
      inputRows,
      surfaces,
      direct,
      morphology,
      overlap,
      chunks: chunkPaths.length,
      bytes
    };
  } catch (error) {
    if (outputFd !== null) closeSync(outputFd);
    if (outputCreated) unlinkSync(input.destination);
    throw error;
  } finally {
    await Promise.all(iterators.map(iterator => iterator.return(undefined)));
    for (const path of chunkPaths) unlinkSync(path);
  }
}
