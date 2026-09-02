import {
  closeSync,
  fstatSync,
  openSync,
  readSync,
  unlinkSync,
  writeSync
} from 'node:fs';
import { TextDecoder } from 'node:util';

export interface GeneratedPathSpoolRow {
  readonly ordinal: number;
  readonly rootSeq: number;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
}

export interface GeneratedOccurrenceSpoolRow {
  readonly pathOrdinal: number;
  readonly precedence: number;
  readonly firstRule: number;
  readonly secondRule: number | null;
  readonly route: 'kana' | 'kanji';
  readonly kind: 'emission' | 'patch';
  readonly installed: boolean;
  readonly surface: string;
  /** Opposite-route text stored on the same physical target, when declared. */
  readonly physicalCounterpart: string | null;
}

export interface GeneratedProjectionSpoolSummary {
  readonly paths: number;
  readonly occurrences: number;
  readonly installedOccurrences: number;
}

const HEADER_BYTES = 16;
const VERSION = 2;
const PATH_BYTES = 20;
const OCCURRENCE_PREFIX_BYTES = 21;
const NULL_U32 = 0xffff_ffff;
const NULL_U16 = 0xffff;
const BUFFER_BYTES = 4 * 1024 * 1024;
const PATH_MAGIC = Buffer.from('IGPATH01');
const OCCURRENCE_MAGIC = Buffer.from('IGOCCR01');
const utf8 = new TextDecoder('utf-8', { fatal: true });

function assertUint(value: number, maximum: number, label: string): void {
  if (!Number.isSafeInteger(value) || value < 0 || value > maximum) {
    throw new Error(`${label} is outside 0..${maximum}`);
  }
}

function writeAll(fd: number, bytes: Buffer, position?: number): void {
  let offset = 0;
  while (offset < bytes.byteLength) {
    const written = writeSync(
      fd,
      bytes,
      offset,
      bytes.byteLength - offset,
      position === undefined ? undefined : position + offset
    );
    if (written === 0) throw new Error('Generated projection spool write made no progress');
    offset += written;
  }
}

function readExact(fd: number, bytes: Buffer, position: number, label: string): void {
  let offset = 0;
  while (offset < bytes.byteLength) {
    const read = readSync(fd, bytes, offset, bytes.byteLength - offset, position + offset);
    if (read === 0) throw new Error(`Truncated ${label}`);
    offset += read;
  }
}

function header(magic: Buffer, recordBytes: number, rows: number): Buffer {
  assertUint(rows, NULL_U32, 'Spool row count');
  const value = Buffer.alloc(HEADER_BYTES);
  magic.copy(value, 0);
  value.writeUInt16LE(VERSION, 8);
  value.writeUInt16LE(recordBytes, 10);
  value.writeUInt32LE(rows, 12);
  return value;
}

function readHeader(
  fd: number,
  magic: Buffer,
  recordBytes: number,
  label: string
): number {
  const value = Buffer.alloc(HEADER_BYTES);
  readExact(fd, value, 0, `${label} header`);
  if (!value.subarray(0, 8).equals(magic)) throw new Error(`Invalid ${label} magic`);
  if (value.readUInt16LE(8) !== VERSION) throw new Error(`Unsupported ${label} version`);
  if (value.readUInt16LE(10) !== recordBytes) throw new Error(`Invalid ${label} record size`);
  return value.readUInt32LE(12);
}

function occurrenceByteLengths(row: GeneratedOccurrenceSpoolRow): {
  readonly surface: number;
  readonly counterpart: number | null;
} {
  assertUint(row.pathOrdinal, NULL_U32 - 1, 'Generated occurrence path ordinal');
  assertUint(row.precedence, NULL_U32 - 1, 'Generated occurrence precedence');
  assertUint(row.firstRule, NULL_U16 - 1, 'Generated occurrence first rule');
  if (row.secondRule !== null) {
    assertUint(row.secondRule, NULL_U16 - 1, 'Generated occurrence second rule');
  }
  const surfaceBytes = Buffer.byteLength(row.surface, 'utf8');
  assertUint(surfaceBytes, NULL_U32, 'Generated occurrence surface byte length');
  const counterpartBytes = row.physicalCounterpart === null
    ? null : Buffer.byteLength(row.physicalCounterpart, 'utf8');
  if (counterpartBytes !== null) {
    assertUint(counterpartBytes, NULL_U32 - 1, 'Generated occurrence counterpart byte length');
  }
  return { surface: surfaceBytes, counterpart: counterpartBytes };
}

function writeOccurrenceRow(
  output: BufferedOutput,
  row: GeneratedOccurrenceSpoolRow
): void {
  const lengths = occurrenceByteLengths(row);
  const value = output.reserve(
    OCCURRENCE_PREFIX_BYTES + lengths.surface + (lengths.counterpart ?? 0)
  );
  value.writeUInt32LE(row.pathOrdinal, 0);
  value.writeUInt32LE(row.precedence, 4);
  value.writeUInt32LE(lengths.surface, 8);
  value.writeUInt32LE(lengths.counterpart ?? NULL_U32, 12);
  value.writeUInt16LE(row.firstRule, 16);
  value.writeUInt16LE(row.secondRule ?? NULL_U16, 18);
  value[20] = (row.route === 'kanji' ? 1 : 0)
    | (row.kind === 'patch' ? 2 : 0)
    | (row.installed ? 4 : 0);
  value.write(row.surface, OCCURRENCE_PREFIX_BYTES, lengths.surface, 'utf8');
  if (row.physicalCounterpart !== null) {
    value.write(
      row.physicalCounterpart,
      OCCURRENCE_PREFIX_BYTES + lengths.surface,
      lengths.counterpart!,
      'utf8'
    );
  }
}

class BufferedOutput {
  readonly #fd: number;
  readonly #buffer = Buffer.allocUnsafe(BUFFER_BYTES);
  #used = 0;

  constructor(fd: number) {
    this.#fd = fd;
  }

  reserve(bytes: number): Buffer {
    if (bytes > this.#buffer.byteLength) {
      throw new Error(`Generated projection spool row exceeds ${this.#buffer.byteLength} bytes`);
    }
    if (this.#used + bytes > this.#buffer.byteLength) this.flush();
    const target = this.#buffer.subarray(this.#used, this.#used + bytes);
    this.#used += bytes;
    return target;
  }

  flush(): void {
    if (this.#used === 0) return;
    writeAll(this.#fd, this.#buffer.subarray(0, this.#used));
    this.#used = 0;
  }
}

class BufferedInput {
  readonly #fd: number;
  readonly #fileBytes: number;
  readonly #buffer = Buffer.allocUnsafe(BUFFER_BYTES);
  #bufferOffset = 0;
  #bufferBytes = 0;
  #nextFileOffset = HEADER_BYTES;
  #consumed = HEADER_BYTES;

  constructor(fd: number, fileBytes: number) {
    this.#fd = fd;
    this.#fileBytes = fileBytes;
  }

  read(bytes: number, label: string): Buffer {
    if (bytes < 0 || bytes > this.#fileBytes - this.#consumed) {
      throw new Error(`Truncated ${label}`);
    }
    if (bytes === 0) return Buffer.alloc(0);
    if (bytes <= this.#bufferBytes - this.#bufferOffset) {
      const value = this.#buffer.subarray(this.#bufferOffset, this.#bufferOffset + bytes);
      this.#bufferOffset += bytes;
      this.#consumed += bytes;
      return value;
    }
    const value = Buffer.allocUnsafe(bytes);
    let written = 0;
    while (written < bytes) {
      if (this.#bufferOffset === this.#bufferBytes) this.#fill();
      const available = this.#bufferBytes - this.#bufferOffset;
      if (available === 0) throw new Error(`Truncated ${label}`);
      const take = Math.min(available, bytes - written);
      this.#buffer.copy(value, written, this.#bufferOffset, this.#bufferOffset + take);
      this.#bufferOffset += take;
      written += take;
    }
    this.#consumed += bytes;
    return value;
  }

  get remaining(): number {
    return this.#fileBytes - this.#consumed;
  }

  #fill(): void {
    this.#bufferOffset = 0;
    this.#bufferBytes = readSync(
      this.#fd,
      this.#buffer,
      0,
      this.#buffer.byteLength,
      this.#nextFileOffset
    );
    this.#nextFileOffset += this.#bufferBytes;
  }
}

/**
 * Exact disk boundary for the two high-volume generated projections. It owns
 * only semantic path joins and lookup occurrences; sorting and pack encoding
 * remain in their existing compiler owners.
 */
export class GeneratedProjectionSpoolWriter {
  readonly #pathsPath: string;
  readonly #occurrencesPath: string;
  readonly #pathsFd: number;
  readonly #occurrencesFd: number;
  readonly #paths: BufferedOutput;
  readonly #occurrences: BufferedOutput;
  #pathRows = 0;
  #occurrenceRows = 0;
  #installedOccurrenceRows = 0;
  #closed = false;

  constructor(pathsPath: string, occurrencesPath: string) {
    this.#pathsPath = pathsPath;
    this.#occurrencesPath = occurrencesPath;
    this.#pathsFd = openSync(pathsPath, 'wx');
    try {
      this.#occurrencesFd = openSync(occurrencesPath, 'wx');
    } catch (error) {
      closeSync(this.#pathsFd);
      unlinkSync(pathsPath);
      throw error;
    }
    writeAll(this.#pathsFd, header(PATH_MAGIC, PATH_BYTES, 0));
    writeAll(this.#occurrencesFd, header(OCCURRENCE_MAGIC, 0, 0));
    this.#paths = new BufferedOutput(this.#pathsFd);
    this.#occurrences = new BufferedOutput(this.#occurrencesFd);
  }

  writePath(row: GeneratedPathSpoolRow): void {
    this.#assertOpen();
    if (row.ordinal !== this.#pathRows) {
      throw new Error(`Generated path ordinal ${row.ordinal} is not dense ${this.#pathRows}`);
    }
    assertUint(row.rootSeq, NULL_U32 - 1, 'Generated path root sequence');
    assertUint(row.firstAlias, NULL_U16 - 1, 'Generated path first alias');
    if (row.secondAlias !== null) {
      assertUint(row.secondAlias, NULL_U16 - 1, 'Generated path second alias');
    }
    assertUint(row.targetSeq, NULL_U32 - 1, 'Generated path target sequence');
    if (row.viaTargetSeq !== null) {
      assertUint(row.viaTargetSeq, NULL_U32 - 1, 'Generated path via target sequence');
    }
    const value = this.#paths.reserve(PATH_BYTES);
    value.writeUInt32LE(row.ordinal, 0);
    value.writeUInt32LE(row.rootSeq, 4);
    value.writeUInt16LE(row.firstAlias, 8);
    value.writeUInt16LE(row.secondAlias ?? NULL_U16, 10);
    value.writeUInt32LE(row.targetSeq, 12);
    value.writeUInt32LE(row.viaTargetSeq ?? NULL_U32, 16);
    this.#pathRows++;
  }

  writeOccurrence(row: GeneratedOccurrenceSpoolRow): void {
    this.#assertOpen();
    if (row.pathOrdinal >= this.#pathRows) {
      throw new Error(`Generated occurrence references unwritten path ${row.pathOrdinal}`);
    }
    writeOccurrenceRow(this.#occurrences, row);
    this.#occurrenceRows++;
    this.#installedOccurrenceRows += Number(row.installed);
  }

  close(): GeneratedProjectionSpoolSummary {
    this.#assertOpen();
    this.#paths.flush();
    this.#occurrences.flush();
    writeAll(this.#pathsFd, header(PATH_MAGIC, PATH_BYTES, this.#pathRows), 0);
    writeAll(
      this.#occurrencesFd,
      header(OCCURRENCE_MAGIC, 0, this.#occurrenceRows),
      0
    );
    closeSync(this.#pathsFd);
    closeSync(this.#occurrencesFd);
    this.#closed = true;
    return {
      paths: this.#pathRows,
      occurrences: this.#occurrenceRows,
      installedOccurrences: this.#installedOccurrenceRows
    };
  }

  abort(): void {
    if (this.#closed) return;
    closeSync(this.#pathsFd);
    closeSync(this.#occurrencesFd);
    unlinkSync(this.#pathsPath);
    unlinkSync(this.#occurrencesPath);
    this.#closed = true;
  }

  #assertOpen(): void {
    if (this.#closed) throw new Error('Generated projection spool is closed');
  }
}

/** Write one deterministic sorted-run file using the occurrence spool schema. */
export function writeGeneratedOccurrenceSpool(
  path: string,
  rows: Iterable<GeneratedOccurrenceSpoolRow>
): number {
  const fd = openSync(path, 'wx');
  let closed = false;
  try {
    writeAll(fd, header(OCCURRENCE_MAGIC, 0, 0));
    const output = new BufferedOutput(fd);
    let count = 0;
    for (const row of rows) {
      writeOccurrenceRow(output, row);
      count++;
    }
    output.flush();
    writeAll(fd, header(OCCURRENCE_MAGIC, 0, count), 0);
    closeSync(fd);
    closed = true;
    return count;
  } catch (error) {
    if (!closed) closeSync(fd);
    unlinkSync(path);
    throw error;
  }
}

export function generatedPathSpoolRows(path: string): number {
  const fd = openSync(path, 'r');
  try {
    const rows = readHeader(fd, PATH_MAGIC, PATH_BYTES, 'generated path spool');
    const expectedBytes = HEADER_BYTES + rows * PATH_BYTES;
    const fileBytes = fstatSync(fd).size;
    if (fileBytes !== expectedBytes) {
      throw new Error(`Generated path spool has ${fileBytes}/${expectedBytes} bytes`);
    }
    return rows;
  } finally {
    closeSync(fd);
  }
}

export function* readGeneratedPathSpool(path: string): Generator<GeneratedPathSpoolRow> {
  const fd = openSync(path, 'r');
  try {
    const rows = readHeader(fd, PATH_MAGIC, PATH_BYTES, 'generated path spool');
    const fileBytes = fstatSync(fd).size;
    const expectedBytes = HEADER_BYTES + rows * PATH_BYTES;
    if (fileBytes !== expectedBytes) {
      throw new Error(`Generated path spool has ${fileBytes}/${expectedBytes} bytes`);
    }
    const input = new BufferedInput(fd, fileBytes);
    for (let ordinal = 0; ordinal < rows; ordinal++) {
      const value = input.read(PATH_BYTES, `generated path row ${ordinal}`);
      const storedOrdinal = value.readUInt32LE(0);
      if (storedOrdinal !== ordinal) {
        throw new Error(`Generated path ordinal ${storedOrdinal} is not dense ${ordinal}`);
      }
      const secondAlias = value.readUInt16LE(10);
      const viaTargetSeq = value.readUInt32LE(16);
      const firstAlias = value.readUInt16LE(8);
      const targetSeq = value.readUInt32LE(12);
      if (firstAlias === NULL_U16) {
        throw new Error(`Generated path row ${ordinal} has a null first alias`);
      }
      if (targetSeq === NULL_U32) {
        throw new Error(`Generated path row ${ordinal} has a null target`);
      }
      yield {
        ordinal: storedOrdinal,
        rootSeq: value.readUInt32LE(4),
        firstAlias,
        secondAlias: secondAlias === NULL_U16 ? null : secondAlias,
        targetSeq,
        viaTargetSeq: viaTargetSeq === NULL_U32 ? null : viaTargetSeq
      };
    }
  } finally {
    closeSync(fd);
  }
}

export function* readGeneratedOccurrenceSpool(
  path: string
): Generator<GeneratedOccurrenceSpoolRow> {
  const fd = openSync(path, 'r');
  try {
    const rows = readHeader(fd, OCCURRENCE_MAGIC, 0, 'generated occurrence spool');
    const input = new BufferedInput(fd, fstatSync(fd).size);
    for (let index = 0; index < rows; index++) {
      const prefix = input.read(OCCURRENCE_PREFIX_BYTES, `generated occurrence row ${index}`);
      const surfaceBytes = prefix.readUInt32LE(8);
      const counterpartBytes = prefix.readUInt32LE(12);
      const secondRule = prefix.readUInt16LE(18);
      const firstRule = prefix.readUInt16LE(16);
      const flags = prefix[20]!;
      const pathOrdinal = prefix.readUInt32LE(0);
      const precedence = prefix.readUInt32LE(4);
      if ((flags & ~7) !== 0) throw new Error(`Generated occurrence row ${index} has invalid flags`);
      if (firstRule === NULL_U16) {
        throw new Error(`Generated occurrence row ${index} has a null first rule`);
      }
      const bytes = input.read(surfaceBytes, `generated occurrence surface ${index}`);
      let surface: string;
      try {
        surface = utf8.decode(bytes);
      } catch {
        throw new Error(`Generated occurrence surface ${index} is not UTF-8`);
      }
      let physicalCounterpart: string | null = null;
      if (counterpartBytes !== NULL_U32) {
        try {
          physicalCounterpart = utf8.decode(input.read(
            counterpartBytes,
            `generated occurrence counterpart ${index}`
          ));
        } catch {
          throw new Error(`Generated occurrence counterpart ${index} is not UTF-8`);
        }
      }
      yield {
        pathOrdinal,
        precedence,
        firstRule,
        secondRule: secondRule === NULL_U16 ? null : secondRule,
        route: (flags & 1) === 0 ? 'kana' : 'kanji',
        kind: (flags & 2) === 0 ? 'emission' : 'patch',
        installed: (flags & 4) !== 0,
        surface,
        physicalCounterpart
      };
    }
    if (input.remaining !== 0) {
      throw new Error(`Generated occurrence spool has ${input.remaining} trailing bytes`);
    }
  } finally {
    closeSync(fd);
  }
}
