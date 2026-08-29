import { gzipSync } from 'node:zlib';
import type postgres from 'postgres';

const MAGIC = 'ICHIDETL';
const FORMAT_VERSION = 1;
const HEADER_BYTES = 96;
const ENTRY_BYTES = 8;
const BLOCK_BYTES = 24;
const ALIGNMENT = 8;
const LITTLE_ENDIAN = true;

const TOTAL_BYTES_OFFSET = 16;
const HEADER_CHECKSUM_OFFSET = 20;
const ENTRY_COUNT_OFFSET = 24;
const BLOCK_COUNT_OFFSET = 28;
const ENTRY_STRIDE_OFFSET = 32;
const BLOCK_STRIDE_OFFSET = 34;
const TARGET_BLOCK_BYTES_OFFSET = 36;
const ENTRIES_OFFSET = 40;
const BLOCKS_OFFSET = 44;
const DATA_OFFSET = 48;
const ENTRIES_CHECKSUM_OFFSET = 52;
const BLOCKS_CHECKSUM_OFFSET = 56;

const PROPERTY_TAGS = [
  'dial', 'field', 'misc', 'pos', 's_inf', 'stagk', 'stagr'
] as const;
const PROPERTY_TAG_IDS = new Map<string, number>(
  PROPERTY_TAGS.map((tag, index) => [tag, index])
);

const CRC32_POLYNOMIAL = 0xedb8_8320;
const CRC32_TABLE = new Uint32Array(256);
for (let value = 0; value < CRC32_TABLE.length; value++) {
  let checksum = value;
  for (let bit = 0; bit < 8; bit++) {
    checksum = (checksum & 1) === 1
      ? CRC32_POLYNOMIAL ^ (checksum >>> 1)
      : checksum >>> 1;
  }
  CRC32_TABLE[value] = checksum >>> 0;
}

const UTF8_ENCODER = new TextEncoder();

export type DetailPropertyTag = typeof PROPERTY_TAGS[number];

export interface DetailGlossSource {
  readonly ord: number;
  readonly text: string;
}

export interface DetailPropertySource {
  readonly tag: DetailPropertyTag;
  readonly ord: number;
  readonly text: string;
}

export interface DetailSenseSource {
  readonly ord: number;
  readonly glosses: readonly DetailGlossSource[];
  readonly properties: readonly DetailPropertySource[];
}

export interface DetailEntrySource {
  readonly seq: number;
  readonly senses: readonly DetailSenseSource[];
}

export interface DetailStoreBuild {
  readonly bytes: Uint8Array;
  readonly stats: {
    readonly entryCount: number;
    readonly senseCount: number;
    readonly glossCount: number;
    readonly propertyCount: number;
    readonly blockCount: number;
    readonly indexBytes: number;
    readonly uncompressedRecordBytes: number;
    readonly compressedDataBytes: number;
    readonly totalBytes: number;
    readonly largestRecordBytes: number;
    readonly largestBlockBytes: number;
  };
}

export class DetailStoreEncodingError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'DetailStoreEncodingError';
  }
}

interface SenseRow {
  id: number;
  seq: number;
  ord: number;
}

interface GlossRow {
  senseId: number;
  ord: number;
  text: string;
}

interface PropertyRow {
  senseId: number;
  tag: string;
  ord: number;
  text: string;
}

interface EncodedBlock {
  readonly firstEntry: number;
  readonly entryOffsets: readonly number[];
  readonly uncompressed: Uint8Array;
  readonly compressed: Uint8Array;
}

class ByteWriter {
  readonly #bytes: number[] = [];

  get byteLength(): number {
    return this.#bytes.length;
  }

  uint(value: number, label: string): void {
    assertUint32(value, label);
    do {
      let byte = value & 0x7f;
      value = Math.floor(value / 128);
      if (value !== 0) byte |= 0x80;
      this.#bytes.push(byte);
    } while (value !== 0);
  }

  byte(value: number, label: string): void {
    if (!Number.isSafeInteger(value) || value < 0 || value > 0xff) {
      throw new DetailStoreEncodingError(`${label} must fit uint8`);
    }
    this.#bytes.push(value);
  }

  text(value: string): void {
    const encoded = UTF8_ENCODER.encode(value);
    this.uint(encoded.byteLength, 'UTF-8 byte length');
    for (const byte of encoded) this.#bytes.push(byte);
  }

  finish(): Uint8Array {
    return Uint8Array.from(this.#bytes);
  }
}

function crc32(bytes: Uint8Array): number {
  let checksum = 0xffff_ffff;
  for (const byte of bytes) {
    checksum = CRC32_TABLE[(checksum ^ byte) & 0xff]! ^ (checksum >>> 8);
  }
  return (checksum ^ 0xffff_ffff) >>> 0;
}

function align(value: number): number {
  return Math.ceil(value / ALIGNMENT) * ALIGNMENT;
}

function assertUint32(value: number, label: string): void {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff_ffff) {
    throw new DetailStoreEncodingError(`${label} must fit uint32`);
  }
}

function compareText(left: string, right: string): number {
  if (left === right) return 0;
  const leftBytes = UTF8_ENCODER.encode(left);
  const rightBytes = UTF8_ENCODER.encode(right);
  const shared = Math.min(leftBytes.byteLength, rightBytes.byteLength);
  for (let index = 0; index < shared; index++) {
    const difference = leftBytes[index]! - rightBytes[index]!;
    if (difference !== 0) return difference;
  }
  return leftBytes.byteLength - rightBytes.byteLength;
}

function encodeEntry(entry: DetailEntrySource): Uint8Array {
  const writer = new ByteWriter();
  writer.uint(entry.seq, 'Entry sequence');
  writer.uint(entry.senses.length, 'Sense count');
  let previousSenseOrdinal = -1;
  for (const sense of entry.senses) {
    if (sense.ord <= previousSenseOrdinal) {
      throw new DetailStoreEncodingError(`Entry ${entry.seq} senses are not ordered`);
    }
    previousSenseOrdinal = sense.ord;
    writer.uint(sense.ord, 'Sense ordinal');
    writer.uint(sense.glosses.length, 'Gloss count');
    let previousGlossOrdinal = -1;
    for (const gloss of sense.glosses) {
      if (gloss.ord <= previousGlossOrdinal) {
        throw new DetailStoreEncodingError(`Entry ${entry.seq} glosses are not ordered`);
      }
      previousGlossOrdinal = gloss.ord;
      writer.uint(gloss.ord, 'Gloss ordinal');
      writer.text(gloss.text);
    }
    writer.uint(sense.properties.length, 'Property count');
    let previousProperty: DetailPropertySource | null = null;
    for (const property of sense.properties) {
      const tagId = PROPERTY_TAG_IDS.get(property.tag);
      if (tagId === undefined) {
        throw new DetailStoreEncodingError(`Unknown sense-property tag ${property.tag}`);
      }
      if (previousProperty) {
        const tagOrder = compareText(previousProperty.tag, property.tag);
        if (
          tagOrder > 0
          || (tagOrder === 0 && previousProperty.ord > property.ord)
          || (
            tagOrder === 0
            && previousProperty.ord === property.ord
            && compareText(previousProperty.text, property.text) > 0
          )
        ) {
          throw new DetailStoreEncodingError(`Entry ${entry.seq} properties are not ordered`);
        }
      }
      previousProperty = property;
      writer.byte(tagId, 'Property tag ID');
      writer.uint(property.ord, 'Property ordinal');
      writer.text(property.text);
    }
  }
  return writer.finish();
}

function concatBytes(parts: readonly Uint8Array[], byteLength: number): Uint8Array {
  const output = new Uint8Array(byteLength);
  let offset = 0;
  for (const part of parts) {
    output.set(part, offset);
    offset += part.byteLength;
  }
  return output;
}

function makeBlock(records: readonly Uint8Array[], firstEntry: number): EncodedBlock {
  const entryOffsets: number[] = [];
  const parts: Uint8Array[] = [];
  let byteLength = 0;
  for (const record of records) {
    entryOffsets.push(byteLength);
    const prefix = new Uint8Array(4);
    new DataView(prefix.buffer).setUint32(0, record.byteLength, LITTLE_ENDIAN);
    parts.push(prefix, record);
    byteLength += 4 + record.byteLength;
  }
  const uncompressed = concatBytes(parts, byteLength);
  const compressed = new Uint8Array(gzipSync(uncompressed, { level: 9 }));
  return { firstEntry, entryOffsets, uncompressed, compressed };
}

export function buildDetailStore(
  sourceEntries: readonly DetailEntrySource[],
  options: { readonly targetBlockBytes?: number } = {}
): DetailStoreBuild {
  const targetBlockBytes = options.targetBlockBytes ?? 64 * 1024;
  if (!Number.isSafeInteger(targetBlockBytes) || targetBlockBytes < 1024) {
    throw new DetailStoreEncodingError('Target block size must be an integer of at least 1024 bytes');
  }
  if (sourceEntries.length === 0) {
    throw new DetailStoreEncodingError('Detail store requires at least one root entry');
  }

  const entries = sourceEntries.map((entry) => ({
    ...entry,
    senses: entry.senses.map((sense) => ({
      ...sense,
      glosses: [...sense.glosses],
      properties: [...sense.properties]
    }))
  })).sort((left, right) => left.seq - right.seq);
  for (let index = 1; index < entries.length; index++) {
    if (entries[index - 1]!.seq === entries[index]!.seq) {
      throw new DetailStoreEncodingError(`Duplicate root sequence ${entries[index]!.seq}`);
    }
  }

  const records = entries.map(encodeEntry);
  const blocks: EncodedBlock[] = [];
  let blockRecords: Uint8Array[] = [];
  let blockBytes = 0;
  let firstEntry = 0;
  for (const record of records) {
    const storedBytes = 4 + record.byteLength;
    if (blockRecords.length > 0 && blockBytes + storedBytes > targetBlockBytes) {
      blocks.push(makeBlock(blockRecords, firstEntry));
      firstEntry += blockRecords.length;
      blockRecords = [];
      blockBytes = 0;
    }
    blockRecords.push(record);
    blockBytes += storedBytes;
  }
  if (blockRecords.length > 0) blocks.push(makeBlock(blockRecords, firstEntry));

  const entriesOffset = HEADER_BYTES;
  const entryTableBytes = entries.length * ENTRY_BYTES;
  const blocksOffset = align(entriesOffset + entryTableBytes);
  const blockTableBytes = blocks.length * BLOCK_BYTES;
  const dataOffset = align(blocksOffset + blockTableBytes);
  const compressedDataBytes = blocks.reduce((sum, block) => sum + block.compressed.byteLength, 0);
  const totalBytes = dataOffset + compressedDataBytes;
  for (const [value, label] of [
    [entries.length, 'Entry count'], [blocks.length, 'Block count'],
    [targetBlockBytes, 'Target block size'], [entriesOffset, 'Entries offset'],
    [blocksOffset, 'Blocks offset'], [dataOffset, 'Data offset'],
    [totalBytes, 'Total bytes']
  ] as const) assertUint32(value, label);

  const output = new Uint8Array(totalBytes);
  const view = new DataView(output.buffer);
  for (let index = 0; index < MAGIC.length; index++) output[index] = MAGIC.charCodeAt(index);
  view.setUint16(8, FORMAT_VERSION, LITTLE_ENDIAN);
  view.setUint16(10, HEADER_BYTES, LITTLE_ENDIAN);
  view.setUint32(12, 0, LITTLE_ENDIAN);
  view.setUint32(TOTAL_BYTES_OFFSET, totalBytes, LITTLE_ENDIAN);
  view.setUint32(ENTRY_COUNT_OFFSET, entries.length, LITTLE_ENDIAN);
  view.setUint32(BLOCK_COUNT_OFFSET, blocks.length, LITTLE_ENDIAN);
  view.setUint16(ENTRY_STRIDE_OFFSET, ENTRY_BYTES, LITTLE_ENDIAN);
  view.setUint16(BLOCK_STRIDE_OFFSET, BLOCK_BYTES, LITTLE_ENDIAN);
  view.setUint32(TARGET_BLOCK_BYTES_OFFSET, targetBlockBytes, LITTLE_ENDIAN);
  view.setUint32(ENTRIES_OFFSET, entriesOffset, LITTLE_ENDIAN);
  view.setUint32(BLOCKS_OFFSET, blocksOffset, LITTLE_ENDIAN);
  view.setUint32(DATA_OFFSET, dataOffset, LITTLE_ENDIAN);

  let entryIndex = 0;
  let compressedOffset = 0;
  for (let blockIndex = 0; blockIndex < blocks.length; blockIndex++) {
    const block = blocks[blockIndex]!;
    for (const recordOffset of block.entryOffsets) {
      const offset = entriesOffset + entryIndex * ENTRY_BYTES;
      view.setUint32(offset, blockIndex, LITTLE_ENDIAN);
      view.setUint32(offset + 4, recordOffset, LITTLE_ENDIAN);
      entryIndex++;
    }
    const offset = blocksOffset + blockIndex * BLOCK_BYTES;
    view.setUint32(offset, compressedOffset, LITTLE_ENDIAN);
    view.setUint32(offset + 4, block.compressed.byteLength, LITTLE_ENDIAN);
    view.setUint32(offset + 8, block.uncompressed.byteLength, LITTLE_ENDIAN);
    view.setUint32(offset + 12, crc32(block.uncompressed), LITTLE_ENDIAN);
    view.setUint32(offset + 16, block.firstEntry, LITTLE_ENDIAN);
    view.setUint32(offset + 20, block.entryOffsets.length, LITTLE_ENDIAN);
    output.set(block.compressed, dataOffset + compressedOffset);
    compressedOffset += block.compressed.byteLength;
  }

  const entryTable = output.subarray(entriesOffset, entriesOffset + entryTableBytes);
  const blockTable = output.subarray(blocksOffset, blocksOffset + blockTableBytes);
  view.setUint32(ENTRIES_CHECKSUM_OFFSET, crc32(entryTable), LITTLE_ENDIAN);
  view.setUint32(BLOCKS_CHECKSUM_OFFSET, crc32(blockTable), LITTLE_ENDIAN);
  view.setUint32(HEADER_CHECKSUM_OFFSET, 0, LITTLE_ENDIAN);
  view.setUint32(HEADER_CHECKSUM_OFFSET, crc32(output.subarray(0, HEADER_BYTES)), LITTLE_ENDIAN);

  let largestRecordBytes = 0;
  for (const record of records) largestRecordBytes = Math.max(largestRecordBytes, record.byteLength);
  let largestBlockBytes = 0;
  for (const block of blocks) {
    largestBlockBytes = Math.max(largestBlockBytes, block.uncompressed.byteLength);
  }

  return {
    bytes: output,
    stats: {
      entryCount: entries.length,
      senseCount: entries.reduce((sum, entry) => sum + entry.senses.length, 0),
      glossCount: entries.reduce(
        (sum, entry) => sum + entry.senses.reduce((inner, sense) => inner + sense.glosses.length, 0),
        0
      ),
      propertyCount: entries.reduce(
        (sum, entry) => sum + entry.senses.reduce((inner, sense) => inner + sense.properties.length, 0),
        0
      ),
      blockCount: blocks.length,
      indexBytes: dataOffset,
      uncompressedRecordBytes: blocks.reduce((sum, block) => sum + block.uncompressed.byteLength, 0),
      compressedDataBytes,
      totalBytes,
      largestRecordBytes,
      largestBlockBytes
    }
  };
}

/** Load the normalized, root-only detail projection from PostgreSQL. */
export async function loadDetailEntries(sql: postgres.Sql): Promise<DetailEntrySource[]> {
  const [entryRows, senseRows, glossRows, propertyRows] = await Promise.all([
    sql.unsafe<Array<{ seq: number }>>(`
      SELECT seq FROM entry WHERE root_p = TRUE ORDER BY seq
    `),
    sql.unsafe<SenseRow[]>(`
      SELECT s.id, s.seq, s.ord
      FROM sense s
      JOIN entry e USING (seq)
      WHERE e.root_p = TRUE
      ORDER BY s.seq, s.ord
    `),
    sql.unsafe<GlossRow[]>(`
      SELECT g.sense_id AS "senseId", g.ord, g.text
      FROM gloss g
      JOIN sense s ON s.id = g.sense_id
      JOIN entry e USING (seq)
      WHERE e.root_p = TRUE
      ORDER BY s.seq, s.ord, g.ord
    `),
    sql.unsafe<PropertyRow[]>(`
      SELECT sp.sense_id AS "senseId", sp.tag, sp.ord, sp.text
      FROM sense_prop sp
      JOIN sense s ON s.id = sp.sense_id
      JOIN entry e ON e.seq = s.seq
      WHERE e.root_p = TRUE
      ORDER BY s.seq, s.ord, sp.tag COLLATE "C", sp.ord, sp.text COLLATE "C"
    `)
  ]);

  const glosses = new Map<number, DetailGlossSource[]>();
  for (const row of glossRows) {
    const values = glosses.get(row.senseId);
    const gloss = { ord: row.ord, text: row.text };
    if (values) values.push(gloss);
    else glosses.set(row.senseId, [gloss]);
  }
  const properties = new Map<number, DetailPropertySource[]>();
  for (const row of propertyRows) {
    if (!PROPERTY_TAG_IDS.has(row.tag)) {
      throw new DetailStoreEncodingError(`Unknown database sense-property tag ${row.tag}`);
    }
    const values = properties.get(row.senseId);
    const property = {
      tag: row.tag as DetailPropertyTag,
      ord: row.ord,
      text: row.text
    };
    if (values) values.push(property);
    else properties.set(row.senseId, [property]);
  }

  const senses = new Map<number, DetailSenseSource[]>();
  for (const row of senseRows) {
    const values = senses.get(row.seq);
    const sense = {
      ord: row.ord,
      glosses: glosses.get(row.id) ?? [],
      properties: properties.get(row.id) ?? []
    };
    if (values) values.push(sense);
    else senses.set(row.seq, [sense]);
    glosses.delete(row.id);
    properties.delete(row.id);
  }
  if (glosses.size !== 0 || properties.size !== 0) {
    throw new DetailStoreEncodingError('Gloss or property references a missing root sense');
  }
  return entryRows.map(({ seq }) => ({ seq, senses: senses.get(seq) ?? [] }));
}
