import { gzipSync } from 'node:zlib';

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

export interface IndexedGzipStoreStats {
  readonly entryCount: number;
  readonly blockCount: number;
  readonly indexBytes: number;
  readonly uncompressedRecordBytes: number;
  readonly compressedDataBytes: number;
  readonly totalBytes: number;
  readonly largestRecordBytes: number;
  readonly largestBlockBytes: number;
}

export class BinaryStoreEncodingError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'BinaryStoreEncodingError';
  }
}

export class BinaryWriter {
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
      throw new BinaryStoreEncodingError(`${label} must fit uint8`);
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

interface EncodedBlock {
  readonly firstEntry: number;
  readonly entryOffsets: readonly number[];
  readonly uncompressed: Uint8Array;
  readonly compressed: Uint8Array;
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

export function assertUint32(value: number, label: string): void {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff_ffff) {
    throw new BinaryStoreEncodingError(`${label} must fit uint32`);
  }
}

export function compareBinaryText(left: string, right: string): number {
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

export function buildIndexedGzipStore(options: {
  readonly magic: string;
  readonly formatVersion: number;
  readonly headerBytes: number;
  readonly records: readonly Uint8Array[];
  readonly targetBlockBytes?: number;
  readonly writeHeaderExtension?: (bytes: Uint8Array, view: DataView) => void;
}): { readonly bytes: Uint8Array; readonly stats: IndexedGzipStoreStats } {
  const targetBlockBytes = options.targetBlockBytes ?? 64 * 1024;
  if (options.magic.length !== 8 || !/^[\x20-\x7e]{8}$/.test(options.magic)) {
    throw new BinaryStoreEncodingError('Store magic must be exactly eight ASCII bytes');
  }
  if (!Number.isSafeInteger(options.formatVersion)
    || options.formatVersion < 1 || options.formatVersion > 0xffff) {
    throw new BinaryStoreEncodingError('Store format version must fit uint16');
  }
  if (!Number.isSafeInteger(options.headerBytes)
    || options.headerBytes < 64 || options.headerBytes > 0xffff
    || options.headerBytes % ALIGNMENT !== 0) {
    throw new BinaryStoreEncodingError('Store header size must be an aligned uint16 of at least 64');
  }
  if (!Number.isSafeInteger(targetBlockBytes) || targetBlockBytes < 1024) {
    throw new BinaryStoreEncodingError('Target block size must be an integer of at least 1024 bytes');
  }
  if (options.records.length === 0) {
    throw new BinaryStoreEncodingError('Indexed store requires at least one entry record');
  }

  const blocks: EncodedBlock[] = [];
  let blockRecords: Uint8Array[] = [];
  let blockBytes = 0;
  let firstEntry = 0;
  for (const record of options.records) {
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

  const entriesOffset = options.headerBytes;
  const entryTableBytes = options.records.length * ENTRY_BYTES;
  const blocksOffset = align(entriesOffset + entryTableBytes);
  const blockTableBytes = blocks.length * BLOCK_BYTES;
  const dataOffset = align(blocksOffset + blockTableBytes);
  const compressedDataBytes = blocks.reduce((sum, block) => sum + block.compressed.byteLength, 0);
  const totalBytes = dataOffset + compressedDataBytes;
  for (const [value, label] of [
    [options.records.length, 'Entry count'], [blocks.length, 'Block count'],
    [targetBlockBytes, 'Target block size'], [entriesOffset, 'Entries offset'],
    [blocksOffset, 'Blocks offset'], [dataOffset, 'Data offset'], [totalBytes, 'Total bytes']
  ] as const) assertUint32(value, label);

  const output = new Uint8Array(totalBytes);
  const view = new DataView(output.buffer);
  for (let index = 0; index < options.magic.length; index++) {
    output[index] = options.magic.charCodeAt(index);
  }
  view.setUint16(8, options.formatVersion, LITTLE_ENDIAN);
  view.setUint16(10, options.headerBytes, LITTLE_ENDIAN);
  view.setUint32(12, 0, LITTLE_ENDIAN);
  view.setUint32(TOTAL_BYTES_OFFSET, totalBytes, LITTLE_ENDIAN);
  view.setUint32(ENTRY_COUNT_OFFSET, options.records.length, LITTLE_ENDIAN);
  view.setUint32(BLOCK_COUNT_OFFSET, blocks.length, LITTLE_ENDIAN);
  view.setUint16(ENTRY_STRIDE_OFFSET, ENTRY_BYTES, LITTLE_ENDIAN);
  view.setUint16(BLOCK_STRIDE_OFFSET, BLOCK_BYTES, LITTLE_ENDIAN);
  view.setUint32(TARGET_BLOCK_BYTES_OFFSET, targetBlockBytes, LITTLE_ENDIAN);
  view.setUint32(ENTRIES_OFFSET, entriesOffset, LITTLE_ENDIAN);
  view.setUint32(BLOCKS_OFFSET, blocksOffset, LITTLE_ENDIAN);
  view.setUint32(DATA_OFFSET, dataOffset, LITTLE_ENDIAN);
  options.writeHeaderExtension?.(output, view);

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
  view.setUint32(HEADER_CHECKSUM_OFFSET, crc32(output.subarray(0, options.headerBytes)), LITTLE_ENDIAN);

  return {
    bytes: output,
    stats: {
      entryCount: options.records.length,
      blockCount: blocks.length,
      indexBytes: dataOffset,
      uncompressedRecordBytes: blocks.reduce((sum, block) => sum + block.uncompressed.byteLength, 0),
      compressedDataBytes,
      totalBytes,
      largestRecordBytes: options.records.reduce(
        (maximum, record) => Math.max(maximum, record.byteLength), 0
      ),
      largestBlockBytes: blocks.reduce(
        (maximum, block) => Math.max(maximum, block.uncompressed.byteLength), 0
      )
    }
  };
}
