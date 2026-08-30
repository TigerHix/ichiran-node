import { crc32 } from './crc32.js';

export const DETAILS_MAGIC = 'ICHIDETL';
export const DETAILS_FORMAT_VERSION = 2;
export const DETAILS_HEADER_BYTES = 96;
export const DETAILS_ENTRY_BYTES = 8;
export const DETAILS_BLOCK_BYTES = 24;

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
const RESERVED_OFFSET = 60;

const BLOCK_DATA_OFFSET = 0;
const BLOCK_COMPRESSED_BYTES_OFFSET = 4;
const BLOCK_UNCOMPRESSED_BYTES_OFFSET = 8;
const BLOCK_CHECKSUM_OFFSET = 12;
const BLOCK_FIRST_ENTRY_OFFSET = 16;
const BLOCK_ENTRY_COUNT_OFFSET = 20;

const PROPERTY_TAGS = [
  'dial', 'field', 'misc', 'pos', 's_inf', 'stagk', 'stagr'
] as const;

export type DetailPropertyTag = typeof PROPERTY_TAGS[number];

export interface DetailGloss {
  readonly ord: number;
  readonly text: string;
}

export interface DetailProperty {
  readonly tag: DetailPropertyTag;
  readonly ord: number;
  readonly text: string;
}

export interface DetailSense {
  readonly ord: number;
  readonly glosses: readonly DetailGloss[];
  readonly properties: readonly DetailProperty[];
}

export interface DetailForm {
  readonly route: 'kanji' | 'kana';
  readonly text: string;
  readonly ord: number;
  readonly common: number | null;
  readonly commonTags: string;
  readonly conjugatable: boolean;
  readonly nokanji: boolean;
  readonly best: string | null;
}

export interface DetailEntry {
  readonly seq: number;
  readonly forms: readonly DetailForm[];
  readonly senses: readonly DetailSense[];
}

export interface DetailRandomAccessSource {
  readonly byteLength: number;
  read(offset: number, byteLength: number): Promise<Uint8Array>;
}

export type DetailGzipDecoder = (
  compressed: Uint8Array,
  expectedByteLength: number
) => Promise<Uint8Array>;

export type DetailStoreErrorCode =
  | 'invalid-header'
  | 'unsupported-version'
  | 'corrupt-index'
  | 'corrupt-block'
  | 'out-of-range';

export class DetailStoreError extends Error {
  readonly code: DetailStoreErrorCode;

  constructor(code: DetailStoreErrorCode, message: string) {
    super(message);
    this.name = 'DetailStoreError';
    this.code = code;
  }
}

export interface DetailStoreManifest {
  readonly byteLength: number;
  readonly entryCount: number;
  readonly blockCount: number;
  readonly targetBlockBytes: number;
  readonly residentIndexBytes: number;
  readonly compressedDataBytes: number;
}

interface Utf8Decoder {
  decode(input: Uint8Array): string;
}

interface Utf8DecoderConstructor {
  new(label: string, options: { fatal: boolean }): Utf8Decoder;
}

const UTF8_DECODER = new (
  globalThis as unknown as { TextDecoder: Utf8DecoderConstructor }
).TextDecoder('utf-8', { fatal: true });

function align(value: number): number {
  return Math.ceil(value / 8) * 8;
}

function hasMagic(bytes: Uint8Array): boolean {
  for (let index = 0; index < DETAILS_MAGIC.length; index++) {
    if (bytes[index] !== DETAILS_MAGIC.charCodeAt(index)) return false;
  }
  return true;
}

function assertZero(bytes: Uint8Array, start: number, end: number): void {
  for (let index = start; index < end; index++) {
    if (bytes[index] !== 0) {
      throw new DetailStoreError('invalid-header', `Reserved byte ${index} is non-zero`);
    }
  }
}

function assertIndex(index: number, count: number, label: string): void {
  if (!Number.isSafeInteger(index) || index < 0 || index >= count) {
    throw new DetailStoreError('out-of-range', `${label} ${index} is outside [0, ${count})`);
  }
}

async function readExact(
  source: DetailRandomAccessSource,
  offset: number,
  byteLength: number,
  code: DetailStoreErrorCode
): Promise<Uint8Array> {
  if (
    !Number.isSafeInteger(offset)
    || !Number.isSafeInteger(byteLength)
    || offset < 0
    || byteLength < 0
    || offset + byteLength > source.byteLength
  ) {
    throw new DetailStoreError(code, 'Random-access read exceeds the detail store');
  }
  const bytes = await source.read(offset, byteLength);
  if (bytes.byteLength !== byteLength) {
    throw new DetailStoreError(code, `Expected ${byteLength} bytes but read ${bytes.byteLength}`);
  }
  return bytes;
}

class RecordCursor {
  readonly #bytes: Uint8Array;
  readonly #end: number;
  #offset: number;

  constructor(bytes: Uint8Array, start: number, byteLength: number) {
    this.#bytes = bytes;
    this.#offset = start;
    this.#end = start + byteLength;
    if (start < 0 || byteLength < 0 || this.#end > bytes.byteLength) {
      throw new DetailStoreError('corrupt-block', 'Detail record exceeds its block');
    }
  }

  uint(): number {
    let value = 0;
    let shift = 0;
    for (let count = 0; count < 5; count++) {
      if (this.#offset >= this.#end) {
        throw new DetailStoreError('corrupt-block', 'Truncated detail varint');
      }
      const byte = this.#bytes[this.#offset++]!;
      value += (byte & 0x7f) * 2 ** shift;
      if ((byte & 0x80) === 0) {
        if (!Number.isSafeInteger(value) || value > 0xffff_ffff) {
          throw new DetailStoreError('corrupt-block', 'Detail varint exceeds uint32');
        }
        return value;
      }
      shift += 7;
    }
    throw new DetailStoreError('corrupt-block', 'Detail varint is not canonical uint32');
  }

  byte(): number {
    if (this.#offset >= this.#end) {
      throw new DetailStoreError('corrupt-block', 'Truncated detail byte');
    }
    return this.#bytes[this.#offset++]!;
  }

  text(): string {
    const byteLength = this.uint();
    const end = this.#offset + byteLength;
    if (end > this.#end) {
      throw new DetailStoreError('corrupt-block', 'Truncated detail string');
    }
    let text: string;
    try {
      text = UTF8_DECODER.decode(this.#bytes.subarray(this.#offset, end));
    } catch {
      throw new DetailStoreError('corrupt-block', 'Detail string is not valid UTF-8');
    }
    this.#offset = end;
    return text;
  }

  finish(): void {
    if (this.#offset !== this.#end) {
      throw new DetailStoreError('corrupt-block', 'Detail record has trailing bytes');
    }
  }
}

/** A one-block cache keeps details random-access without making them resident. */
export class DetailStoreReader {
  readonly manifest: DetailStoreManifest;

  readonly #source: DetailRandomAccessSource;
  readonly #decodeGzip: DetailGzipDecoder;
  readonly #indexView: DataView;
  readonly #blocksView: DataView;
  readonly #dataOffset: number;
  #cachedBlock = -1;
  #cachedBytes: Uint8Array | null = null;

  private constructor(
    source: DetailRandomAccessSource,
    decodeGzip: DetailGzipDecoder,
    manifest: DetailStoreManifest,
    index: Uint8Array,
    blocks: Uint8Array,
    dataOffset: number
  ) {
    this.#source = source;
    this.#decodeGzip = decodeGzip;
    this.manifest = manifest;
    this.#indexView = new DataView(index.buffer, index.byteOffset, index.byteLength);
    this.#blocksView = new DataView(blocks.buffer, blocks.byteOffset, blocks.byteLength);
    this.#dataOffset = dataOffset;
  }

  static async open(
    source: DetailRandomAccessSource,
    decodeGzip: DetailGzipDecoder
  ): Promise<DetailStoreReader> {
    const header = await readExact(source, 0, DETAILS_HEADER_BYTES, 'invalid-header');
    if (!hasMagic(header)) {
      throw new DetailStoreError('invalid-header', `Expected ${DETAILS_MAGIC} magic bytes`);
    }
    const view = new DataView(header.buffer, header.byteOffset, header.byteLength);
    const version = view.getUint16(8, LITTLE_ENDIAN);
    if (version !== DETAILS_FORMAT_VERSION) {
      throw new DetailStoreError('unsupported-version', `Unsupported detail format ${version}`);
    }
    if (
      view.getUint16(10, LITTLE_ENDIAN) !== DETAILS_HEADER_BYTES
      || view.getUint32(12, LITTLE_ENDIAN) !== 0
      || view.getUint16(ENTRY_STRIDE_OFFSET, LITTLE_ENDIAN) !== DETAILS_ENTRY_BYTES
      || view.getUint16(BLOCK_STRIDE_OFFSET, LITTLE_ENDIAN) !== DETAILS_BLOCK_BYTES
    ) {
      throw new DetailStoreError('invalid-header', 'Detail header sizes or flags are invalid');
    }
    assertZero(header, RESERVED_OFFSET, DETAILS_HEADER_BYTES);

    const headerCopy = header.slice();
    new DataView(headerCopy.buffer).setUint32(HEADER_CHECKSUM_OFFSET, 0, LITTLE_ENDIAN);
    if (crc32(headerCopy) !== view.getUint32(HEADER_CHECKSUM_OFFSET, LITTLE_ENDIAN)) {
      throw new DetailStoreError('invalid-header', 'Detail header checksum does not match');
    }

    const totalBytes = view.getUint32(TOTAL_BYTES_OFFSET, LITTLE_ENDIAN);
    const entryCount = view.getUint32(ENTRY_COUNT_OFFSET, LITTLE_ENDIAN);
    const blockCount = view.getUint32(BLOCK_COUNT_OFFSET, LITTLE_ENDIAN);
    const targetBlockBytes = view.getUint32(TARGET_BLOCK_BYTES_OFFSET, LITTLE_ENDIAN);
    const entriesOffset = view.getUint32(ENTRIES_OFFSET, LITTLE_ENDIAN);
    const blocksOffset = view.getUint32(BLOCKS_OFFSET, LITTLE_ENDIAN);
    const dataOffset = view.getUint32(DATA_OFFSET, LITTLE_ENDIAN);
    const entryBytes = entryCount * DETAILS_ENTRY_BYTES;
    const blockBytes = blockCount * DETAILS_BLOCK_BYTES;
    if (
      totalBytes !== source.byteLength
      || entryCount === 0
      || blockCount === 0
      || targetBlockBytes === 0
      || !Number.isSafeInteger(entryBytes)
      || !Number.isSafeInteger(blockBytes)
      || entriesOffset !== DETAILS_HEADER_BYTES
      || blocksOffset !== align(entriesOffset + entryBytes)
      || dataOffset !== align(blocksOffset + blockBytes)
      || dataOffset > totalBytes
    ) {
      throw new DetailStoreError('invalid-header', 'Detail offsets or counts are invalid');
    }

    const prefix = await readExact(
      source,
      entriesOffset,
      dataOffset - entriesOffset,
      'corrupt-index'
    );
    const index = prefix.subarray(0, entryBytes);
    const blockRelative = blocksOffset - entriesOffset;
    const blocks = prefix.subarray(blockRelative, blockRelative + blockBytes);
    if (crc32(index) !== view.getUint32(ENTRIES_CHECKSUM_OFFSET, LITTLE_ENDIAN)) {
      throw new DetailStoreError('corrupt-index', 'Detail entry-index checksum does not match');
    }
    if (crc32(blocks) !== view.getUint32(BLOCKS_CHECKSUM_OFFSET, LITTLE_ENDIAN)) {
      throw new DetailStoreError('corrupt-index', 'Detail block-table checksum does not match');
    }
    assertZero(prefix, entryBytes, blockRelative);
    assertZero(prefix, blockRelative + blockBytes, prefix.byteLength);

    const indexView = new DataView(index.buffer, index.byteOffset, index.byteLength);
    const blocksView = new DataView(blocks.buffer, blocks.byteOffset, blocks.byteLength);
    let nextEntry = 0;
    let nextData = 0;
    for (let block = 0; block < blockCount; block++) {
      const offset = block * DETAILS_BLOCK_BYTES;
      const data = blocksView.getUint32(offset + BLOCK_DATA_OFFSET, LITTLE_ENDIAN);
      const compressed = blocksView.getUint32(offset + BLOCK_COMPRESSED_BYTES_OFFSET, LITTLE_ENDIAN);
      const uncompressed = blocksView.getUint32(offset + BLOCK_UNCOMPRESSED_BYTES_OFFSET, LITTLE_ENDIAN);
      const firstEntry = blocksView.getUint32(offset + BLOCK_FIRST_ENTRY_OFFSET, LITTLE_ENDIAN);
      const count = blocksView.getUint32(offset + BLOCK_ENTRY_COUNT_OFFSET, LITTLE_ENDIAN);
      if (
        data !== nextData
        || compressed === 0
        || uncompressed === 0
        || firstEntry !== nextEntry
        || count === 0
        || dataOffset + data + compressed > totalBytes
      ) {
        throw new DetailStoreError('corrupt-index', `Detail block ${block} is not canonical`);
      }
      let previousRecordOffset = -1;
      for (let entry = firstEntry; entry < firstEntry + count; entry++) {
        if (entry >= entryCount) {
          throw new DetailStoreError('corrupt-index', `Detail block ${block} exceeds entry count`);
        }
        const entryOffset = entry * DETAILS_ENTRY_BYTES;
        const entryBlock = indexView.getUint32(entryOffset, LITTLE_ENDIAN);
        const recordOffset = indexView.getUint32(entryOffset + 4, LITTLE_ENDIAN);
        if (entryBlock !== block || recordOffset <= previousRecordOffset || recordOffset + 4 > uncompressed) {
          throw new DetailStoreError('corrupt-index', `Detail entry ${entry} is not canonical`);
        }
        previousRecordOffset = recordOffset;
      }
      nextEntry += count;
      nextData += compressed;
    }
    if (nextEntry !== entryCount || dataOffset + nextData !== totalBytes) {
      throw new DetailStoreError('corrupt-index', 'Detail blocks do not cover the store');
    }

    return new DetailStoreReader(
      source,
      decodeGzip,
      Object.freeze({
        byteLength: totalBytes,
        entryCount,
        blockCount,
        targetBlockBytes,
        residentIndexBytes: prefix.byteLength + header.byteLength,
        compressedDataBytes: totalBytes - dataOffset
      }),
      index,
      blocks,
      dataOffset
    );
  }

  async entry(entryIndex: number): Promise<DetailEntry> {
    assertIndex(entryIndex, this.manifest.entryCount, 'Detail entry');
    const entryOffset = entryIndex * DETAILS_ENTRY_BYTES;
    const block = this.#indexView.getUint32(entryOffset, LITTLE_ENDIAN);
    const recordOffset = this.#indexView.getUint32(entryOffset + 4, LITTLE_ENDIAN);
    const bytes = await this.#loadBlock(block);
    if (recordOffset + 4 > bytes.byteLength) {
      throw new DetailStoreError('corrupt-block', `Detail entry ${entryIndex} has no length`);
    }
    const recordBytes = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength)
      .getUint32(recordOffset, LITTLE_ENDIAN);
    const cursor = new RecordCursor(bytes, recordOffset + 4, recordBytes);
    const seq = cursor.uint();
    const formCount = cursor.uint();
    const forms: DetailForm[] = [];
    for (let formIndex = 0; formIndex < formCount; formIndex++) {
      const flags = cursor.byte();
      if ((flags & 0xf0) !== 0) {
        throw new DetailStoreError('corrupt-block', `Unknown detail form flags ${flags}`);
      }
      const route = (flags & 1) !== 0 ? 'kana' : 'kanji';
      const ord = cursor.uint();
      const encodedCommon = cursor.uint();
      forms.push({
        route,
        ord,
        common: encodedCommon === 0 ? null : encodedCommon - 1,
        text: cursor.text(),
        commonTags: cursor.text(),
        conjugatable: (flags & (1 << 1)) !== 0,
        nokanji: (flags & (1 << 2)) !== 0,
        best: (flags & (1 << 3)) !== 0 ? cursor.text() : null
      });
    }
    const senseCount = cursor.uint();
    const senses: DetailSense[] = [];
    for (let senseIndex = 0; senseIndex < senseCount; senseIndex++) {
      const ord = cursor.uint();
      const glossCount = cursor.uint();
      const glosses: DetailGloss[] = [];
      for (let glossIndex = 0; glossIndex < glossCount; glossIndex++) {
        glosses.push({ ord: cursor.uint(), text: cursor.text() });
      }
      const propertyCount = cursor.uint();
      const properties: DetailProperty[] = [];
      for (let propertyIndex = 0; propertyIndex < propertyCount; propertyIndex++) {
        const tagId = cursor.byte();
        const tag = PROPERTY_TAGS[tagId];
        if (tag === undefined) {
          throw new DetailStoreError('corrupt-block', `Unknown detail property tag ${tagId}`);
        }
        properties.push({ tag, ord: cursor.uint(), text: cursor.text() });
      }
      senses.push({ ord, glosses, properties });
    }
    cursor.finish();
    return { seq, forms, senses };
  }

  clearCache(): void {
    this.#cachedBlock = -1;
    this.#cachedBytes = null;
  }

  async #loadBlock(block: number): Promise<Uint8Array> {
    assertIndex(block, this.manifest.blockCount, 'Detail block');
    if (block === this.#cachedBlock && this.#cachedBytes) return this.#cachedBytes;
    const offset = block * DETAILS_BLOCK_BYTES;
    const data = this.#blocksView.getUint32(offset + BLOCK_DATA_OFFSET, LITTLE_ENDIAN);
    const compressedBytes = this.#blocksView.getUint32(
      offset + BLOCK_COMPRESSED_BYTES_OFFSET,
      LITTLE_ENDIAN
    );
    const uncompressedBytes = this.#blocksView.getUint32(
      offset + BLOCK_UNCOMPRESSED_BYTES_OFFSET,
      LITTLE_ENDIAN
    );
    const expectedChecksum = this.#blocksView.getUint32(
      offset + BLOCK_CHECKSUM_OFFSET,
      LITTLE_ENDIAN
    );
    const compressed = await readExact(
      this.#source,
      this.#dataOffset + data,
      compressedBytes,
      'corrupt-block'
    );
    let decoded: Uint8Array;
    try {
      decoded = await this.#decodeGzip(compressed, uncompressedBytes);
    } catch (error) {
      throw new DetailStoreError(
        'corrupt-block',
        `Could not decompress detail block ${block}: ${error instanceof Error ? error.message : String(error)}`
      );
    }
    if (decoded.byteLength !== uncompressedBytes || crc32(decoded) !== expectedChecksum) {
      throw new DetailStoreError('corrupt-block', `Detail block ${block} checksum does not match`);
    }
    this.#cachedBlock = block;
    this.#cachedBytes = decoded;
    return decoded;
  }
}

export function memoryDetailSource(input: ArrayBuffer | Uint8Array): DetailRandomAccessSource {
  const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
  return {
    byteLength: bytes.byteLength,
    async read(offset, byteLength) {
      return bytes.subarray(offset, offset + byteLength);
    }
  };
}

export function openDetailStore(
  source: DetailRandomAccessSource,
  decodeGzip: DetailGzipDecoder
): Promise<DetailStoreReader> {
  return DetailStoreReader.open(source, decodeGzip);
}
