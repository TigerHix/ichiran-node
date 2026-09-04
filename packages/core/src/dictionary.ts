import { crc32 } from './crc32.js';
import {
  DictionaryStoreError,
  type DictionaryEntry,
  type DictionaryForm,
  type DictionaryGloss,
  type DictionaryProperty,
  type DictionaryRandomAccessSource,
  type DictionarySense,
  type DictionaryStoreErrorCode,
  type LexiconEntry,
  type LexiconPropertyTag,
  type LexiconSense,
  type LocaleGlossEntry,
  type LocaleGlossGroup
} from './dictionary-contract.js';
export * from './dictionary-contract.js';

export const LEXICON_MAGIC = 'ICHILEXI';
export const LOCALE_GLOSS_MAGIC = 'ICHIGLOS';
export const DICTIONARY_FORMAT_VERSION = 1;
export const LEXICON_HEADER_BYTES = 96;
export const LOCALE_GLOSS_HEADER_BYTES = 128;

const ENTRY_BYTES = 8;
const BLOCK_BYTES = 24;
const LITTLE_ENDIAN = true;
const LEXICON_PROPERTY_TAGS: readonly LexiconPropertyTag[] = [
  'dial', 'field', 'misc', 'pos', 'stagk', 'stagr'
];

export type DictionaryGzipDecoder = (
  compressed: Uint8Array,
  expectedByteLength: number
) => Promise<Uint8Array>;

export interface DictionaryStoreManifest {
  readonly byteLength: number;
  readonly entryCount: number;
  readonly blockCount: number;
  readonly targetBlockBytes: number;
  readonly residentIndexBytes: number;
  readonly compressedDataBytes: number;
}

interface StoreSpec {
  readonly magic: string;
  readonly headerBytes: number;
  readonly label: string;
}

interface StoreHeader extends DictionaryStoreManifest {
  readonly entriesOffset: number;
  readonly blocksOffset: number;
  readonly dataOffset: number;
}

const decoder = new TextDecoder('utf-8', { fatal: true });

function align(value: number): number { return Math.ceil(value / 8) * 8; }

function assertZero(bytes: Uint8Array, start: number, end: number, label: string): void {
  for (let index = start; index < end; index++) {
    if (bytes[index] !== 0) throw new DictionaryStoreError('invalid-header', `${label} byte ${index} is non-zero`);
  }
}

function hasMagic(bytes: Uint8Array, expected: string): boolean {
  return [...expected].every((character, index) => bytes[index] === character.charCodeAt(0));
}

function parseHeader(bytes: Uint8Array, totalBytes: number, spec: StoreSpec): StoreHeader {
  if (bytes.byteLength < spec.headerBytes || !hasMagic(bytes, spec.magic)) {
    throw new DictionaryStoreError('invalid-header', `Expected a complete ${spec.label} header`);
  }
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const version = view.getUint16(8, LITTLE_ENDIAN);
  if (version !== DICTIONARY_FORMAT_VERSION) {
    throw new DictionaryStoreError('unsupported-version', `Unsupported ${spec.label} format ${version}`);
  }
  if (
    view.getUint16(10, LITTLE_ENDIAN) !== spec.headerBytes
    || view.getUint32(12, LITTLE_ENDIAN) !== 0
    || view.getUint16(32, LITTLE_ENDIAN) !== ENTRY_BYTES
    || view.getUint16(34, LITTLE_ENDIAN) !== BLOCK_BYTES
  ) throw new DictionaryStoreError('invalid-header', `${spec.label} header sizes or flags are invalid`);
  const headerCopy = bytes.subarray(0, spec.headerBytes).slice();
  new DataView(headerCopy.buffer).setUint32(20, 0, LITTLE_ENDIAN);
  if (crc32(headerCopy) !== view.getUint32(20, LITTLE_ENDIAN)) {
    throw new DictionaryStoreError('invalid-header', `${spec.label} header checksum does not match`);
  }
  const byteLength = view.getUint32(16, LITTLE_ENDIAN);
  const entryCount = view.getUint32(24, LITTLE_ENDIAN);
  const blockCount = view.getUint32(28, LITTLE_ENDIAN);
  const targetBlockBytes = view.getUint32(36, LITTLE_ENDIAN);
  const entriesOffset = view.getUint32(40, LITTLE_ENDIAN);
  const blocksOffset = view.getUint32(44, LITTLE_ENDIAN);
  const dataOffset = view.getUint32(48, LITTLE_ENDIAN);
  const entryBytes = entryCount * ENTRY_BYTES;
  const blockBytes = blockCount * BLOCK_BYTES;
  if (
    byteLength !== totalBytes || entryCount === 0 || blockCount === 0 || targetBlockBytes === 0
    || entriesOffset !== spec.headerBytes || blocksOffset !== align(entriesOffset + entryBytes)
    || dataOffset !== align(blocksOffset + blockBytes) || dataOffset > totalBytes
  ) throw new DictionaryStoreError('invalid-header', `${spec.label} offsets or counts are invalid`);
  return {
    byteLength, entryCount, blockCount, targetBlockBytes, entriesOffset, blocksOffset, dataOffset,
    residentIndexBytes: dataOffset,
    compressedDataBytes: byteLength - dataOffset
  };
}

async function readExact(
  source: DictionaryRandomAccessSource,
  offset: number,
  byteLength: number,
  code: DictionaryStoreErrorCode
): Promise<Uint8Array> {
  if (!Number.isSafeInteger(offset) || !Number.isSafeInteger(byteLength) || offset < 0
      || byteLength < 0 || offset + byteLength > source.byteLength) {
    throw new DictionaryStoreError(code, 'Random-access read exceeds the dictionary store');
  }
  const bytes = await source.read(offset, byteLength);
  if (bytes.byteLength !== byteLength) {
    throw new DictionaryStoreError(code, `Expected ${byteLength} bytes but read ${bytes.byteLength}`);
  }
  return bytes;
}

class IndexedStoreReader {
  readonly manifest: DictionaryStoreManifest;
  readonly #source: DictionaryRandomAccessSource;
  readonly #decodeGzip: DictionaryGzipDecoder;
  readonly #index: DataView;
  readonly #blocks: DataView;
  readonly #dataOffset: number;
  #cachedBlock = -1;
  #cachedBytes: Uint8Array | null = null;

  private constructor(
    source: DictionaryRandomAccessSource,
    decodeGzip: DictionaryGzipDecoder,
    header: StoreHeader,
    index: Uint8Array,
    blocks: Uint8Array
  ) {
    this.#source = source;
    this.#decodeGzip = decodeGzip;
    this.manifest = header;
    this.#index = new DataView(index.buffer, index.byteOffset, index.byteLength);
    this.#blocks = new DataView(blocks.buffer, blocks.byteOffset, blocks.byteLength);
    this.#dataOffset = header.dataOffset;
  }

  static async open(
    source: DictionaryRandomAccessSource,
    decodeGzip: DictionaryGzipDecoder,
    spec: StoreSpec
  ): Promise<{ readonly reader: IndexedStoreReader; readonly header: Uint8Array }> {
    const fixed = await readExact(source, 0, spec.headerBytes, 'invalid-header');
    const parsed = parseHeader(fixed, source.byteLength, spec);
    const prefix = await readExact(source, parsed.entriesOffset, parsed.dataOffset - parsed.entriesOffset, 'corrupt-index');
    const entryBytes = parsed.entryCount * ENTRY_BYTES;
    const blockStart = parsed.blocksOffset - parsed.entriesOffset;
    const blockBytes = parsed.blockCount * BLOCK_BYTES;
    const index = prefix.subarray(0, entryBytes);
    const blocks = prefix.subarray(blockStart, blockStart + blockBytes);
    const headerView = new DataView(fixed.buffer, fixed.byteOffset, fixed.byteLength);
    if (crc32(index) !== headerView.getUint32(52, LITTLE_ENDIAN)
        || crc32(blocks) !== headerView.getUint32(56, LITTLE_ENDIAN)) {
      throw new DictionaryStoreError('corrupt-index', `${spec.label} index checksum does not match`);
    }
    assertZero(prefix, entryBytes, blockStart, `${spec.label} index padding`);
    assertZero(prefix, blockStart + blockBytes, prefix.byteLength, `${spec.label} block padding`);
    let nextEntry = 0;
    let nextData = 0;
    const indexView = new DataView(index.buffer, index.byteOffset, index.byteLength);
    const blocksView = new DataView(blocks.buffer, blocks.byteOffset, blocks.byteLength);
    for (let block = 0; block < parsed.blockCount; block++) {
      const at = block * BLOCK_BYTES;
      const data = blocksView.getUint32(at, LITTLE_ENDIAN);
      const compressed = blocksView.getUint32(at + 4, LITTLE_ENDIAN);
      const uncompressed = blocksView.getUint32(at + 8, LITTLE_ENDIAN);
      const firstEntry = blocksView.getUint32(at + 16, LITTLE_ENDIAN);
      const count = blocksView.getUint32(at + 20, LITTLE_ENDIAN);
      if (data !== nextData || compressed === 0 || uncompressed === 0 || firstEntry !== nextEntry
          || count === 0 || firstEntry + count > parsed.entryCount
          || parsed.dataOffset + data + compressed > parsed.byteLength) {
        throw new DictionaryStoreError('corrupt-index', `${spec.label} block ${block} is not canonical`);
      }
      let previous = -1;
      for (let entry = firstEntry; entry < firstEntry + count; entry++) {
        const entryAt = entry * ENTRY_BYTES;
        const entryBlock = indexView.getUint32(entryAt, LITTLE_ENDIAN);
        const record = indexView.getUint32(entryAt + 4, LITTLE_ENDIAN);
        if (entryBlock !== block || record <= previous || record + 4 > uncompressed) {
          throw new DictionaryStoreError('corrupt-index', `${spec.label} entry ${entry} is not canonical`);
        }
        previous = record;
      }
      nextEntry += count;
      nextData += compressed;
    }
    if (nextEntry !== parsed.entryCount || parsed.dataOffset + nextData !== parsed.byteLength) {
      throw new DictionaryStoreError('corrupt-index', `${spec.label} blocks do not cover the store`);
    }
    return { reader: new IndexedStoreReader(source, decodeGzip, parsed, index, blocks), header: fixed };
  }

  async record(entryIndex: number): Promise<Uint8Array> {
    if (!Number.isSafeInteger(entryIndex) || entryIndex < 0 || entryIndex >= this.manifest.entryCount) {
      throw new DictionaryStoreError('out-of-range', `Dictionary entry ${entryIndex} is out of range`);
    }
    const entryAt = entryIndex * ENTRY_BYTES;
    const block = this.#index.getUint32(entryAt, LITTLE_ENDIAN);
    const recordOffset = this.#index.getUint32(entryAt + 4, LITTLE_ENDIAN);
    if (this.#cachedBlock !== block || !this.#cachedBytes) {
      const blockAt = block * BLOCK_BYTES;
      const relative = this.#blocks.getUint32(blockAt, LITTLE_ENDIAN);
      const compressedBytes = this.#blocks.getUint32(blockAt + 4, LITTLE_ENDIAN);
      const uncompressedBytes = this.#blocks.getUint32(blockAt + 8, LITTLE_ENDIAN);
      const checksum = this.#blocks.getUint32(blockAt + 12, LITTLE_ENDIAN);
      const compressed = await readExact(this.#source, this.#dataOffset + relative, compressedBytes, 'corrupt-block');
      let decoded: Uint8Array;
      try { decoded = await this.#decodeGzip(compressed, uncompressedBytes); }
      catch (error) {
        throw new DictionaryStoreError('corrupt-block', `Could not decompress dictionary block: ${error instanceof Error ? error.message : String(error)}`);
      }
      if (decoded.byteLength !== uncompressedBytes || crc32(decoded) !== checksum) {
        throw new DictionaryStoreError('corrupt-block', 'Dictionary block checksum does not match');
      }
      this.#cachedBlock = block;
      this.#cachedBytes = decoded;
    }
    const bytes = this.#cachedBytes;
    const recordBytes = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength).getUint32(recordOffset, LITTLE_ENDIAN);
    const start = recordOffset + 4;
    if (start + recordBytes > bytes.byteLength) throw new DictionaryStoreError('corrupt-block', 'Dictionary record exceeds its block');
    return bytes.subarray(start, start + recordBytes);
  }
}

class Cursor {
  readonly #bytes: Uint8Array;
  #offset = 0;
  constructor(bytes: Uint8Array) { this.#bytes = bytes; }
  uint(): number {
    let value = 0;
    let shift = 0;
    for (let count = 0; count < 5; count++) {
      const byte = this.byte();
      value += (byte & 0x7f) * 2 ** shift;
      if ((byte & 0x80) === 0) return value;
      shift += 7;
    }
    throw new DictionaryStoreError('corrupt-block', 'Dictionary varint is not canonical uint32');
  }
  byte(): number {
    const value = this.#bytes[this.#offset++];
    if (value === undefined) throw new DictionaryStoreError('corrupt-block', 'Truncated dictionary byte');
    return value;
  }
  text(): string {
    const length = this.uint();
    const start = this.#offset;
    const end = this.#offset + length;
    if (end > this.#bytes.byteLength) throw new DictionaryStoreError('corrupt-block', 'Truncated dictionary string');
    this.#offset = end;
    try { return decoder.decode(this.#bytes.subarray(start, end)); }
    catch { throw new DictionaryStoreError('corrupt-block', 'Dictionary string is not valid UTF-8'); }
  }
  count(label: string): number {
    const count = this.uint();
    if (count > this.#bytes.byteLength - this.#offset) throw new DictionaryStoreError('corrupt-block', `${label} exceeds the record`);
    return count;
  }
  finish(): void {
    if (this.#offset !== this.#bytes.byteLength) throw new DictionaryStoreError('corrupt-block', 'Dictionary record has trailing bytes');
  }
}

function decodeForm(cursor: Cursor): DictionaryForm {
  const flags = cursor.byte();
  if ((flags & 0xf0) !== 0) throw new DictionaryStoreError('corrupt-block', `Unknown lexicon form flags ${flags}`);
  const ord = cursor.uint();
  const common = cursor.uint();
  const text = cursor.text();
  const commonTags = cursor.text();
  const best = (flags & 8) !== 0 ? cursor.text() : null;
  return {
    route: (flags & 1) !== 0 ? 'kana' : 'kanji', ord, common: common === 0 ? null : common - 1,
    text, commonTags, conjugatable: (flags & 2) !== 0, nokanji: (flags & 4) !== 0, best
  };
}

function decodeGlosses(cursor: Cursor, label: string): DictionaryGloss[] {
  return Array.from({ length: cursor.count(label) }, () => ({ ord: cursor.uint(), text: cursor.text() }));
}

export class LexiconStoreReader {
  readonly manifest: DictionaryStoreManifest;
  readonly #store: IndexedStoreReader;
  private constructor(store: IndexedStoreReader) { this.#store = store; this.manifest = store.manifest; }
  static async open(source: DictionaryRandomAccessSource, decodeGzip: DictionaryGzipDecoder): Promise<LexiconStoreReader> {
    const { reader, header } = await IndexedStoreReader.open(source, decodeGzip, {
      magic: LEXICON_MAGIC, headerBytes: LEXICON_HEADER_BYTES, label: 'lexicon'
    });
    assertZero(header, 60, LEXICON_HEADER_BYTES, 'lexicon reserved header');
    return new LexiconStoreReader(reader);
  }
  async entry(index: number): Promise<LexiconEntry> {
    const cursor = new Cursor(await this.#store.record(index));
    const seq = cursor.uint();
    const forms = Array.from({ length: cursor.count('Lexicon form count') }, () => decodeForm(cursor));
    const senses: LexiconSense[] = Array.from({ length: cursor.count('Lexicon sense count') }, () => {
      const ord = cursor.uint();
      const properties = Array.from({ length: cursor.count('Lexicon property count') }, (): DictionaryProperty & { tag: LexiconPropertyTag } => {
        const tag = LEXICON_PROPERTY_TAGS[cursor.byte()];
        if (!tag) throw new DictionaryStoreError('corrupt-block', 'Unknown lexicon property tag');
        return { tag, ord: cursor.uint(), text: cursor.text() };
      });
      return { ord, properties };
    });
    cursor.finish();
    return { seq, forms, senses };
  }
}

export class LocaleGlossStoreReader {
  readonly manifest: DictionaryStoreManifest;
  readonly locale: string;
  readonly lexiconSha256: string;
  readonly #store: IndexedStoreReader;
  private constructor(store: IndexedStoreReader, locale: string, lexiconSha256: string) {
    this.#store = store; this.manifest = store.manifest; this.locale = locale; this.lexiconSha256 = lexiconSha256;
  }
  static async open(
    source: DictionaryRandomAccessSource,
    decodeGzip: DictionaryGzipDecoder,
    expected: { readonly locale: string; readonly lexiconSha256: string; readonly entryCount: number }
  ): Promise<LocaleGlossStoreReader> {
    const { reader, header } = await IndexedStoreReader.open(source, decodeGzip, {
      magic: LOCALE_GLOSS_MAGIC, headerBytes: LOCALE_GLOSS_HEADER_BYTES, label: 'locale gloss'
    });
    const localeBytes = header[92]!;
    if (localeBytes === 0 || localeBytes > 31) throw new DictionaryStoreError('invalid-header', 'Locale gloss has invalid locale length');
    assertZero(header, 93 + localeBytes, 128, 'locale gloss reserved header');
    let locale: string;
    try { locale = decoder.decode(header.subarray(93, 93 + localeBytes)); }
    catch { throw new DictionaryStoreError('invalid-header', 'Locale gloss locale is not valid UTF-8'); }
    const digest = [...header.subarray(60, 92)].map(value => value.toString(16).padStart(2, '0')).join('');
    if (locale !== expected.locale || digest !== expected.lexiconSha256.toLowerCase()
        || reader.manifest.entryCount !== expected.entryCount) {
      throw new DictionaryStoreError('invalid-header', 'Locale gloss binding does not match the lexicon or requested locale');
    }
    return new LocaleGlossStoreReader(reader, locale, digest);
  }
  async entry(index: number): Promise<LocaleGlossEntry> {
    const cursor = new Cursor(await this.#store.record(index));
    const seq = cursor.uint();
    const groupCount = cursor.count('Locale group count');
    const groups: LocaleGlossGroup[] = [];
    const claimed = new Set<number>();
    let previousTargets: readonly number[] | null = null;
    for (let groupIndex = 0; groupIndex < groupCount; groupIndex++) {
      const targets = Array.from({ length: cursor.count('Locale target count') }, () => cursor.uint());
      if (targets.length === 0 && groupCount !== 1) {
        throw new DictionaryStoreError('corrupt-block', 'Locale entry mixes entry-wide and aligned groups');
      }
      for (let index = 1; index < targets.length; index++) {
        if (targets[index - 1]! >= targets[index]!) throw new DictionaryStoreError('corrupt-block', 'Locale targets are not strictly increasing');
      }
      if (targets.some(target => claimed.has(target))) {
        throw new DictionaryStoreError('corrupt-block', 'Locale targets are duplicated across groups');
      }
      for (const target of targets) claimed.add(target);
      if (previousTargets && compareTargets(previousTargets, targets) >= 0) {
        throw new DictionaryStoreError('corrupt-block', 'Locale groups are not ordered');
      }
      previousTargets = targets;
      const group = {
        targets,
        glosses: decodeGlosses(cursor, 'Locale gloss count'),
        info: decodeGlosses(cursor, 'Locale info count')
      };
      validateLocalizedTexts(group.glosses, 'gloss');
      validateLocalizedTexts(group.info, 'info');
      if (group.glosses.length === 0 && group.info.length === 0) {
        throw new DictionaryStoreError('corrupt-block', 'Locale group has no localized text');
      }
      groups.push(group);
    }
    cursor.finish();
    return { seq, groups };
  }
}

function compareTargets(left: readonly number[], right: readonly number[]): number {
  for (let index = 0; index < Math.min(left.length, right.length); index++) {
    if (left[index] !== right[index]) return left[index]! - right[index]!;
  }
  return left.length - right.length;
}

function validateLocalizedTexts(values: readonly DictionaryGloss[], label: string): void {
  for (let index = 0; index < values.length; index++) {
    const value = values[index]!;
    if (value.text.length === 0 || index > 0 && values[index - 1]!.ord >= value.ord) {
      throw new DictionaryStoreError('corrupt-block', `Locale ${label} strings are empty or unordered`);
    }
  }
}

export class DictionaryReader {
  readonly lexicon: LexiconStoreReader;
  readonly locale: LocaleGlossStoreReader;
  readonly fallback: LocaleGlossStoreReader;
  constructor(lexicon: LexiconStoreReader, locale: LocaleGlossStoreReader, fallback: LocaleGlossStoreReader) {
    this.lexicon = lexicon; this.locale = locale; this.fallback = fallback;
  }
  async entry(entryIndex: number): Promise<DictionaryEntry> {
    const [lexicon, fallback] = await Promise.all([
      this.lexicon.entry(entryIndex), this.fallback.entry(entryIndex)
    ]);
    const locale = this.locale === this.fallback ? fallback : await this.locale.entry(entryIndex);
    return localizeEntry(lexicon, locale, fallback);
  }
}

export function localizeEntry(
  lexicon: LexiconEntry,
  locale: LocaleGlossEntry,
  fallback: LocaleGlossEntry
): DictionaryEntry {
  if (lexicon.seq !== locale.seq || lexicon.seq !== fallback.seq) {
    throw new DictionaryStoreError('corrupt-block', 'Dictionary entry sequence does not match across stores');
  }
  const senseOrds = new Set(lexicon.senses.map(sense => sense.ord));
  for (const entry of [locale, fallback]) {
    for (const group of entry.groups) {
      if (group.targets.some(target => !senseOrds.has(target))) {
        throw new DictionaryStoreError(
          'corrupt-block',
          'Locale group targets a sense that is absent from the lexicon entry'
        );
      }
    }
  }
  const matching = (entry: LocaleGlossEntry, ord: number) => entry.groups.filter(group => group.targets.includes(ord));
  const senses: DictionarySense[] = lexicon.senses.map(sense => {
    const selected = matching(locale, sense.ord);
    const english = matching(fallback, sense.ord);
    const gloss = selected.some(group => group.glosses.length > 0) ? selected : english;
    const info = selected.some(group => group.info.length > 0) ? selected : english;
    return {
      ord: sense.ord,
      glosses: gloss.flatMap(group => group.glosses),
      properties: [...sense.properties, ...info.flatMap(group => group.info.map(value => ({
        tag: 's_inf' as const, ord: value.ord, text: value.text
      })))]
    };
  });
  const selectedEntryWide = locale.groups.filter(group => group.targets.length === 0);
  const fallbackEntryWide = fallback.groups.filter(group => group.targets.length === 0);
  const entryWideGlosses = (
    selectedEntryWide.some(group => group.glosses.length > 0)
      ? selectedEntryWide : fallbackEntryWide
  ).flatMap(group => group.glosses);
  const entryWideInfo = (
    selectedEntryWide.some(group => group.info.length > 0)
      ? selectedEntryWide : fallbackEntryWide
  ).flatMap(group => group.info);
  let ord = lexicon.senses.reduce((maximum, sense) => Math.max(maximum, sense.ord + 1), 0);
  if (entryWideGlosses.length > 0 || entryWideInfo.length > 0) {
    senses.push({
      ord: ord++,
      glosses: entryWideGlosses,
      properties: entryWideInfo.map(value => ({ tag: 's_inf', ord: value.ord, text: value.text }))
    });
  }
  return { seq: lexicon.seq, forms: lexicon.forms, senses };
}

export function memoryDictionarySource(input: ArrayBuffer | Uint8Array): DictionaryRandomAccessSource {
  const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
  return { byteLength: bytes.byteLength, async read(offset, byteLength) { return bytes.subarray(offset, offset + byteLength); } };
}
