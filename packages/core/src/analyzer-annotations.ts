import { crc32 } from './crc32.js';
import type {
  AnalyzerSupportHint,
  AnalyzerSupportRoute,
  AnalyzerSupportSplit,
  AnalyzerSupportSplitKind,
  AnalyzerSupportSplitPart
} from './analyzer-support.js';

export const ANALYZER_ANNOTATIONS_MAGIC = 'IANAN001';
export const ANALYZER_ANNOTATIONS_FORMAT_VERSION = 4;
export const ANALYZER_ANNOTATIONS_HEADER_BYTES = 184;
export const ANALYZER_ANNOTATIONS_BLOCK_BYTES = 24;
export const ANALYZER_GENERATED_BLOCK_BYTES = 24;
export const ANALYZER_GENERATED_ROOT_BYTES = 8;
export const ANALYZER_GENERATED_RECORD_BYTES = 10;
export const ANALYZER_LOOKUP_ORDER_RECORD_BYTES = 4;
export const ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES = 16;
export const ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES = 8;
/** Pack section reserved for compressed analyzer split/hint outputs. */
export const ANALYZER_ANNOTATIONS_SECTION_ID = 5;

const LITTLE_ENDIAN = true;
const GENERATED_ALIAS_BITS = 11;
const GENERATED_ALIAS_MAX = (1 << GENERATED_ALIAS_BITS) - 2;
const GENERATED_KEY_BITS = GENERATED_ALIAS_BITS * 2;
const GENERATED_KEY_MASK = (1 << GENERATED_KEY_BITS) - 1;
const GENERATED_GROUP_MASK = (1 << 18) - 1;
const GENERATED_MEMBER_ORD_MAX = 6;
const GENERATED_VIA_MEMBER_NONE = 7;
const GENERATED_PROPERTY_NONE = 0xffff;
/** The pinned alpha has 36 compact generated blocks (9,336,624 decoded bytes). */
export const ANALYZER_GENERATED_CACHE_BLOCKS = 36;
/** Decoded split/hint definitions retained in recency order. */
export const ANALYZER_ANNOTATION_CACHE_BLOCKS = 16;
const ALIGNMENT = 8;

function align(value: number): number {
  return Math.ceil(value / ALIGNMENT) * ALIGNMENT;
}

function compareBytes(left: Uint8Array, right: Uint8Array): number {
  const shared = Math.min(left.byteLength, right.byteLength);
  for (let index = 0; index < shared; index++) {
    const difference = left[index]! - right[index]!;
    if (difference !== 0) return difference;
  }
  return left.byteLength - right.byteLength;
}

export interface AnalyzerAnnotationsRandomAccessSource {
  readonly byteLength: number;
  read(offset: number, byteLength: number): Promise<Uint8Array>;
}

export function analyzerAnnotationsMemorySource(
  input: ArrayBuffer | Uint8Array
): AnalyzerAnnotationsRandomAccessSource {
  const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
  return {
    byteLength: bytes.byteLength,
    async read(offset: number, byteLength: number): Promise<Uint8Array> {
      return bytes.subarray(offset, offset + byteLength);
    }
  };
}

export type AnalyzerAnnotationsGzipDecoder = (
  compressed: Uint8Array,
  expectedByteLength: number
) => Promise<Uint8Array>;

export interface AnalyzerAnnotationsManifest {
  readonly byteLength: number;
  readonly blocks: number;
  readonly splits: number;
  readonly hints: number;
  readonly residentIndexBytes: number;
  readonly compressedBytes: number;
  readonly uncompressedBytes: number;
  readonly largestUncompressedBlock: number;
  readonly generatedBlocks: number;
  readonly generatedRoots: number;
  readonly generatedRecords: number;
  readonly lookupOrderRecords: number;
  readonly lookupOrderRoots: number;
  readonly lookupOrderMaxRank: number;
  readonly lookupOrderExceptionSurfaces: number;
  readonly lookupOrderExceptionClasses: number;
  readonly lookupOrderExceptionLocators: number;
  readonly lookupOrderExceptionBytes: number;
  readonly generatedPhysicalGroups: number;
  readonly generatedFactPairs: number;
  readonly generatedCompressedBytes: number;
  readonly generatedUncompressedBytes: number;
  readonly largestGeneratedBlock: number;
  readonly largestGeneratedCompressedBlock: number;
}

export interface AnalyzerGeneratedFacts {
  /** Null means the generated target has the same count as its lexical root. */
  readonly nKanji: number | null;
  readonly nKana: number | null;
  /** Dense pack-local target identity; null means this path is physically unique. */
  readonly physicalGroup: number | null;
  /** Null is the compact count-only case; otherwise every physical conj_prop row. */
  readonly members: readonly AnalyzerGeneratedMember[] | null;
}

export interface AnalyzerGeneratedMember {
  readonly property: {
    /** Index in the morphology artifact's canonical positions array. */
    readonly posId: number;
    readonly type: number;
    readonly negative: boolean | null;
    readonly formal: boolean | null;
  };
  readonly memberOrd: number;
  /** Stable conj_prop order within memberOrd. */
  readonly propOrd: number;
  /** Exact prefix member selected by a two-stage physical row. */
  readonly viaMemberOrd: number | null;
}

export class AnalyzerAnnotationsError extends Error {
  readonly code: 'invalid-header' | 'corrupt-index' | 'corrupt-block' | 'out-of-range';

  constructor(code: AnalyzerAnnotationsError['code'], message: string) {
    super(message);
    this.name = 'AnalyzerAnnotationsError';
    this.code = code;
  }
}

interface Utf8Decoder {
  decode(input: Uint8Array): string;
}

interface Utf8DecoderConstructor {
  new(label: string, options: { fatal: boolean }): Utf8Decoder;
}

const UTF8 = new (
  globalThis as unknown as { TextDecoder: Utf8DecoderConstructor }
).TextDecoder('utf-8', { fatal: true });

function route(code: unknown): AnalyzerSupportRoute {
  if (code === 0) return 'kana';
  if (code === 1) return 'kanji';
  throw new AnalyzerAnnotationsError('corrupt-block', `Invalid annotation route ${String(code)}`);
}

function uint(value: unknown, label: string, max = 0xffff_ffff): number {
  if (!Number.isSafeInteger(value) || (value as number) < 0 || (value as number) > max) {
    throw new AnalyzerAnnotationsError('corrupt-block', `${label} is not an unsigned integer`);
  }
  return value as number;
}

function sint32(value: unknown, label: string): number {
  if (!Number.isSafeInteger(value) || (value as number) < -0x8000_0000 || (value as number) > 0x7fff_ffff) {
    throw new AnalyzerAnnotationsError('corrupt-block', `${label} is not a signed 32-bit integer`);
  }
  return value as number;
}

function text(value: unknown, label: string): string {
  if (typeof value !== 'string') throw new AnalyzerAnnotationsError('corrupt-block', `${label} is not text`);
  return value;
}

function nullableText(value: unknown, label: string): string | null {
  if (value === null) return null;
  return text(value, label);
}

function nullableUint(value: unknown, label: string): number | null {
  if (value === null) return null;
  return uint(value, label, 0xfe);
}

function nullableBoolean(value: unknown, label: string): boolean | null {
  if (value === null || typeof value === 'boolean') return value;
  throw new AnalyzerAnnotationsError('corrupt-block', `${label} is not a nullable boolean`);
}

function hasMagic(bytes: Uint8Array): boolean {
  if (bytes.byteLength < ANALYZER_ANNOTATIONS_MAGIC.length) return false;
  for (let index = 0; index < ANALYZER_ANNOTATIONS_MAGIC.length; index++) {
    if (bytes[index] !== ANALYZER_ANNOTATIONS_MAGIC.charCodeAt(index)) return false;
  }
  return true;
}

async function readExact(
  source: AnalyzerAnnotationsRandomAccessSource,
  offset: number,
  byteLength: number,
  code: AnalyzerAnnotationsError['code']
): Promise<Uint8Array> {
  if (
    !Number.isSafeInteger(offset) || !Number.isSafeInteger(byteLength)
    || offset < 0 || byteLength < 0 || offset + byteLength > source.byteLength
  ) throw new AnalyzerAnnotationsError(code, 'Annotation read lies outside the store');
  const bytes = await source.read(offset, byteLength);
  if (bytes.byteLength !== byteLength) throw new AnalyzerAnnotationsError(code, 'Annotation read was truncated');
  return bytes;
}

function part(value: unknown): AnalyzerSupportSplitPart {
  if (value === 1) return ':score';
  if (value === 2) return ':pscore';
  if (!Array.isArray(value) || value.length !== 10 || value[0] !== 0) {
    throw new AnalyzerAnnotationsError('corrupt-block', 'Invalid split-part tuple');
  }
  const flags = uint(value[8], 'Split-part flags', 3);
  const generatedValue = value[9];
  if (generatedValue !== null && !Array.isArray(generatedValue)) {
    throw new AnalyzerAnnotationsError('corrupt-block', 'Invalid split-part generated locator');
  }
  const generated = generatedValue === null ? null : generatedValue.map((
    entry: unknown,
    index: number
  ) => {
    if (!Array.isArray(entry) || entry.length !== 6) {
      throw new AnalyzerAnnotationsError(
        'corrupt-block', `Invalid split-part generated locator ${index}`
      );
    }
    return {
      from: uint(entry[0], `Split-part generated from ${index}`),
      via: uint(entry[1], `Split-part generated via ${index}`, 1) === 1,
      pos: text(entry[2], `Split-part generated POS ${index}`),
      type: uint(entry[3], `Split-part generated type ${index}`, 0xffff),
      negative: nullableBoolean(entry[4], `Split-part generated negative ${index}`),
      formal: nullableBoolean(entry[5], `Split-part generated formal ${index}`)
    };
  });
  return {
    route: route(value[1]),
    seq: uint(value[2], 'Split-part seq'),
    text: text(value[3], 'Split-part text'),
    best: nullableText(value[4], 'Split-part best'),
    ord: uint(value[5], 'Split-part ordinal', 0xffff),
    common: nullableUint(value[6], 'Split-part common'),
    commonTags: text(value[7], 'Split-part common tags'),
    conjugatable: (flags & 1) !== 0,
    nokanji: (flags & 2) !== 0,
    generated
  };
}

function split(seq: number, value: unknown): AnalyzerSupportSplit {
  if (!Array.isArray(value) || value.length !== 8 || !Array.isArray(value[3]) || !Array.isArray(value[7])) {
    throw new AnalyzerAnnotationsError('corrupt-block', 'Invalid split tuple');
  }
  const kindCode = uint(value[2], 'Split kind', 1);
  return {
    definitionSeq: seq,
    route: route(value[0]),
    surface: text(value[1], 'Split surface'),
    kind: kindCode === 0 ? 'split' : 'segsplit',
    parts: value[3].map(part),
    score: sint32(value[4], 'Split score'),
    primary: uint(value[5], 'Split primary', 0xff),
    connector: text(value[6], 'Split connector'),
    root: value[7].map((entry, index) => uint(entry, `Split root ${index}`))
  };
}

function hint(seq: number, value: unknown): AnalyzerSupportHint {
  if (!Array.isArray(value) || value.length !== 4) {
    throw new AnalyzerAnnotationsError('corrupt-block', 'Invalid hint tuple');
  }
  return {
    definitionSeq: seq,
    route: route(value[0]),
    surface: text(value[1], 'Hint surface'),
    reading: text(value[2], 'Hint reading'),
    hint: text(value[3], 'Hint value')
  };
}

interface DecodedBlock {
  seq: number;
  splits: AnalyzerSupportSplit[];
  hints: AnalyzerSupportHint[];
}

interface DecodedGeneratedBlock {
  readonly blockIndex: number;
  readonly bytes: Uint8Array;
  readonly view: DataView;
  readonly roots: number;
  readonly records: number;
  readonly recordsOffset: number;
  readonly orders: number;
  readonly ordersOffset: number;
  readonly facts: Uint8Array;
  readonly physicalGroups: number;
}

function generatedKey(aliases: readonly [number] | readonly [number, number]): number {
  const [first, second] = aliases;
  if (!Number.isInteger(first) || first < 0 || first > GENERATED_ALIAS_MAX
    || (second !== undefined
      && (!Number.isInteger(second) || second < 0 || second > GENERATED_ALIAS_MAX))) {
    throw new AnalyzerAnnotationsError('out-of-range', 'Generated alias lies outside the packed range');
  }
  const key = (first << GENERATED_ALIAS_BITS) | (second === undefined ? 0 : second + 1);
  if (key === GENERATED_KEY_MASK) {
    throw new AnalyzerAnnotationsError(
      'out-of-range', 'Generated key collides with the direct lookup-order sentinel'
    );
  }
  return key;
}

interface LookupOrderExceptionSpan {
  readonly first: number;
  readonly count: number;
}

function lookupOrderExceptionKey(routeValue: AnalyzerSupportRoute, surface: string): string {
  return `${routeValue}\u0000${surface}`;
}

function exceptionLookupOrder(
  exceptions: ReadonlyMap<string, LookupOrderExceptionSpan>,
  index: DataView,
  locatorsOffset: number,
  routeValue: AnalyzerSupportRoute,
  surface: string,
  rootSeq: number,
  key: number
): number | null | undefined {
  const span = exceptions.get(lookupOrderExceptionKey(routeValue, surface));
  if (!span) return undefined;
  let low = 0;
  let high = span.count;
  while (low < high) {
    const middle = (low + high) >>> 1;
    const at = locatorsOffset
      + (span.first + middle) * ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES;
    const foundRoot = index.getUint32(at, LITTLE_ENDIAN);
    const foundKey = index.getUint32(at + 4, LITTLE_ENDIAN) & GENERATED_KEY_MASK;
    if (foundRoot < rootSeq || (foundRoot === rootSeq && foundKey < key)) low = middle + 1;
    else high = middle;
  }
  if (low >= span.count) return null;
  const at = locatorsOffset
    + (span.first + low) * ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES;
  const packed = index.getUint32(at + 4, LITTLE_ENDIAN);
  if (index.getUint32(at, LITTLE_ENDIAN) !== rootSeq
    || (packed & GENERATED_KEY_MASK) !== key) return null;
  return (packed >>> GENERATED_KEY_BITS) & 0x3f;
}

function u24(view: DataView, offset: number): number {
  return view.getUint8(offset)
    | (view.getUint8(offset + 1) << 8)
    | (view.getUint8(offset + 2) << 16);
}

function decodedGeneratedFact(
  block: DecodedGeneratedBlock,
  firstRecord: number,
  recordCount: number,
  wanted: number
): AnalyzerGeneratedFacts {
  const members: AnalyzerGeneratedMember[] = [];
  let factCode: number | null = null;
  let physicalGroup: number | null = null;
  let countOnly = false;
  for (let index = 0; index < recordCount; index++) {
    const at = block.recordsOffset
      + (firstRecord + index) * ANALYZER_GENERATED_RECORD_BYTES;
    const storedKey = block.view.getUint32(at, LITTLE_ENDIAN);
    if ((storedKey & GENERATED_KEY_MASK) !== wanted) break;
    const fact = block.view.getUint8(at + 4);
    const physical = u24(block.view, at + 5);
    const property = block.view.getUint16(at + 8, LITTLE_ENDIAN);
    if (factCode === null) {
      factCode = fact;
      const group = physical & GENERATED_GROUP_MASK;
      physicalGroup = group === 0 ? null : group;
    }
    if (property === GENERATED_PROPERTY_NONE) {
      countOnly = true;
      continue;
    }
    const viaMemberOrd = (physical >>> 21) & 7;
    const negative = (property >>> 11) & 3;
    const formal = (property >>> 13) & 3;
    members.push({
      property: {
        posId: property & 31,
        type: (property >>> 5) & 63,
        negative: negative === 2 ? null : negative === 1,
        formal: formal === 2 ? null : formal === 1
      },
      memberOrd: (physical >>> 18) & 7,
      propOrd: storedKey >>> GENERATED_KEY_BITS,
      viaMemberOrd: viaMemberOrd === GENERATED_VIA_MEMBER_NONE ? null : viaMemberOrd
    });
  }
  const fact = factCode ?? 0;
  return {
    nKanji: fact === 0 ? null : block.facts[(fact - 1) * 2]!,
    nKana: fact === 0 ? null : block.facts[(fact - 1) * 2 + 1]!,
    physicalGroup,
    members: countOnly ? null : members
  };
}

interface GeneratedRootLocation {
  readonly firstRecord: number;
  readonly recordCount: number;
  readonly firstOrder: number;
  readonly orderCount: number;
}

function generatedRootLocation(
  block: DecodedGeneratedBlock,
  rootSeq: number
): GeneratedRootLocation | null {
  let low = 0;
  let high = block.roots;
  while (low < high) {
    const middle = (low + high) >>> 1;
    const seq = block.view.getUint32(12 + middle * 20, LITTLE_ENDIAN);
    if (seq < rootSeq) low = middle + 1;
    else high = middle;
  }
  if (low >= block.roots
    || block.view.getUint32(12 + low * 20, LITTLE_ENDIAN) !== rootSeq) return null;
  const at = 12 + low * 20;
  return {
    firstRecord: block.view.getUint32(at + 4, LITTLE_ENDIAN),
    recordCount: block.view.getUint32(at + 8, LITTLE_ENDIAN),
    firstOrder: block.view.getUint32(at + 12, LITTLE_ENDIAN),
    orderCount: block.view.getUint32(at + 16, LITTLE_ENDIAN)
  };
}

function generatedFact(
  block: DecodedGeneratedBlock,
  rootSeq: number,
  aliases: readonly [number] | readonly [number, number]
): AnalyzerGeneratedFacts | null {
  const location = generatedRootLocation(block, rootSeq);
  if (location === null) return null;
  const { firstRecord, recordCount } = location;
  const wanted = generatedKey(aliases);
  let recordLow = 0;
  let recordHigh = recordCount;
  while (recordLow < recordHigh) {
    const middle = (recordLow + recordHigh) >>> 1;
    const at = block.recordsOffset
      + (firstRecord + middle) * ANALYZER_GENERATED_RECORD_BYTES;
    const key = block.view.getUint32(at, LITTLE_ENDIAN) & GENERATED_KEY_MASK;
    if (key < wanted) recordLow = middle + 1;
    else recordHigh = middle;
  }
  if (recordLow >= recordCount) return null;
  const at = block.recordsOffset
    + (firstRecord + recordLow) * ANALYZER_GENERATED_RECORD_BYTES;
  const storedKey = block.view.getUint32(at, LITTLE_ENDIAN);
  if ((storedKey & GENERATED_KEY_MASK) !== wanted) return null;
  return decodedGeneratedFact(
    block,
    firstRecord + recordLow,
    recordCount - recordLow,
    wanted
  );
}

function generatedOrder(
  block: DecodedGeneratedBlock,
  rootSeq: number,
  wanted: number
): number | null {
  const location = generatedRootLocation(block, rootSeq);
  if (location === null) {
    throw new AnalyzerAnnotationsError(
      'corrupt-index',
      `Generated block ${block.blockIndex} does not contain indexed root ${rootSeq}`
    );
  }
  let low = 0;
  let high = location.orderCount;
  while (low < high) {
    const middle = (low + high) >>> 1;
    const at = block.ordersOffset
      + (location.firstOrder + middle) * ANALYZER_LOOKUP_ORDER_RECORD_BYTES;
    const key = block.view.getUint32(at, LITTLE_ENDIAN) & GENERATED_KEY_MASK;
    if (key < wanted) low = middle + 1;
    else high = middle;
  }
  if (low >= location.orderCount) return null;
  const at = block.ordersOffset
    + (location.firstOrder + low) * ANALYZER_LOOKUP_ORDER_RECORD_BYTES;
  const packed = block.view.getUint32(at, LITTLE_ENDIAN);
  return (packed & GENERATED_KEY_MASK) === wanted
    ? (packed >>> GENERATED_KEY_BITS) & 0x3f
    : null;
}

export class AnalyzerAnnotationNotLoadedError extends Error {
  readonly kind: 'annotation' | 'generated';
  /** Definition seq for annotations; triggering root seq for generated facts. */
  readonly definitionSeq: number;
  readonly blockIndex: number | null;

  constructor(
    definitionSeq: number,
    kind: 'annotation' | 'generated' = 'annotation',
    blockIndex: number | null = null
  ) {
    super(kind === 'annotation'
      ? `Analyzer annotation definition ${definitionSeq} is not loaded`
      : `Analyzer generated block ${blockIndex} for root ${definitionSeq} is not loaded`);
    this.name = 'AnalyzerAnnotationNotLoadedError';
    this.kind = kind;
    this.definitionSeq = definitionSeq;
    this.blockIndex = blockIndex;
  }
}

/**
 * Records the cold analyzer facts touched by a discarded discovery pass.
 *
 * Returning null deliberately keeps discovery synchronous. The real analyzer
 * runs again after these dependencies are batch-preloaded, so only that exact
 * pass can produce a public result.
 */
export class AnalyzerAnnotationDependencyCollector {
  readonly #definitionSeqs = new Set<number>();
  readonly #generatedRootSeqs = new Set<number>();

  get definitionSeqs(): ReadonlySet<number> {
    return this.#definitionSeqs;
  }

  get generatedRootSeqs(): ReadonlySet<number> {
    return this.#generatedRootSeqs;
  }

  split(
    definitionSeq: number,
    _routeValue: AnalyzerSupportRoute,
    _surface: string,
    _kind: AnalyzerSupportSplitKind = 'split'
  ): null {
    this.#definitionSeqs.add(definitionSeq);
    return null;
  }

  hint(
    definitionSeq: number,
    _routeValue: AnalyzerSupportRoute,
    _surface: string,
    _reading: string
  ): null {
    this.#definitionSeqs.add(definitionSeq);
    return null;
  }

  generated(
    rootSeq: number,
    _aliases: readonly [number] | readonly [number, number]
  ): null {
    this.#generatedRootSeqs.add(rootSeq);
    return null;
  }

  lookupOrder(
    _routeValue: AnalyzerSupportRoute,
    _surface: string,
    rootSeq: number,
    _aliases: readonly [number] | readonly [number, number] | null
  ): number {
    this.#generatedRootSeqs.add(rootSeq);
    // Discovery output is discarded after dependencies are preloaded. A
    // concrete placeholder lets exact completeness checks reach every root.
    return 0;
  }
}

/** Mutable synchronous view used by one retryable Worker analysis request. */
export class PreloadedAnalyzerAnnotations {
  readonly #known: ReadonlySet<number>;
  readonly #load: (seq: number) => Promise<DecodedBlock | null>;
  readonly #generatedBlockForRoot: (seq: number) => number | null;
  readonly #loadGeneratedBlocks: (
    blockIndexes: readonly number[]
  ) => Promise<ReadonlyMap<number, DecodedGeneratedBlock>>;
  readonly #lookupException: (
    routeValue: AnalyzerSupportRoute,
    surface: string,
    rootSeq: number,
    key: number
  ) => number | null | undefined;
  readonly #blocks: Map<number, DecodedBlock>;
  readonly #generatedBlocks: Map<number, DecodedGeneratedBlock>;
  readonly #preloadedGeneratedRoots = new Set<number>();

  constructor(
    known: ReadonlySet<number>,
    load: (seq: number) => Promise<DecodedBlock | null>,
    generatedBlockForRoot: (seq: number) => number | null,
    loadGeneratedBlocks: (
      blockIndexes: readonly number[]
    ) => Promise<ReadonlyMap<number, DecodedGeneratedBlock>>,
    lookupException: (
      routeValue: AnalyzerSupportRoute,
      surface: string,
      rootSeq: number,
      key: number
    ) => number | null | undefined,
    blocks: ReadonlyMap<number, DecodedBlock>,
    generatedBlocks: ReadonlyMap<number, DecodedGeneratedBlock>
  ) {
    this.#known = known;
    this.#load = load;
    this.#generatedBlockForRoot = generatedBlockForRoot;
    this.#loadGeneratedBlocks = loadGeneratedBlocks;
    this.#lookupException = lookupException;
    // Each request owns its maps. The decoded values are immutable and shared
    // with the Reader caches, while clear() cannot mutate those caches.
    this.#blocks = new Map(blocks);
    this.#generatedBlocks = new Map(generatedBlocks);
  }

  get loadedBlocks(): number {
    return this.#blocks.size;
  }

  get loadedGeneratedRoots(): number {
    return this.#preloadedGeneratedRoots.size;
  }

  get loadedGeneratedBlocks(): number {
    return this.#generatedBlocks.size;
  }

  async preload(definitionSeqs: Iterable<number>): Promise<void> {
    const seqs = [...new Set(definitionSeqs)]
      .filter(seq => this.#known.has(seq) && !this.#blocks.has(seq))
      .sort((left, right) => left - right);
    for (const seq of seqs) {
      const block = await this.#load(seq);
      if (block) this.#blocks.set(seq, block);
    }
  }

  async preloadGenerated(rootSeqs: Iterable<number>): Promise<void> {
    const seqs = [...new Set(rootSeqs)]
      .filter(seq => !this.#preloadedGeneratedRoots.has(seq))
      .sort((left, right) => left - right);
    const blockIndexes = [...new Set(seqs
      .map(seq => this.#generatedBlockForRoot(seq))
      .filter((block): block is number => block !== null && !this.#generatedBlocks.has(block)))]
      .sort((left, right) => left - right);
    const loaded = await this.#loadGeneratedBlocks(blockIndexes);
    for (const [index, block] of loaded) this.#generatedBlocks.set(index, block);
    for (const seq of seqs) {
      const blockIndex = this.#generatedBlockForRoot(seq);
      if (blockIndex !== null && this.#generatedBlocks.has(blockIndex)) {
        this.#preloadedGeneratedRoots.add(seq);
      }
    }
  }

  async preloadMissing(error: AnalyzerAnnotationNotLoadedError): Promise<void> {
    if (error.kind === 'annotation') {
      await this.preload([error.definitionSeq]);
    } else {
      await this.preloadGenerated([error.definitionSeq]);
    }
  }

  async preloadDependencies(
    dependencies: AnalyzerAnnotationDependencyCollector
  ): Promise<void> {
    await this.preload(dependencies.definitionSeqs);
    await this.preloadGenerated(dependencies.generatedRootSeqs);
  }

  clear(): void {
    this.#blocks.clear();
    this.#generatedBlocks.clear();
    this.#preloadedGeneratedRoots.clear();
  }

  #block(seq: number): DecodedBlock | null {
    const block = this.#blocks.get(seq);
    if (block) return block;
    if (this.#known.has(seq)) throw new AnalyzerAnnotationNotLoadedError(seq);
    return null;
  }

  split(
    definitionSeq: number,
    routeValue: AnalyzerSupportRoute,
    surface: string,
    kind: AnalyzerSupportSplitKind = 'split'
  ): AnalyzerSupportSplit | null {
    return this.#block(definitionSeq)?.splits.find(value =>
      value.route === routeValue && value.surface === surface && value.kind === kind) ?? null;
  }

  hint(
    definitionSeq: number,
    routeValue: AnalyzerSupportRoute,
    surface: string,
    reading: string
  ): string | null {
    return this.#block(definitionSeq)?.hints.find(value =>
      value.route === routeValue && value.surface === surface && value.reading === reading)?.hint ?? null;
  }

  generated(
    rootSeq: number,
    aliases: readonly [number] | readonly [number, number]
  ): AnalyzerGeneratedFacts | null {
    const blockIndex = this.#generatedBlockForRoot(rootSeq);
    if (blockIndex === null) return null;
    const block = this.#generatedBlocks.get(blockIndex);
    if (!block) throw new AnalyzerAnnotationNotLoadedError(rootSeq, 'generated', blockIndex);
    return generatedFact(block, rootSeq, aliases);
  }

  lookupOrder(
    routeValue: AnalyzerSupportRoute,
    surface: string,
    rootSeq: number,
    aliases: readonly [number] | readonly [number, number] | null
  ): number | null {
    const key = aliases === null ? GENERATED_KEY_MASK : generatedKey(aliases);
    const exception = this.#lookupException(routeValue, surface, rootSeq, key);
    if (exception !== undefined) return exception;
    const blockIndex = this.#generatedBlockForRoot(rootSeq);
    if (blockIndex === null) return null;
    const block = this.#generatedBlocks.get(blockIndex);
    if (!block) throw new AnalyzerAnnotationNotLoadedError(rootSeq, 'generated', blockIndex);
    return generatedOrder(block, rootSeq, key);
  }
}

export class AnalyzerAnnotationsReader {
  readonly manifest: AnalyzerAnnotationsManifest;

  readonly #source: AnalyzerAnnotationsRandomAccessSource;
  readonly #decode: AnalyzerAnnotationsGzipDecoder;
  readonly #index: DataView;
  readonly #generatedBlocksOffset: number;
  readonly #generatedRootsOffset: number;
  readonly #generatedFactsOffset: number;
  readonly #exceptionLocatorsOffset: number;
  readonly #lookupOrderExceptions: ReadonlyMap<string, LookupOrderExceptionSpan>;
  readonly #annotationDataOffset: number;
  readonly #generatedDataOffset: number;
  readonly #generatedCompressedBytes: number;
  readonly #generatedFacts: Uint8Array;
  readonly #knownDefinitions: ReadonlySet<number>;
  readonly #annotationCache = new Map<number, DecodedBlock>();
  readonly #generatedCache = new Map<number, DecodedGeneratedBlock>();

  private constructor(
    source: AnalyzerAnnotationsRandomAccessSource,
    decode: AnalyzerAnnotationsGzipDecoder,
    manifest: AnalyzerAnnotationsManifest,
    indexBytes: Uint8Array,
    generatedBlocksOffset: number,
    generatedRootsOffset: number,
    generatedFactsOffset: number,
    exceptionLocatorsOffset: number,
    lookupOrderExceptions: ReadonlyMap<string, LookupOrderExceptionSpan>,
    annotationDataOffset: number,
    generatedDataOffset: number,
    generatedCompressedBytes: number
  ) {
    this.#source = source;
    this.#decode = decode;
    this.manifest = manifest;
    this.#index = new DataView(indexBytes.buffer, indexBytes.byteOffset, indexBytes.byteLength);
    this.#generatedBlocksOffset = generatedBlocksOffset - ANALYZER_ANNOTATIONS_HEADER_BYTES;
    this.#generatedRootsOffset = generatedRootsOffset - ANALYZER_ANNOTATIONS_HEADER_BYTES;
    this.#generatedFactsOffset = generatedFactsOffset - ANALYZER_ANNOTATIONS_HEADER_BYTES;
    this.#exceptionLocatorsOffset = exceptionLocatorsOffset - ANALYZER_ANNOTATIONS_HEADER_BYTES;
    this.#lookupOrderExceptions = lookupOrderExceptions;
    this.#annotationDataOffset = annotationDataOffset;
    this.#generatedDataOffset = generatedDataOffset;
    this.#generatedCompressedBytes = generatedCompressedBytes;
    this.#generatedFacts = indexBytes.subarray(
      this.#generatedFactsOffset,
      this.#generatedFactsOffset + manifest.generatedFactPairs * 2
    );
    this.#knownDefinitions = new Set(this.definitionSeqs());
  }

  static async open(
    source: AnalyzerAnnotationsRandomAccessSource,
    decode: AnalyzerAnnotationsGzipDecoder
  ): Promise<AnalyzerAnnotationsReader> {
    const header = await readExact(source, 0, ANALYZER_ANNOTATIONS_HEADER_BYTES, 'invalid-header');
    if (!hasMagic(header)) throw new AnalyzerAnnotationsError('invalid-header', 'Invalid annotation magic');
    const view = new DataView(header.buffer, header.byteOffset, header.byteLength);
    if (
      view.getUint16(8, LITTLE_ENDIAN) !== ANALYZER_ANNOTATIONS_FORMAT_VERSION
      || view.getUint16(10, LITTLE_ENDIAN) !== ANALYZER_ANNOTATIONS_HEADER_BYTES
      || view.getUint32(12, LITTLE_ENDIAN) !== source.byteLength
    ) throw new AnalyzerAnnotationsError('invalid-header', 'Invalid annotation header fields');
    const headerCopy = header.slice();
    new DataView(headerCopy.buffer).setUint32(16, 0, LITTLE_ENDIAN);
    if (crc32(headerCopy) !== view.getUint32(16, LITTLE_ENDIAN)) {
      throw new AnalyzerAnnotationsError('invalid-header', 'Annotation header checksum mismatch');
    }
    const blocks = view.getUint32(24, LITTLE_ENDIAN);
    const splits = view.getUint32(28, LITTLE_ENDIAN);
    const hints = view.getUint32(32, LITTLE_ENDIAN);
    const generatedBlocks = view.getUint32(52, LITTLE_ENDIAN);
    const generatedRoots = view.getUint32(56, LITTLE_ENDIAN);
    const generatedRecords = view.getUint32(60, LITTLE_ENDIAN);
    const generatedPhysicalGroups = view.getUint32(64, LITTLE_ENDIAN);
    const generatedFactPairs = view.getUint32(68, LITTLE_ENDIAN);
    const lookupOrderRecords = view.getUint32(128, LITTLE_ENDIAN);
    const lookupOrderRoots = view.getUint32(132, LITTLE_ENDIAN);
    const lookupOrderMaxRank = view.getUint32(136, LITTLE_ENDIAN);
    const lookupOrderExceptionSurfaces = view.getUint32(144, LITTLE_ENDIAN);
    const lookupOrderExceptionLocators = view.getUint32(148, LITTLE_ENDIAN);
    const lookupOrderExceptionClasses = view.getUint32(152, LITTLE_ENDIAN);
    const lookupOrderExceptionMaxRank = view.getUint32(156, LITTLE_ENDIAN);
    if (
      view.getUint32(36, LITTLE_ENDIAN) !== ANALYZER_ANNOTATIONS_BLOCK_BYTES
      || view.getUint32(72, LITTLE_ENDIAN) !== ANALYZER_GENERATED_BLOCK_BYTES
      || view.getUint32(76, LITTLE_ENDIAN) !== ANALYZER_GENERATED_ROOT_BYTES
      || view.getUint32(80, LITTLE_ENDIAN) !== ANALYZER_GENERATED_RECORD_BYTES
      || view.getUint32(120, LITTLE_ENDIAN) !== 256 * 1024
      || view.getUint32(140, LITTLE_ENDIAN) !== ANALYZER_LOOKUP_ORDER_RECORD_BYTES
      || view.getUint32(160, LITTLE_ENDIAN) !== ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES
      || view.getUint32(164, LITTLE_ENDIAN)
        !== ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES
      || lookupOrderMaxRank > 0x3f
      || lookupOrderExceptionMaxRank > 0x3f
      || lookupOrderRoots > generatedRoots
      || (lookupOrderRecords === 0 && (lookupOrderRoots !== 0 || lookupOrderMaxRank !== 0))
      || (lookupOrderExceptionSurfaces === 0
        && (lookupOrderExceptionLocators !== 0
          || lookupOrderExceptionClasses !== 0
          || lookupOrderExceptionMaxRank !== 0))
    ) {
      throw new AnalyzerAnnotationsError('invalid-header', 'Invalid annotation block stride');
    }
    const blocksOffset = view.getUint32(40, LITTLE_ENDIAN);
    const annotationDataOffset = view.getUint32(44, LITTLE_ENDIAN);
    const annotationCompressedBytes = view.getUint32(48, LITTLE_ENDIAN);
    const generatedBlocksOffset = view.getUint32(84, LITTLE_ENDIAN);
    const generatedRootsOffset = view.getUint32(88, LITTLE_ENDIAN);
    const generatedFactsOffset = view.getUint32(92, LITTLE_ENDIAN);
    const generatedDataOffset = view.getUint32(96, LITTLE_ENDIAN);
    const generatedCompressedBytes = view.getUint32(100, LITTLE_ENDIAN);
    const generatedUncompressedBytes = view.getUint32(104, LITTLE_ENDIAN);
    const largestGeneratedBlock = view.getUint32(108, LITTLE_ENDIAN);
    const annotationUncompressedBytes = view.getUint32(112, LITTLE_ENDIAN);
    const largestAnnotationBlock = view.getUint32(116, LITTLE_ENDIAN);
    const largestGeneratedCompressedBlock = view.getUint32(124, LITTLE_ENDIAN);
    const exceptionEntriesOffset = view.getUint32(168, LITTLE_ENDIAN);
    const exceptionLocatorsOffset = view.getUint32(172, LITTLE_ENDIAN);
    const exceptionStringsOffset = view.getUint32(176, LITTLE_ENDIAN);
    const exceptionStringBytes = view.getUint32(180, LITTLE_ENDIAN);
    const expectedGeneratedBlocksOffset = blocksOffset
      + blocks * ANALYZER_ANNOTATIONS_BLOCK_BYTES;
    const expectedGeneratedRootsOffset = expectedGeneratedBlocksOffset
      + generatedBlocks * ANALYZER_GENERATED_BLOCK_BYTES;
    const expectedGeneratedFactsOffset = expectedGeneratedRootsOffset
      + generatedRoots * ANALYZER_GENERATED_ROOT_BYTES;
    const expectedExceptionEntriesOffset = align(
      expectedGeneratedFactsOffset + generatedFactPairs * 2
    );
    const expectedExceptionLocatorsOffset = expectedExceptionEntriesOffset
      + lookupOrderExceptionSurfaces * ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES;
    const expectedExceptionStringsOffset = expectedExceptionLocatorsOffset
      + lookupOrderExceptionLocators * ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES;
    const expectedAnnotationDataOffset = align(expectedExceptionStringsOffset + exceptionStringBytes);
    const expectedGeneratedDataOffset = align(annotationDataOffset + annotationCompressedBytes);
    if (
      blocksOffset !== ANALYZER_ANNOTATIONS_HEADER_BYTES
      || generatedBlocksOffset !== expectedGeneratedBlocksOffset
      || generatedRootsOffset !== expectedGeneratedRootsOffset
      || generatedFactsOffset !== expectedGeneratedFactsOffset
      || exceptionEntriesOffset !== expectedExceptionEntriesOffset
      || exceptionLocatorsOffset !== expectedExceptionLocatorsOffset
      || exceptionStringsOffset !== expectedExceptionStringsOffset
      || annotationDataOffset !== expectedAnnotationDataOffset
      || generatedDataOffset !== expectedGeneratedDataOffset
      || align(generatedDataOffset + generatedCompressedBytes) !== source.byteLength
    ) throw new AnalyzerAnnotationsError('invalid-header', 'Invalid annotation layout');
    const index = await readExact(
      source,
      blocksOffset,
      annotationDataOffset - blocksOffset,
      'corrupt-index'
    );
    if (crc32(index) !== view.getUint32(20, LITTLE_ENDIAN)) {
      throw new AnalyzerAnnotationsError('corrupt-index', 'Annotation index checksum mismatch');
    }
    const indexView = new DataView(index.buffer, index.byteOffset, index.byteLength);
    let previousSeq = 0;
    let previousEnd = 0;
    let splitTotal = 0;
    let hintTotal = 0;
    let annotationUncompressedTotal = 0;
    let annotationLargest = 0;
    for (let block = 0; block < blocks; block++) {
      const at = block * ANALYZER_ANNOTATIONS_BLOCK_BYTES;
      const seq = indexView.getUint32(at, LITTLE_ENDIAN);
      const offset = indexView.getUint32(at + 4, LITTLE_ENDIAN);
      const compressed = indexView.getUint32(at + 8, LITTLE_ENDIAN);
      const uncompressed = indexView.getUint32(at + 12, LITTLE_ENDIAN);
      if ((block > 0 && seq <= previousSeq)
        || offset !== previousEnd
        || offset + compressed > annotationCompressedBytes) {
        throw new AnalyzerAnnotationsError('corrupt-index', 'Annotation block index is non-canonical');
      }
      previousSeq = seq;
      previousEnd = offset + compressed;
      splitTotal += indexView.getUint16(at + 20, LITTLE_ENDIAN);
      hintTotal += indexView.getUint16(at + 22, LITTLE_ENDIAN);
      annotationUncompressedTotal += uncompressed;
      annotationLargest = Math.max(annotationLargest, uncompressed);
    }
    if (
      previousEnd !== annotationCompressedBytes
      || splitTotal !== splits
      || hintTotal !== hints
      || annotationUncompressedTotal !== annotationUncompressedBytes
      || annotationLargest !== largestAnnotationBlock
    ) {
      throw new AnalyzerAnnotationsError('corrupt-index', 'Annotation block totals disagree with header');
    }

    const generatedBlockRelative = generatedBlocksOffset - blocksOffset;
    previousSeq = 0;
    previousEnd = 0;
    let generatedRootTotal = 0;
    let generatedUncompressedTotal = 0;
    let generatedLargest = 0;
    let generatedCompressedLargest = 0;
    let lookupOrderTotal = 0;
    const rootsPerBlock = new Uint32Array(generatedBlocks);
    for (let block = 0; block < generatedBlocks; block++) {
      const at = generatedBlockRelative + block * ANALYZER_GENERATED_BLOCK_BYTES;
      const firstRoot = indexView.getUint32(at, LITTLE_ENDIAN);
      const offset = indexView.getUint32(at + 4, LITTLE_ENDIAN);
      const compressed = indexView.getUint32(at + 8, LITTLE_ENDIAN);
      const uncompressed = indexView.getUint32(at + 12, LITTLE_ENDIAN);
      const rootCount = indexView.getUint16(at + 20, LITTLE_ENDIAN);
      const orderCount = indexView.getUint16(at + 22, LITTLE_ENDIAN);
      if (
        rootCount === 0
        || (block > 0 && firstRoot <= previousSeq)
        || offset !== previousEnd
        || offset + compressed > generatedCompressedBytes
      ) throw new AnalyzerAnnotationsError('corrupt-index', 'Generated block index is non-canonical');
      previousSeq = firstRoot;
      previousEnd = offset + compressed;
      generatedRootTotal += rootCount;
      generatedUncompressedTotal += uncompressed;
      generatedLargest = Math.max(generatedLargest, uncompressed);
      generatedCompressedLargest = Math.max(generatedCompressedLargest, compressed);
      lookupOrderTotal += orderCount;
      rootsPerBlock[block] = rootCount;
    }
    if (
      previousEnd !== generatedCompressedBytes
      || generatedRootTotal !== generatedRoots
      || generatedUncompressedTotal !== generatedUncompressedBytes
      || generatedLargest !== largestGeneratedBlock
      || generatedCompressedLargest !== largestGeneratedCompressedBlock
      || lookupOrderTotal !== lookupOrderRecords
    ) throw new AnalyzerAnnotationsError('corrupt-index', 'Generated block totals disagree with header');

    const generatedRootRelative = generatedRootsOffset - blocksOffset;
    const rootsSeen = new Uint32Array(generatedBlocks);
    previousSeq = 0;
    let previousBlock = 0;
    for (let root = 0; root < generatedRoots; root++) {
      const at = generatedRootRelative + root * ANALYZER_GENERATED_ROOT_BYTES;
      const seq = indexView.getUint32(at, LITTLE_ENDIAN);
      const block = indexView.getUint16(at + 4, LITTLE_ENDIAN);
      if (
        block >= generatedBlocks
        || indexView.getUint16(at + 6, LITTLE_ENDIAN) !== 0
        || (root > 0 && seq <= previousSeq)
        || (root > 0 && block < previousBlock)
      ) throw new AnalyzerAnnotationsError('corrupt-index', 'Generated root index is non-canonical');
      if (rootsSeen[block] === 0) {
        const blockAt = generatedBlockRelative + block * ANALYZER_GENERATED_BLOCK_BYTES;
        if (indexView.getUint32(blockAt, LITTLE_ENDIAN) !== seq) {
          throw new AnalyzerAnnotationsError('corrupt-index', 'Generated block first root disagrees with root index');
        }
      }
      rootsSeen[block]++;
      previousSeq = seq;
      previousBlock = block;
    }
    for (let block = 0; block < generatedBlocks; block++) {
      if (rootsSeen[block] !== rootsPerBlock[block]) {
        throw new AnalyzerAnnotationsError('corrupt-index', 'Generated root/block coverage disagrees');
      }
    }

    const generatedFactsEnd = generatedFactsOffset - blocksOffset + generatedFactPairs * 2;
    const exceptionEntryRelative = exceptionEntriesOffset - blocksOffset;
    if (index.subarray(generatedFactsEnd, exceptionEntryRelative).some(value => value !== 0)) {
      throw new AnalyzerAnnotationsError('corrupt-index', 'Non-zero generated-fact padding');
    }

    const exceptionLocatorRelative = exceptionLocatorsOffset - blocksOffset;
    const exceptionStringRelative = exceptionStringsOffset - blocksOffset;
    const lookupOrderExceptions = new Map<string, LookupOrderExceptionSpan>();
    let exceptionLocatorTotal = 0;
    let exceptionClassTotal = 0;
    let exceptionStringTotal = 0;
    let exceptionMaximumRank = 0;
    let previousExceptionRoute = -1;
    let previousExceptionSurface: Uint8Array<ArrayBufferLike> = new Uint8Array();
    for (let exception = 0; exception < lookupOrderExceptionSurfaces; exception++) {
      const at = exceptionEntryRelative + exception * ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES;
      const surfaceOffset = indexView.getUint32(at, LITTLE_ENDIAN);
      const firstLocator = indexView.getUint32(at + 4, LITTLE_ENDIAN);
      const surfaceBytes = indexView.getUint16(at + 8, LITTLE_ENDIAN);
      const locatorCount = indexView.getUint16(at + 10, LITTLE_ENDIAN);
      const routeCode = indexView.getUint8(at + 12);
      const maximumRank = indexView.getUint8(at + 13);
      if (
        surfaceBytes === 0
        || locatorCount === 0
        || routeCode > 1
        || maximumRank > 0x3f
        || indexView.getUint16(at + 14, LITTLE_ENDIAN) !== 0
        || surfaceOffset !== exceptionStringTotal
        || firstLocator !== exceptionLocatorTotal
        || surfaceOffset + surfaceBytes > exceptionStringBytes
        || firstLocator + locatorCount > lookupOrderExceptionLocators
      ) throw new AnalyzerAnnotationsError('corrupt-index', 'Invalid lookup-order exception span');
      const encodedSurface = index.subarray(
        exceptionStringRelative + surfaceOffset,
        exceptionStringRelative + surfaceOffset + surfaceBytes
      );
      if (
        routeCode < previousExceptionRoute
        || (routeCode === previousExceptionRoute
          && compareBytes(previousExceptionSurface, encodedSurface) >= 0)
      ) throw new AnalyzerAnnotationsError('corrupt-index', 'Lookup-order exceptions are not canonical');
      let surface: string;
      try {
        surface = UTF8.decode(encodedSurface);
      } catch {
        throw new AnalyzerAnnotationsError('corrupt-index', 'Invalid lookup-order exception UTF-8');
      }
      const routeValue: AnalyzerSupportRoute = routeCode === 0 ? 'kana' : 'kanji';
      const key = lookupOrderExceptionKey(routeValue, surface);
      if (lookupOrderExceptions.has(key)) {
        throw new AnalyzerAnnotationsError('corrupt-index', 'Duplicate lookup-order exception');
      }
      lookupOrderExceptions.set(key, { first: firstLocator, count: locatorCount });
      const ranks = new Set<number>();
      let priorRoot = 0;
      let priorKey = 0;
      for (let locator = 0; locator < locatorCount; locator++) {
        const locatorAt = exceptionLocatorRelative
          + (firstLocator + locator) * ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES;
        const rootSeq = indexView.getUint32(locatorAt, LITTLE_ENDIAN);
        const packed = indexView.getUint32(locatorAt + 4, LITTLE_ENDIAN);
        const locatorKey = packed & GENERATED_KEY_MASK;
        const rank = (packed >>> GENERATED_KEY_BITS) & 0x3f;
        const firstAlias = locatorKey >>> GENERATED_ALIAS_BITS;
        const secondCode = locatorKey & ((1 << GENERATED_ALIAS_BITS) - 1);
        if (
          rootSeq === 0
          || (packed >>> 28) !== 0
          || (locatorKey !== GENERATED_KEY_MASK
            && (firstAlias > GENERATED_ALIAS_MAX
              || (secondCode > 0 && secondCode - 1 > GENERATED_ALIAS_MAX)))
          || (locator > 0
            && (rootSeq < priorRoot || (rootSeq === priorRoot && locatorKey <= priorKey)))
        ) throw new AnalyzerAnnotationsError('corrupt-index', 'Invalid lookup-order exception locator');
        priorRoot = rootSeq;
        priorKey = locatorKey;
        ranks.add(rank);
      }
      const orderedRanks = [...ranks].sort((left, right) => left - right);
      if (
        orderedRanks.length < 2
        || orderedRanks.length !== maximumRank + 1
        || orderedRanks.some((rank, index) => rank !== index)
      ) throw new AnalyzerAnnotationsError('corrupt-index', 'Lookup-order exception ranks are not dense');
      exceptionClassTotal += orderedRanks.length;
      exceptionMaximumRank = Math.max(exceptionMaximumRank, maximumRank);
      exceptionLocatorTotal += locatorCount;
      exceptionStringTotal += surfaceBytes;
      previousExceptionRoute = routeCode;
      previousExceptionSurface = encodedSurface;
    }
    if (
      exceptionLocatorTotal !== lookupOrderExceptionLocators
      || exceptionClassTotal !== lookupOrderExceptionClasses
      || exceptionStringTotal !== exceptionStringBytes
      || exceptionMaximumRank !== lookupOrderExceptionMaxRank
    ) throw new AnalyzerAnnotationsError('corrupt-index', 'Lookup-order exception totals disagree');
    const exceptionPaddingStart = exceptionStringRelative + exceptionStringBytes;
    const exceptionPaddingEnd = annotationDataOffset - blocksOffset;
    if (index.subarray(exceptionPaddingStart, exceptionPaddingEnd).some(value => value !== 0)) {
      throw new AnalyzerAnnotationsError('corrupt-index', 'Non-zero lookup-order exception padding');
    }

    const annotationPaddingStart = annotationDataOffset + annotationCompressedBytes;
    if (annotationPaddingStart < generatedDataOffset) {
      const padding = await readExact(
        source,
        annotationPaddingStart,
        generatedDataOffset - annotationPaddingStart,
        'corrupt-index'
      );
      if (padding.some(value => value !== 0)) {
        throw new AnalyzerAnnotationsError('corrupt-index', 'Non-zero annotation-data padding');
      }
    }

    const trailing = source.byteLength - (generatedDataOffset + generatedCompressedBytes);
    if (trailing > 0) {
      const padding = await readExact(
        source,
        generatedDataOffset + generatedCompressedBytes,
        trailing,
        'corrupt-index'
      );
      if (padding.some(value => value !== 0)) {
        throw new AnalyzerAnnotationsError('corrupt-index', 'Non-zero annotation trailing padding');
      }
    }
    const manifest: AnalyzerAnnotationsManifest = {
      byteLength: source.byteLength,
      blocks,
      splits,
      hints,
      residentIndexBytes: annotationDataOffset,
      compressedBytes: annotationCompressedBytes + generatedCompressedBytes,
      uncompressedBytes: annotationUncompressedBytes + generatedUncompressedBytes,
      largestUncompressedBlock: largestAnnotationBlock,
      generatedBlocks,
      generatedRoots,
      generatedRecords,
      lookupOrderRecords,
      lookupOrderRoots,
      lookupOrderMaxRank,
      lookupOrderExceptionSurfaces,
      lookupOrderExceptionClasses,
      lookupOrderExceptionLocators,
      lookupOrderExceptionBytes:
        lookupOrderExceptionSurfaces * ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES
        + lookupOrderExceptionLocators * ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES
        + exceptionStringBytes,
      generatedPhysicalGroups,
      generatedFactPairs,
      generatedCompressedBytes,
      generatedUncompressedBytes,
      largestGeneratedBlock,
      largestGeneratedCompressedBlock
    };
    return new AnalyzerAnnotationsReader(
      source,
      decode,
      manifest,
      index,
      generatedBlocksOffset,
      generatedRootsOffset,
      generatedFactsOffset,
      exceptionLocatorsOffset,
      lookupOrderExceptions,
      annotationDataOffset,
      generatedDataOffset,
      generatedCompressedBytes
    );
  }

  async split(
    definitionSeq: number,
    routeValue: AnalyzerSupportRoute,
    surface: string,
    kind: AnalyzerSupportSplitKind = 'split'
  ): Promise<AnalyzerSupportSplit | null> {
    const block = await this.#block(definitionSeq);
    return block?.splits.find(value =>
      value.route === routeValue && value.surface === surface && value.kind === kind) ?? null;
  }

  async hint(
    definitionSeq: number,
    routeValue: AnalyzerSupportRoute,
    surface: string,
    reading: string
  ): Promise<string | null> {
    const block = await this.#block(definitionSeq);
    return block?.hints.find(value =>
      value.route === routeValue && value.surface === surface && value.reading === reading)?.hint ?? null;
  }

  async generated(
    rootSeq: number,
    aliases: readonly [number] | readonly [number, number]
  ): Promise<AnalyzerGeneratedFacts | null> {
    const blockIndex = this.#generatedBlockForRoot(rootSeq);
    if (blockIndex === null) return null;
    return generatedFact(await this.#generatedBlock(blockIndex), rootSeq, aliases);
  }

  async lookupOrder(
    routeValue: AnalyzerSupportRoute,
    surface: string,
    rootSeq: number,
    aliases: readonly [number] | readonly [number, number] | null
  ): Promise<number | null> {
    const key = aliases === null ? GENERATED_KEY_MASK : generatedKey(aliases);
    const exception = exceptionLookupOrder(
      this.#lookupOrderExceptions,
      this.#index,
      this.#exceptionLocatorsOffset,
      routeValue,
      surface,
      rootSeq,
      key
    );
    if (exception !== undefined) return exception;
    const blockIndex = this.#generatedBlockForRoot(rootSeq);
    if (blockIndex === null) return null;
    return generatedOrder(await this.#generatedBlock(blockIndex), rootSeq, key);
  }

  definitionSeqs(): Uint32Array {
    return Uint32Array.from({ length: this.manifest.blocks }, (_, index) =>
      this.#index.getUint32(index * ANALYZER_ANNOTATIONS_BLOCK_BYTES, LITTLE_ENDIAN));
  }

  generatedRootSeqs(): Uint32Array {
    return Uint32Array.from({ length: this.manifest.generatedRoots }, (_, index) =>
      this.#index.getUint32(
        this.#generatedRootsOffset + index * ANALYZER_GENERATED_ROOT_BYTES,
        LITTLE_ENDIAN
      ));
  }

  createPreloaded(): PreloadedAnalyzerAnnotations {
    return new PreloadedAnalyzerAnnotations(
      this.#knownDefinitions,
      seq => this.#block(seq),
      seq => this.#generatedBlockForRoot(seq),
      indexes => this.#generatedBlocksFor(indexes),
      (routeValue, surface, rootSeq, key) => exceptionLookupOrder(
        this.#lookupOrderExceptions,
        this.#index,
        this.#exceptionLocatorsOffset,
        routeValue,
        surface,
        rootSeq,
        key
      ),
      this.#annotationCache,
      this.#generatedCache
    );
  }

  /** Inflate and verify every generated-fact block once at Worker startup. */
  async preloadAllGenerated(): Promise<void> {
    for (let blockIndex = 0; blockIndex < this.manifest.generatedBlocks; blockIndex++) {
      await this.#generatedBlock(blockIndex);
    }
  }

  /**
   * Inflate only definitions reachable from the current candidate set during
   * the async Worker preparation phase; scoring then uses synchronous lookups.
   */
  async preload(definitionSeqs: Iterable<number>): Promise<PreloadedAnalyzerAnnotations> {
    const view = this.createPreloaded();
    await view.preload(definitionSeqs);
    return view;
  }

  async #block(seq: number): Promise<DecodedBlock | null> {
    const cached = this.#annotationCache.get(seq);
    if (cached) {
      this.#annotationCache.delete(seq);
      this.#annotationCache.set(seq, cached);
      return cached;
    }
    let low = 0;
    let high = this.manifest.blocks;
    while (low < high) {
      const middle = (low + high) >>> 1;
      const current = this.#index.getUint32(middle * ANALYZER_ANNOTATIONS_BLOCK_BYTES, LITTLE_ENDIAN);
      if (current < seq) low = middle + 1;
      else high = middle;
    }
    if (
      low >= this.manifest.blocks
      || this.#index.getUint32(low * ANALYZER_ANNOTATIONS_BLOCK_BYTES, LITTLE_ENDIAN) !== seq
    ) return null;
    const at = low * ANALYZER_ANNOTATIONS_BLOCK_BYTES;
    const offset = this.#index.getUint32(at + 4, LITTLE_ENDIAN);
    const compressedLength = this.#index.getUint32(at + 8, LITTLE_ENDIAN);
    const uncompressedLength = this.#index.getUint32(at + 12, LITTLE_ENDIAN);
    const expectedChecksum = this.#index.getUint32(at + 16, LITTLE_ENDIAN);
    const expectedSplits = this.#index.getUint16(at + 20, LITTLE_ENDIAN);
    const expectedHints = this.#index.getUint16(at + 22, LITTLE_ENDIAN);
    const compressed = await readExact(
      this.#source,
      this.#annotationDataOffset + offset,
      compressedLength,
      'corrupt-block'
    );
    let bytes: Uint8Array;
    try {
      bytes = await this.#decode(compressed, uncompressedLength);
    } catch {
      throw new AnalyzerAnnotationsError('corrupt-block', `Annotation block ${seq} could not be decoded`);
    }
    if (bytes.byteLength !== uncompressedLength || crc32(bytes) !== expectedChecksum) {
      throw new AnalyzerAnnotationsError('corrupt-block', `Annotation block ${seq} failed verification`);
    }
    let parsed: unknown;
    try {
      parsed = JSON.parse(UTF8.decode(bytes));
    } catch {
      throw new AnalyzerAnnotationsError('corrupt-block', `Annotation block ${seq} is not valid JSON`);
    }
    if (
      !Array.isArray(parsed) || parsed.length !== 4
      || parsed[0] !== ANALYZER_ANNOTATIONS_FORMAT_VERSION || parsed[1] !== seq
      || !Array.isArray(parsed[2]) || !Array.isArray(parsed[3])
      || parsed[2].length !== expectedSplits || parsed[3].length !== expectedHints
    ) throw new AnalyzerAnnotationsError('corrupt-block', `Annotation block ${seq} has invalid structure`);
    const decoded: DecodedBlock = {
      seq,
      splits: parsed[2].map(value => split(seq, value)),
      hints: parsed[3].map(value => hint(seq, value))
    };
    this.#annotationCache.set(seq, decoded);
    while (this.#annotationCache.size > ANALYZER_ANNOTATION_CACHE_BLOCKS) {
      const oldest = this.#annotationCache.keys().next().value as number | undefined;
      if (oldest === undefined) break;
      this.#annotationCache.delete(oldest);
    }
    return decoded;
  }

  #generatedBlockForRoot(seq: number): number | null {
    let low = 0;
    let high = this.manifest.generatedRoots;
    while (low < high) {
      const middle = (low + high) >>> 1;
      const at = this.#generatedRootsOffset + middle * ANALYZER_GENERATED_ROOT_BYTES;
      const current = this.#index.getUint32(at, LITTLE_ENDIAN);
      if (current < seq) low = middle + 1;
      else high = middle;
    }
    if (low >= this.manifest.generatedRoots) return null;
    const at = this.#generatedRootsOffset + low * ANALYZER_GENERATED_ROOT_BYTES;
    return this.#index.getUint32(at, LITTLE_ENDIAN) === seq
      ? this.#index.getUint16(at + 4, LITTLE_ENDIAN)
      : null;
  }

  async #generatedBlocksFor(
    blockIndexes: readonly number[]
  ): Promise<ReadonlyMap<number, DecodedGeneratedBlock>> {
    const output = new Map<number, DecodedGeneratedBlock>();
    for (const blockIndex of [...new Set(blockIndexes)].sort((left, right) => left - right)) {
      output.set(blockIndex, await this.#generatedBlock(blockIndex));
    }
    return output;
  }

  async #generatedBlock(blockIndex: number): Promise<DecodedGeneratedBlock> {
    const cached = this.#generatedCache.get(blockIndex);
    if (cached) {
      this.#generatedCache.delete(blockIndex);
      this.#generatedCache.set(blockIndex, cached);
      return cached;
    }
    if (!Number.isInteger(blockIndex)
      || blockIndex < 0
      || blockIndex >= this.manifest.generatedBlocks) {
      throw new AnalyzerAnnotationsError('out-of-range', `Generated block ${blockIndex} is out of range`);
    }
    const at = this.#generatedBlocksOffset
      + blockIndex * ANALYZER_GENERATED_BLOCK_BYTES;
    const offset = this.#index.getUint32(at + 4, LITTLE_ENDIAN);
    const compressedLength = this.#index.getUint32(at + 8, LITTLE_ENDIAN);
    const uncompressedLength = this.#index.getUint32(at + 12, LITTLE_ENDIAN);
    const expectedChecksum = this.#index.getUint32(at + 16, LITTLE_ENDIAN);
    const expectedRoots = this.#index.getUint16(at + 20, LITTLE_ENDIAN);
    const expectedOrders = this.#index.getUint16(at + 22, LITTLE_ENDIAN);
    if (offset + compressedLength > this.#generatedCompressedBytes) {
      throw new AnalyzerAnnotationsError('corrupt-index', 'Generated block lies outside data');
    }
    const compressed = await readExact(
      this.#source,
      this.#generatedDataOffset + offset,
      compressedLength,
      'corrupt-block'
    );
    let bytes: Uint8Array;
    try {
      bytes = await this.#decode(compressed, uncompressedLength);
    } catch {
      throw new AnalyzerAnnotationsError(
        'corrupt-block',
        `Generated block ${blockIndex} could not be decoded`
      );
    }
    if (bytes.byteLength !== uncompressedLength || crc32(bytes) !== expectedChecksum) {
      throw new AnalyzerAnnotationsError(
        'corrupt-block',
        `Generated block ${blockIndex} failed verification`
      );
    }
    const decoded = this.#decodeGeneratedBlock(blockIndex, bytes, expectedRoots, expectedOrders);
    this.#generatedCache.set(blockIndex, decoded);
    while (this.#generatedCache.size > ANALYZER_GENERATED_CACHE_BLOCKS) {
      const oldest = this.#generatedCache.keys().next().value as number | undefined;
      if (oldest === undefined) break;
      this.#generatedCache.delete(oldest);
    }
    return decoded;
  }

  #decodeGeneratedBlock(
    blockIndex: number,
    bytes: Uint8Array,
    expectedRoots: number,
    expectedOrders: number
  ): DecodedGeneratedBlock {
    if (bytes.byteLength < 12) {
      throw new AnalyzerAnnotationsError('corrupt-block', 'Generated block is truncated');
    }
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    const roots = view.getUint32(0, LITTLE_ENDIAN);
    const records = view.getUint32(4, LITTLE_ENDIAN);
    const orders = view.getUint32(8, LITTLE_ENDIAN);
    const recordsOffset = 12 + roots * 20;
    const ordersOffset = recordsOffset + records * ANALYZER_GENERATED_RECORD_BYTES;
    if (
      roots !== expectedRoots
      || orders !== expectedOrders
      || ordersOffset + orders * ANALYZER_LOOKUP_ORDER_RECORD_BYTES !== bytes.byteLength
    ) throw new AnalyzerAnnotationsError('corrupt-block', 'Generated block has invalid dimensions');
    let previousSeq = 0;
    let nextRecord = 0;
    let nextOrder = 0;
    const temporary: DecodedGeneratedBlock = {
      blockIndex,
      bytes,
      view,
      roots,
      records,
      recordsOffset,
      orders,
      ordersOffset,
      facts: this.#generatedFacts,
      physicalGroups: this.manifest.generatedPhysicalGroups
    };
    for (let root = 0; root < roots; root++) {
      const at = 12 + root * 20;
      const seq = view.getUint32(at, LITTLE_ENDIAN);
      const first = view.getUint32(at + 4, LITTLE_ENDIAN);
      const count = view.getUint32(at + 8, LITTLE_ENDIAN);
      const firstOrder = view.getUint32(at + 12, LITTLE_ENDIAN);
      const orderCount = view.getUint32(at + 16, LITTLE_ENDIAN);
      if ((root > 0 && seq <= previousSeq)
        || first !== nextRecord
        || firstOrder !== nextOrder
        || count + orderCount === 0) {
        throw new AnalyzerAnnotationsError('corrupt-block', 'Generated block roots are non-canonical');
      }
      let previousLowKey = -1;
      let previousMemberOrd = -1;
      let previousPropOrd = -1;
      let previousViaMemberOrd = -1;
      let previousProperty = -1;
      let semanticFact = -1;
      let semanticGroup = -1;
      let semanticCountOnly = false;
      for (let record = 0; record < count; record++) {
        const recordAt = recordsOffset
          + (first + record) * ANALYZER_GENERATED_RECORD_BYTES;
        const storedKey = view.getUint32(recordAt, LITTLE_ENDIAN);
        const key = storedKey & GENERATED_KEY_MASK;
        const propOrd = storedKey >>> GENERATED_KEY_BITS;
        const fact = view.getUint8(recordAt + 4);
        const physical = u24(view, recordAt + 5);
        const group = physical & GENERATED_GROUP_MASK;
        const memberOrd = (physical >>> 18) & 7;
        const viaMemberOrd = (physical >>> 21) & 7;
        const property = view.getUint16(recordAt + 8, LITTLE_ENDIAN);
        const countOnly = property === GENERATED_PROPERTY_NONE;
        const negative = (property >>> 11) & 3;
        const formal = (property >>> 13) & 3;
        const firstAlias = key >>> GENERATED_ALIAS_BITS;
        const secondCode = key & ((1 << GENERATED_ALIAS_BITS) - 1);
        const sameSemantic = key === previousLowKey;
        const viaOrder = viaMemberOrd === GENERATED_VIA_MEMBER_NONE ? -1 : viaMemberOrd;
        const previousViaOrder = previousViaMemberOrd === GENERATED_VIA_MEMBER_NONE
          ? -1
          : previousViaMemberOrd;
        const canonicalMemberOrder = !sameSemantic
          || memberOrd > previousMemberOrd
          || (memberOrd === previousMemberOrd && propOrd > previousPropOrd)
          || (memberOrd === previousMemberOrd && propOrd === previousPropOrd
            && viaOrder > previousViaOrder)
          || (memberOrd === previousMemberOrd && propOrd === previousPropOrd
            && viaOrder === previousViaOrder && property > previousProperty);
        if (
          key < previousLowKey
          || !canonicalMemberOrder
          || firstAlias > GENERATED_ALIAS_MAX
          || secondCode > GENERATED_ALIAS_MAX + 1
          || fact > this.manifest.generatedFactPairs
          || group > this.manifest.generatedPhysicalGroups
          || (sameSemantic
            && (fact !== semanticFact || group !== semanticGroup || semanticCountOnly || countOnly))
          || (countOnly && (fact === 0 || physical !== 0 || propOrd !== 0))
          || (!countOnly
            && (memberOrd > GENERATED_MEMBER_ORD_MAX
              || negative > 2 || formal > 2 || (property & 0x8000) !== 0))
        ) throw new AnalyzerAnnotationsError('corrupt-block', 'Generated record is non-canonical');
        if (!sameSemantic) {
          semanticFact = fact;
          semanticGroup = group;
          semanticCountOnly = countOnly;
        }
        previousLowKey = key;
        previousMemberOrd = memberOrd;
        previousPropOrd = propOrd;
        previousViaMemberOrd = viaMemberOrd;
        previousProperty = property;
      }
      let previousOrderKey = -1;
      for (let order = 0; order < orderCount; order++) {
        const orderAt = ordersOffset
          + (firstOrder + order) * ANALYZER_LOOKUP_ORDER_RECORD_BYTES;
        const packed = view.getUint32(orderAt, LITTLE_ENDIAN);
        const locator = packed & GENERATED_KEY_MASK;
        const firstAlias = locator >>> GENERATED_ALIAS_BITS;
        if ((packed >>> 28) !== 0
          || locator <= previousOrderKey
          || (locator !== GENERATED_KEY_MASK && firstAlias > GENERATED_ALIAS_MAX)) {
          throw new AnalyzerAnnotationsError('corrupt-block', 'Lookup-order record is invalid');
        }
        previousOrderKey = locator;
      }
      previousSeq = seq;
      nextRecord += count;
      nextOrder += orderCount;
    }
    if (nextRecord !== records || nextOrder !== orders) {
      throw new AnalyzerAnnotationsError('corrupt-block', 'Generated records are not covered');
    }
    return temporary;
  }
}
