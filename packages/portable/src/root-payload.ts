import { crc32 } from './crc32.js';

/** Pack section reserved for the hot, direct-root payload. */
export const ROOT_PAYLOAD_SECTION_ID = 2;

export const ROOT_PAYLOAD_FORMAT_VERSION = 2;
export const ROOT_PAYLOAD_HEADER_BYTES = 128;

export const ROOT_PAYLOAD_SPAN_BYTES = 4;
export const ROOT_PAYLOAD_FORM_BYTES = 11;
export const ROOT_PAYLOAD_ENTRY_BYTES = 9;
export const ROOT_PAYLOAD_RESTRICTION_BYTES = 12;
export const ROOT_PAYLOAD_POS_SET_BYTES = 6;

/** A surface reference with this bit set addresses the payload string pool. */
export const ROOT_SURFACE_REF_STRING_BIT = 0x8000_0000;
/** Sentinel used only by nullable best-counterpart references. */
export const ROOT_SURFACE_REF_NONE = 0xffff_ffff;

const ROOT_PAYLOAD_MAGIC = 'IROOT002';
const LITTLE_ENDIAN = true;
const SECTION_ALIGNMENT = 8;

const HEADER_FLAGS_OFFSET = 12;
const HEADER_TOTAL_BYTES_OFFSET = 16;
const HEADER_CHECKSUM_OFFSET = 20;
const PAYLOAD_CHECKSUM_OFFSET = 24;
const HEADER_RESERVED_OFFSET = 28;

const SURFACE_COUNT_OFFSET = 32;
const FORM_COUNT_OFFSET = 36;
const ENTRY_COUNT_OFFSET = 40;
const RESTRICTION_COUNT_OFFSET = 44;
const STRING_COUNT_OFFSET = 48;
const POS_SET_COUNT_OFFSET = 52;
const POS_MEMBER_COUNT_OFFSET = 56;

const SPAN_STRIDE_OFFSET = 60;
const FORM_STRIDE_OFFSET = 61;
const ENTRY_STRIDE_OFFSET = 62;
const RESTRICTION_STRIDE_OFFSET = 63;

const SPANS_OFFSET = 64;
const FORMS_OFFSET = 68;
const ENTRIES_OFFSET = 72;
const RESTRICTIONS_OFFSET = 76;
const POS_SETS_OFFSET = 80;
const POS_MEMBERS_OFFSET = 84;
const STRING_OFFSETS_OFFSET = 88;
const STRING_DATA_OFFSET = 92;
const STRING_DATA_BYTES_OFFSET = 96;
const HEADER_RESERVED_TAIL_OFFSET = 100;

const FORM_COMMON_NULL = 63;
const FORM_ROUTE_BIT = 0x40;
const FORM_CONJUGATABLE_BIT = 0x80;
const FORM_NOKANJI_BIT = 0x80;

const ENTRY_PRIMARY_NOKANJI_BIT = 1 << 0;
const ENTRY_ARCHIVED_BIT = 1 << 1;
const ENTRY_PREFER_KANA_BIT = 1 << 2;
const ENTRY_PREFER_KANA_ORDINAL_ZERO_BIT = 1 << 3;
const ENTRY_KNOWN_FLAGS =
  ENTRY_PRIMARY_NOKANJI_BIT
  | ENTRY_ARCHIVED_BIT
  | ENTRY_PREFER_KANA_BIT
  | ENTRY_PREFER_KANA_ORDINAL_ZERO_BIT;

export type RootRoute = 'kanji' | 'kana';

export type RootPayloadFormatErrorCode =
  | 'invalid-header'
  | 'unsupported-version'
  | 'corrupt-payload'
  | 'out-of-range';

export class RootPayloadFormatError extends Error {
  readonly code: RootPayloadFormatErrorCode;

  constructor(code: RootPayloadFormatErrorCode, message: string) {
    super(message);
    this.name = 'RootPayloadFormatError';
    this.code = code;
  }
}

export interface RootPayloadLayout {
  readonly byteLength: number;
  readonly spansOffset: number;
  readonly formsOffset: number;
  readonly entriesOffset: number;
  readonly restrictionsOffset: number;
  readonly posSetsOffset: number;
  readonly posMembersOffset: number;
  readonly stringOffsetsOffset: number;
  readonly stringDataOffset: number;
  readonly stringDataBytes: number;
}

interface Utf8Decoder {
  decode(input: Uint8Array): string;
}

interface Utf8DecoderConstructor {
  new(label: string, options: { fatal: boolean; ignoreBOM: boolean }): Utf8Decoder;
}

const UTF8_DECODER = new (
  globalThis as unknown as { TextDecoder: Utf8DecoderConstructor }
).TextDecoder('utf-8', { fatal: true, ignoreBOM: true });

function asBytes(input: ArrayBuffer | Uint8Array): Uint8Array {
  return input instanceof Uint8Array ? input : new Uint8Array(input);
}

function align(value: number): number {
  return Math.ceil(value / SECTION_ALIGNMENT) * SECTION_ALIGNMENT;
}

function checkedProduct(count: number, stride: number, label: string): number {
  const result = count * stride;
  if (!Number.isSafeInteger(result) || result > 0xffff_ffff) {
    throw new RootPayloadFormatError('invalid-header', `${label} byte length overflows uint32`);
  }
  return result;
}

function hasMagic(bytes: Uint8Array): boolean {
  for (let index = 0; index < ROOT_PAYLOAD_MAGIC.length; index++) {
    if (bytes[index] !== ROOT_PAYLOAD_MAGIC.charCodeAt(index)) return false;
  }
  return true;
}

function assertZero(bytes: Uint8Array, start: number, end: number, label: string): void {
  for (let index = start; index < end; index++) {
    if (bytes[index] !== 0) {
      throw new RootPayloadFormatError('invalid-header', `${label} byte ${index} is non-zero`);
    }
  }
}

function assertIndex(index: number, count: number, label: string): void {
  if (!Number.isSafeInteger(index) || index < 0 || index >= count) {
    throw new RootPayloadFormatError(
      'out-of-range',
      `${label} ${index} is outside [0, ${count})`
    );
  }
}

function uint24(view: DataView, offset: number): number {
  return view.getUint16(offset, LITTLE_ENDIAN) | (view.getUint8(offset + 2) << 16);
}

function isStringReference(reference: number): boolean {
  return reference !== ROOT_SURFACE_REF_NONE
    && (reference & ROOT_SURFACE_REF_STRING_BIT) !== 0;
}

/**
 * Zero-copy reader for the root payload.
 *
 * Records stay packed. Scalar accessors read directly through one DataView; the
 * reader never expands rows into JavaScript objects or arrays. Strings are the
 * only decoded values and are cached by their small pool ID.
 */
export class RootPayloadReader {
  readonly surfaceCount: number;
  readonly formCount: number;
  readonly entryCount: number;
  readonly restrictionCount: number;
  readonly stringCount: number;
  readonly posSetCount: number;
  readonly posMemberCount: number;
  readonly layout: RootPayloadLayout;

  readonly #bytes: Uint8Array;
  readonly #view: DataView;
  readonly #strings: Array<string | undefined>;

  constructor(input: ArrayBuffer | Uint8Array) {
    const bytes = asBytes(input);
    if (bytes.byteLength < ROOT_PAYLOAD_HEADER_BYTES) {
      throw new RootPayloadFormatError('invalid-header', 'Root payload is shorter than its header');
    }
    if (!hasMagic(bytes)) {
      throw new RootPayloadFormatError('invalid-header', `Expected ${ROOT_PAYLOAD_MAGIC} magic bytes`);
    }

    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    const version = view.getUint16(8, LITTLE_ENDIAN);
    if (version !== ROOT_PAYLOAD_FORMAT_VERSION) {
      throw new RootPayloadFormatError(
        'unsupported-version',
        `Unsupported root payload version ${version}; expected ${ROOT_PAYLOAD_FORMAT_VERSION}`
      );
    }
    if (view.getUint16(10, LITTLE_ENDIAN) !== ROOT_PAYLOAD_HEADER_BYTES) {
      throw new RootPayloadFormatError('invalid-header', 'Root payload header size is invalid');
    }
    if (
      view.getUint32(HEADER_FLAGS_OFFSET, LITTLE_ENDIAN) !== 0
      || view.getUint32(HEADER_RESERVED_OFFSET, LITTLE_ENDIAN) !== 0
    ) {
      throw new RootPayloadFormatError('invalid-header', 'Root payload reserved header fields are non-zero');
    }
    assertZero(bytes, HEADER_RESERVED_TAIL_OFFSET, ROOT_PAYLOAD_HEADER_BYTES, 'Reserved header');

    const totalBytes = view.getUint32(HEADER_TOTAL_BYTES_OFFSET, LITTLE_ENDIAN);
    if (totalBytes !== bytes.byteLength) {
      throw new RootPayloadFormatError(
        'invalid-header',
        `Root payload declares ${totalBytes} bytes but received ${bytes.byteLength}`
      );
    }

    const headerCopy = bytes.slice(0, ROOT_PAYLOAD_HEADER_BYTES);
    new DataView(headerCopy.buffer).setUint32(HEADER_CHECKSUM_OFFSET, 0, LITTLE_ENDIAN);
    const expectedHeaderChecksum = view.getUint32(HEADER_CHECKSUM_OFFSET, LITTLE_ENDIAN);
    if (crc32(headerCopy) !== expectedHeaderChecksum) {
      throw new RootPayloadFormatError('invalid-header', 'Root payload header checksum does not match');
    }

    const expectedPayloadChecksum = view.getUint32(PAYLOAD_CHECKSUM_OFFSET, LITTLE_ENDIAN);
    if (crc32(bytes.subarray(ROOT_PAYLOAD_HEADER_BYTES)) !== expectedPayloadChecksum) {
      throw new RootPayloadFormatError('corrupt-payload', 'Root payload checksum does not match');
    }

    this.surfaceCount = view.getUint32(SURFACE_COUNT_OFFSET, LITTLE_ENDIAN);
    this.formCount = view.getUint32(FORM_COUNT_OFFSET, LITTLE_ENDIAN);
    this.entryCount = view.getUint32(ENTRY_COUNT_OFFSET, LITTLE_ENDIAN);
    this.restrictionCount = view.getUint32(RESTRICTION_COUNT_OFFSET, LITTLE_ENDIAN);
    this.stringCount = view.getUint32(STRING_COUNT_OFFSET, LITTLE_ENDIAN);
    this.posSetCount = view.getUint32(POS_SET_COUNT_OFFSET, LITTLE_ENDIAN);
    this.posMemberCount = view.getUint32(POS_MEMBER_COUNT_OFFSET, LITTLE_ENDIAN);

    if (
      view.getUint8(SPAN_STRIDE_OFFSET) !== ROOT_PAYLOAD_SPAN_BYTES
      || view.getUint8(FORM_STRIDE_OFFSET) !== ROOT_PAYLOAD_FORM_BYTES
      || view.getUint8(ENTRY_STRIDE_OFFSET) !== ROOT_PAYLOAD_ENTRY_BYTES
      || view.getUint8(RESTRICTION_STRIDE_OFFSET) !== ROOT_PAYLOAD_RESTRICTION_BYTES
    ) {
      throw new RootPayloadFormatError('invalid-header', 'Root payload record strides are invalid');
    }

    const spansOffset = view.getUint32(SPANS_OFFSET, LITTLE_ENDIAN);
    const formsOffset = view.getUint32(FORMS_OFFSET, LITTLE_ENDIAN);
    const entriesOffset = view.getUint32(ENTRIES_OFFSET, LITTLE_ENDIAN);
    const restrictionsOffset = view.getUint32(RESTRICTIONS_OFFSET, LITTLE_ENDIAN);
    const posSetsOffset = view.getUint32(POS_SETS_OFFSET, LITTLE_ENDIAN);
    const posMembersOffset = view.getUint32(POS_MEMBERS_OFFSET, LITTLE_ENDIAN);
    const stringOffsetsOffset = view.getUint32(STRING_OFFSETS_OFFSET, LITTLE_ENDIAN);
    const stringDataOffset = view.getUint32(STRING_DATA_OFFSET, LITTLE_ENDIAN);
    const stringDataBytes = view.getUint32(STRING_DATA_BYTES_OFFSET, LITTLE_ENDIAN);

    let expectedOffset = align(ROOT_PAYLOAD_HEADER_BYTES);
    const sections: ReadonlyArray<readonly [number, number, string]> = [
      [spansOffset, checkedProduct(this.surfaceCount, ROOT_PAYLOAD_SPAN_BYTES, 'Span'), 'spans'],
      [formsOffset, checkedProduct(this.formCount, ROOT_PAYLOAD_FORM_BYTES, 'Form'), 'forms'],
      [entriesOffset, checkedProduct(this.entryCount, ROOT_PAYLOAD_ENTRY_BYTES, 'Entry'), 'entries'],
      [restrictionsOffset, checkedProduct(this.restrictionCount, ROOT_PAYLOAD_RESTRICTION_BYTES, 'Restriction'), 'restrictions'],
      [posSetsOffset, checkedProduct(this.posSetCount, ROOT_PAYLOAD_POS_SET_BYTES, 'POS set'), 'POS sets'],
      [posMembersOffset, checkedProduct(this.posMemberCount, 2, 'POS member'), 'POS members'],
      [stringOffsetsOffset, checkedProduct(this.stringCount + 1, 4, 'String offset'), 'string offsets'],
      [stringDataOffset, stringDataBytes, 'string data']
    ];

    for (const [offset, byteLength, label] of sections) {
      if (offset !== expectedOffset) {
        throw new RootPayloadFormatError(
          'invalid-header',
          `Root payload ${label} starts at ${offset}; expected ${expectedOffset}`
        );
      }
      const end = offset + byteLength;
      if (!Number.isSafeInteger(end) || end > bytes.byteLength) {
        throw new RootPayloadFormatError('invalid-header', `Root payload ${label} exceeds the file`);
      }
      const next = align(end);
      assertZero(bytes, end, next, `${label} padding`);
      expectedOffset = next;
    }
    if (expectedOffset !== bytes.byteLength) {
      throw new RootPayloadFormatError('invalid-header', 'Root payload has trailing bytes');
    }

    if (this.stringCount === 0 || this.posSetCount === 0) {
      throw new RootPayloadFormatError('invalid-header', 'Root payload must contain empty string and POS-set records');
    }
    if (view.getUint32(stringOffsetsOffset, LITTLE_ENDIAN) !== 0) {
      throw new RootPayloadFormatError('invalid-header', 'First string offset must be zero');
    }
    let previousStringOffset = 0;
    for (let index = 1; index <= this.stringCount; index++) {
      const offset = view.getUint32(stringOffsetsOffset + index * 4, LITTLE_ENDIAN);
      if (offset < previousStringOffset || offset > stringDataBytes) {
        throw new RootPayloadFormatError('invalid-header', 'String offsets are not monotonic');
      }
      previousStringOffset = offset;
    }
    if (previousStringOffset !== stringDataBytes) {
      throw new RootPayloadFormatError('invalid-header', 'Final string offset does not equal string data size');
    }

    // Span records form one canonical, gapless partition of the form table.
    let nextForm = 0;
    for (let rank = 0; rank < this.surfaceCount; rank++) {
      const offset = spansOffset + rank * ROOT_PAYLOAD_SPAN_BYTES;
      const first = uint24(view, offset);
      const count = view.getUint8(offset + 3);
      if (first !== nextForm || count === 0) {
        throw new RootPayloadFormatError('invalid-header', `Surface span ${rank} is not canonical`);
      }
      nextForm += count;
    }
    if (nextForm !== this.formCount) {
      throw new RootPayloadFormatError('invalid-header', 'Surface spans do not cover the form table');
    }

    // POS-set records form one canonical, gapless partition of the member table.
    let nextMember = 0;
    for (let set = 0; set < this.posSetCount; set++) {
      const offset = posSetsOffset + set * ROOT_PAYLOAD_POS_SET_BYTES;
      const first = view.getUint32(offset, LITTLE_ENDIAN);
      const count = view.getUint16(offset + 4, LITTLE_ENDIAN);
      if (first !== nextMember) {
        throw new RootPayloadFormatError('invalid-header', `POS set ${set} is not canonical`);
      }
      nextMember += count;
    }
    if (nextMember !== this.posMemberCount) {
      throw new RootPayloadFormatError('invalid-header', 'POS sets do not cover the member table');
    }

    this.#bytes = bytes;
    this.#view = view;
    this.#strings = new Array(this.stringCount);
    this.layout = Object.freeze({
      byteLength: bytes.byteLength,
      spansOffset,
      formsOffset,
      entriesOffset,
      restrictionsOffset,
      posSetsOffset,
      posMembersOffset,
      stringOffsetsOffset,
      stringDataOffset,
      stringDataBytes
    });
  }

  surfaceFormStart(rank: number): number {
    assertIndex(rank, this.surfaceCount, 'Surface rank');
    return uint24(this.#view, this.layout.spansOffset + rank * ROOT_PAYLOAD_SPAN_BYTES);
  }

  surfaceFormCount(rank: number): number {
    assertIndex(rank, this.surfaceCount, 'Surface rank');
    return this.#view.getUint8(this.layout.spansOffset + rank * ROOT_PAYLOAD_SPAN_BYTES + 3);
  }

  formEntryIndex(form: number): number {
    const offset = this.#formOffset(form);
    return uint24(this.#view, offset);
  }

  formBestReference(form: number): number {
    const offset = this.#formOffset(form);
    return this.#view.getUint32(offset + 3, LITTLE_ENDIAN);
  }

  formCommonTagStringId(form: number): number {
    const offset = this.#formOffset(form);
    return this.#view.getUint16(offset + 7, LITTLE_ENDIAN);
  }

  formCommon(form: number): number | null {
    const value = this.#view.getUint8(this.#formOffset(form) + 9) & 0x3f;
    return value === FORM_COMMON_NULL ? null : value;
  }

  formRoute(form: number): RootRoute {
    return (this.#view.getUint8(this.#formOffset(form) + 9) & FORM_ROUTE_BIT) !== 0
      ? 'kana'
      : 'kanji';
  }

  formConjugatable(form: number): boolean {
    return (this.#view.getUint8(this.#formOffset(form) + 9) & FORM_CONJUGATABLE_BIT) !== 0;
  }

  formOrdinal(form: number): number {
    return this.#view.getUint8(this.#formOffset(form) + 10) & 0x7f;
  }

  formNokanji(form: number): boolean {
    return (this.#view.getUint8(this.#formOffset(form) + 10) & FORM_NOKANJI_BIT) !== 0;
  }

  entrySeq(entry: number): number {
    return this.#view.getUint32(this.#entryOffset(entry), LITTLE_ENDIAN);
  }

  entryNKanji(entry: number): number {
    return this.#view.getUint8(this.#entryOffset(entry) + 6);
  }

  entryNKana(entry: number): number {
    return this.#view.getUint8(this.#entryOffset(entry) + 7);
  }

  entryPrimaryNokanji(entry: number): boolean {
    return this.#entryFlag(entry, ENTRY_PRIMARY_NOKANJI_BIT);
  }

  entryArchived(entry: number): boolean {
    return this.#entryFlag(entry, ENTRY_ARCHIVED_BIT);
  }

  entryPreferKana(entry: number): boolean {
    return this.#entryFlag(entry, ENTRY_PREFER_KANA_BIT);
  }

  /** True when at least one `uk` property belongs to sense ordinal zero. */
  entryPreferKanaOnOrdinalZero(entry: number): boolean {
    return this.#entryFlag(entry, ENTRY_PREFER_KANA_ORDINAL_ZERO_BIT);
  }

  entryPosSetIndex(entry: number): number {
    return this.#view.getUint16(this.#entryOffset(entry) + 4, LITTLE_ENDIAN);
  }

  entryPosCount(entry: number): number {
    const set = this.entryPosSetIndex(entry);
    assertIndex(set, this.posSetCount, 'POS set');
    return this.#view.getUint16(
      this.layout.posSetsOffset + set * ROOT_PAYLOAD_POS_SET_BYTES + 4,
      LITTLE_ENDIAN
    );
  }

  entryPosStringIdAt(entry: number, position: number): number {
    const set = this.entryPosSetIndex(entry);
    assertIndex(set, this.posSetCount, 'POS set');
    const setOffset = this.layout.posSetsOffset + set * ROOT_PAYLOAD_POS_SET_BYTES;
    const first = this.#view.getUint32(setOffset, LITTLE_ENDIAN);
    const count = this.#view.getUint16(setOffset + 4, LITTLE_ENDIAN);
    assertIndex(position, count, 'POS position');
    const stringId = this.#view.getUint16(
      this.layout.posMembersOffset + (first + position) * 2,
      LITTLE_ENDIAN
    );
    assertIndex(stringId, this.stringCount, 'POS string ID');
    return stringId;
  }

  /** Binary search over entries, which are canonically ordered by sequence. */
  findEntryIndex(seq: number): number {
    let low = 0;
    let high = this.entryCount - 1;
    while (low <= high) {
      const middle = (low + high) >>> 1;
      const found = this.entrySeq(middle);
      if (found < seq) low = middle + 1;
      else if (found > seq) high = middle - 1;
      else return middle;
    }
    return -1;
  }

  restrictionEntryIndex(restriction: number): number {
    return this.#view.getUint32(this.#restrictionOffset(restriction), LITTLE_ENDIAN);
  }

  restrictionReadingReference(restriction: number): number {
    return this.#view.getUint32(this.#restrictionOffset(restriction) + 4, LITTLE_ENDIAN);
  }

  restrictionWrittenReference(restriction: number): number {
    return this.#view.getUint32(this.#restrictionOffset(restriction) + 8, LITTLE_ENDIAN);
  }

  /** First restriction row for an entry; pair with `restrictionEnd`. */
  restrictionStart(entry: number): number {
    assertIndex(entry, this.entryCount, 'Entry');
    let low = 0;
    let high = this.restrictionCount;
    while (low < high) {
      const middle = (low + high) >>> 1;
      if (this.restrictionEntryIndex(middle) < entry) low = middle + 1;
      else high = middle;
    }
    return low;
  }

  /** One-past-last restriction row for an entry. */
  restrictionEnd(entry: number): number {
    assertIndex(entry, this.entryCount, 'Entry');
    let low = 0;
    let high = this.restrictionCount;
    while (low < high) {
      const middle = (low + high) >>> 1;
      if (this.restrictionEntryIndex(middle) <= entry) low = middle + 1;
      else high = middle;
    }
    return low;
  }

  string(stringId: number): string {
    assertIndex(stringId, this.stringCount, 'String ID');
    const cached = this.#strings[stringId];
    if (cached !== undefined) return cached;

    const start = this.#view.getUint32(
      this.layout.stringOffsetsOffset + stringId * 4,
      LITTLE_ENDIAN
    );
    const end = this.#view.getUint32(
      this.layout.stringOffsetsOffset + (stringId + 1) * 4,
      LITTLE_ENDIAN
    );
    let decoded: string;
    try {
      decoded = UTF8_DECODER.decode(
        this.#bytes.subarray(this.layout.stringDataOffset + start, this.layout.stringDataOffset + end)
      );
    } catch {
      throw new RootPayloadFormatError('corrupt-payload', `String ${stringId} is not valid UTF-8`);
    }
    this.#strings[stringId] = decoded;
    return decoded;
  }

  surfaceReferenceIsNone(reference: number): boolean {
    return reference === ROOT_SURFACE_REF_NONE;
  }

  surfaceReferenceIsString(reference: number): boolean {
    return isStringReference(reference);
  }

  surfaceReferenceRank(reference: number): number {
    if (reference === ROOT_SURFACE_REF_NONE || isStringReference(reference)) {
      throw new RootPayloadFormatError('out-of-range', 'Surface reference does not contain a rank');
    }
    assertIndex(reference, this.surfaceCount, 'Referenced surface rank');
    return reference;
  }

  surfaceReferenceStringId(reference: number): number {
    if (!isStringReference(reference)) {
      throw new RootPayloadFormatError('out-of-range', 'Surface reference does not contain a string ID');
    }
    const stringId = reference & ~ROOT_SURFACE_REF_STRING_BIT;
    assertIndex(stringId, this.stringCount, 'Referenced string ID');
    return stringId;
  }

  /** Resolve nullable rank/string references without coupling this reader to an FST implementation. */
  resolveSurfaceReference(
    reference: number,
    directSurface: (rank: number) => string
  ): string | null {
    if (reference === ROOT_SURFACE_REF_NONE) return null;
    if (isStringReference(reference)) return this.string(this.surfaceReferenceStringId(reference));
    return directSurface(this.surfaceReferenceRank(reference));
  }

  #formOffset(form: number): number {
    assertIndex(form, this.formCount, 'Form');
    return this.layout.formsOffset + form * ROOT_PAYLOAD_FORM_BYTES;
  }

  #entryOffset(entry: number): number {
    assertIndex(entry, this.entryCount, 'Entry');
    return this.layout.entriesOffset + entry * ROOT_PAYLOAD_ENTRY_BYTES;
  }

  #entryFlag(entry: number, flag: number): boolean {
    const flags = this.#view.getUint8(this.#entryOffset(entry) + 8);
    if ((flags & ~ENTRY_KNOWN_FLAGS) !== 0) {
      throw new RootPayloadFormatError('corrupt-payload', `Entry ${entry} has unknown flags`);
    }
    return (flags & flag) !== 0;
  }

  #restrictionOffset(restriction: number): number {
    assertIndex(restriction, this.restrictionCount, 'Restriction');
    return this.layout.restrictionsOffset + restriction * ROOT_PAYLOAD_RESTRICTION_BYTES;
  }
}

export function openRootPayload(input: ArrayBuffer | Uint8Array): RootPayloadReader {
  return new RootPayloadReader(input);
}
