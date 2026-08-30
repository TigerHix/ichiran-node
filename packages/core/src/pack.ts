import { crc32 } from './crc32.js';
import {
  PACK_DIRECTORY_ENTRY_BYTES,
  PACK_FORMAT_VERSION,
  PACK_HEADER_BYTES,
  PACK_MAGIC,
  PACK_MAX_BYTE_LENGTH,
  PACK_MAX_SECTION_ID,
  PACK_SECTION_ALIGNMENT
} from './format.js';
import type {
  PackFormatErrorCode,
  PackManifest,
  PackSection,
  PackSectionInput
} from './types.js';

const LITTLE_ENDIAN = true;

const HEADER_VERSION_OFFSET = 8;
const HEADER_SIZE_OFFSET = 10;
const HEADER_FLAGS_OFFSET = 12;
const HEADER_SECTION_COUNT_OFFSET = 16;
const HEADER_DIRECTORY_BYTES_OFFSET = 20;
const HEADER_TOTAL_BYTES_OFFSET = 24;
const HEADER_DIRECTORY_CHECKSUM_OFFSET = 28;

const ENTRY_ID_OFFSET = 0;
const ENTRY_PAYLOAD_OFFSET = 4;
const ENTRY_PAYLOAD_BYTES_OFFSET = 8;
const ENTRY_PAYLOAD_CHECKSUM_OFFSET = 12;
const ENTRY_RESERVED_0_OFFSET = 16;
const ENTRY_RESERVED_1_OFFSET = 20;

export class PackFormatError extends Error {
  readonly code: PackFormatErrorCode;

  constructor(code: PackFormatErrorCode, message: string) {
    super(message);
    this.name = 'PackFormatError';
    this.code = code;
  }
}

function asBytes(input: ArrayBuffer | Uint8Array): Uint8Array {
  return input instanceof Uint8Array ? input : new Uint8Array(input);
}

function align(value: number): number {
  return Math.ceil(value / PACK_SECTION_ALIGNMENT) * PACK_SECTION_ALIGNMENT;
}

function assertUint32(value: number, label: string, allowZero: boolean): void {
  if (
    !Number.isSafeInteger(value)
    || value < (allowZero ? 0 : 1)
    || value > PACK_MAX_SECTION_ID
  ) {
    throw new PackFormatError(
      'invalid-input',
      `${label} must be ${allowZero ? 'an unsigned' : 'a non-zero unsigned'} 32-bit integer`
    );
  }
}

function writeMagic(target: Uint8Array): void {
  for (let index = 0; index < PACK_MAGIC.length; index++) {
    target[index] = PACK_MAGIC.charCodeAt(index);
  }
}

function hasExpectedMagic(bytes: Uint8Array): boolean {
  for (let index = 0; index < PACK_MAGIC.length; index++) {
    if (bytes[index] !== PACK_MAGIC.charCodeAt(index)) return false;
  }
  return true;
}

function assertZeroPadding(bytes: Uint8Array, start: number, end: number): void {
  for (let index = start; index < end; index++) {
    if (bytes[index] !== 0) {
      throw new PackFormatError('invalid-directory', `Non-zero padding byte at offset ${index}`);
    }
  }
}

/**
 * Encode a canonical version-1 portable pack.
 *
 * Inputs are sorted by section ID without mutating the caller's array. Payloads
 * are copied, section offsets are 8-byte aligned, and all padding is zeroed.
 */
export function encodePack(inputs: readonly PackSectionInput[]): Uint8Array {
  const sections = inputs.map((input) => ({
    id: input.id,
    bytes: asBytes(input.bytes)
  }));

  sections.sort((left, right) => left.id - right.id);

  let previousId = 0;
  for (const section of sections) {
    assertUint32(section.id, 'Section ID', false);
    if (section.id === previousId) {
      throw new PackFormatError('invalid-input', `Duplicate section ID ${section.id}`);
    }
    previousId = section.id;
  }

  const directoryBytes = sections.length * PACK_DIRECTORY_ENTRY_BYTES;
  if (!Number.isSafeInteger(directoryBytes) || directoryBytes > PACK_MAX_BYTE_LENGTH - PACK_HEADER_BYTES) {
    throw new PackFormatError('invalid-input', 'Section directory is too large');
  }

  let nextOffset = align(PACK_HEADER_BYTES + directoryBytes);
  const sectionOffsets: number[] = [];

  for (const section of sections) {
    sectionOffsets.push(nextOffset);
    nextOffset = align(nextOffset + section.bytes.byteLength);
    if (!Number.isSafeInteger(nextOffset) || nextOffset > PACK_MAX_BYTE_LENGTH) {
      throw new PackFormatError('invalid-input', 'Pack exceeds the unsigned 32-bit size limit');
    }
  }

  const output = new Uint8Array(nextOffset);
  const view = new DataView(output.buffer);

  writeMagic(output);
  view.setUint16(HEADER_VERSION_OFFSET, PACK_FORMAT_VERSION, LITTLE_ENDIAN);
  view.setUint16(HEADER_SIZE_OFFSET, PACK_HEADER_BYTES, LITTLE_ENDIAN);
  view.setUint32(HEADER_FLAGS_OFFSET, 0, LITTLE_ENDIAN);
  view.setUint32(HEADER_SECTION_COUNT_OFFSET, sections.length, LITTLE_ENDIAN);
  view.setUint32(HEADER_DIRECTORY_BYTES_OFFSET, directoryBytes, LITTLE_ENDIAN);
  view.setUint32(HEADER_TOTAL_BYTES_OFFSET, output.byteLength, LITTLE_ENDIAN);

  sections.forEach((section, index) => {
    const entryOffset = PACK_HEADER_BYTES + index * PACK_DIRECTORY_ENTRY_BYTES;
    const payloadOffset = sectionOffsets[index]!;

    view.setUint32(entryOffset + ENTRY_ID_OFFSET, section.id, LITTLE_ENDIAN);
    view.setUint32(entryOffset + ENTRY_PAYLOAD_OFFSET, payloadOffset, LITTLE_ENDIAN);
    view.setUint32(entryOffset + ENTRY_PAYLOAD_BYTES_OFFSET, section.bytes.byteLength, LITTLE_ENDIAN);
    view.setUint32(entryOffset + ENTRY_PAYLOAD_CHECKSUM_OFFSET, crc32(section.bytes), LITTLE_ENDIAN);
    view.setUint32(entryOffset + ENTRY_RESERVED_0_OFFSET, 0, LITTLE_ENDIAN);
    view.setUint32(entryOffset + ENTRY_RESERVED_1_OFFSET, 0, LITTLE_ENDIAN);

    output.set(section.bytes, payloadOffset);
  });

  const directory = output.subarray(PACK_HEADER_BYTES, PACK_HEADER_BYTES + directoryBytes);
  view.setUint32(HEADER_DIRECTORY_CHECKSUM_OFFSET, crc32(directory), LITTLE_ENDIAN);

  return output;
}

function parseManifest(bytes: Uint8Array): PackManifest {
  if (bytes.byteLength < PACK_HEADER_BYTES) {
    throw new PackFormatError('invalid-header', 'Pack is shorter than its fixed header');
  }
  if (!hasExpectedMagic(bytes)) {
    throw new PackFormatError('invalid-header', `Expected ${PACK_MAGIC} magic bytes`);
  }

  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const formatVersion = view.getUint16(HEADER_VERSION_OFFSET, LITTLE_ENDIAN);
  if (formatVersion !== PACK_FORMAT_VERSION) {
    throw new PackFormatError(
      'unsupported-version',
      `Unsupported pack format version ${formatVersion}; expected ${PACK_FORMAT_VERSION}`
    );
  }

  const headerBytes = view.getUint16(HEADER_SIZE_OFFSET, LITTLE_ENDIAN);
  const flags = view.getUint32(HEADER_FLAGS_OFFSET, LITTLE_ENDIAN);
  const sectionCount = view.getUint32(HEADER_SECTION_COUNT_OFFSET, LITTLE_ENDIAN);
  const directoryBytes = view.getUint32(HEADER_DIRECTORY_BYTES_OFFSET, LITTLE_ENDIAN);
  const totalBytes = view.getUint32(HEADER_TOTAL_BYTES_OFFSET, LITTLE_ENDIAN);
  const expectedDirectoryBytes = sectionCount * PACK_DIRECTORY_ENTRY_BYTES;

  if (headerBytes !== PACK_HEADER_BYTES || flags !== 0) {
    throw new PackFormatError('invalid-header', 'Header size or reserved flags are invalid');
  }
  if (!Number.isSafeInteger(expectedDirectoryBytes) || directoryBytes !== expectedDirectoryBytes) {
    throw new PackFormatError('invalid-directory', 'Section count does not match directory size');
  }
  if (totalBytes !== bytes.byteLength) {
    throw new PackFormatError(
      'invalid-header',
      `Header declares ${totalBytes} bytes but received ${bytes.byteLength}`
    );
  }

  const directoryEnd = PACK_HEADER_BYTES + directoryBytes;
  if (directoryEnd > bytes.byteLength) {
    throw new PackFormatError('invalid-directory', 'Section directory extends past the end of the pack');
  }

  const directory = bytes.subarray(PACK_HEADER_BYTES, directoryEnd);
  const expectedDirectoryChecksum = view.getUint32(
    HEADER_DIRECTORY_CHECKSUM_OFFSET,
    LITTLE_ENDIAN
  );
  if (crc32(directory) !== expectedDirectoryChecksum) {
    throw new PackFormatError('invalid-directory', 'Section directory checksum does not match');
  }

  const sections: PackSection[] = [];
  let previousId = 0;
  let expectedPayloadOffset = align(directoryEnd);
  assertZeroPadding(bytes, directoryEnd, expectedPayloadOffset);

  for (let index = 0; index < sectionCount; index++) {
    const entryOffset = PACK_HEADER_BYTES + index * PACK_DIRECTORY_ENTRY_BYTES;
    const id = view.getUint32(entryOffset + ENTRY_ID_OFFSET, LITTLE_ENDIAN);
    const offset = view.getUint32(entryOffset + ENTRY_PAYLOAD_OFFSET, LITTLE_ENDIAN);
    const byteLength = view.getUint32(entryOffset + ENTRY_PAYLOAD_BYTES_OFFSET, LITTLE_ENDIAN);
    const checksum = view.getUint32(entryOffset + ENTRY_PAYLOAD_CHECKSUM_OFFSET, LITTLE_ENDIAN);
    const reserved0 = view.getUint32(entryOffset + ENTRY_RESERVED_0_OFFSET, LITTLE_ENDIAN);
    const reserved1 = view.getUint32(entryOffset + ENTRY_RESERVED_1_OFFSET, LITTLE_ENDIAN);

    if (id === 0 || id <= previousId) {
      throw new PackFormatError(
        'invalid-directory',
        'Section IDs must be non-zero, unique, and sorted'
      );
    }
    if (reserved0 !== 0 || reserved1 !== 0) {
      throw new PackFormatError('invalid-directory', `Section ${id} has non-zero reserved fields`);
    }
    if (offset !== expectedPayloadOffset) {
      throw new PackFormatError(
        'invalid-directory',
        `Section ${id} starts at ${offset}; expected canonical offset ${expectedPayloadOffset}`
      );
    }

    const payloadEnd = offset + byteLength;
    if (!Number.isSafeInteger(payloadEnd) || payloadEnd > bytes.byteLength) {
      throw new PackFormatError('invalid-directory', `Section ${id} extends past the end of the pack`);
    }

    sections.push(Object.freeze({ id, offset, byteLength, checksum }));
    previousId = id;

    const nextPayloadOffset = align(payloadEnd);
    assertZeroPadding(bytes, payloadEnd, nextPayloadOffset);
    expectedPayloadOffset = nextPayloadOffset;
  }

  if (expectedPayloadOffset !== bytes.byteLength) {
    throw new PackFormatError('invalid-directory', 'Pack has trailing bytes outside its sections');
  }

  return Object.freeze({
    formatVersion,
    byteLength: bytes.byteLength,
    sections: Object.freeze(sections)
  });
}

/** Parsed view over an immutable portable pack byte buffer. */
export class PackReader {
  readonly manifest: PackManifest;

  readonly #bytes: Uint8Array;
  readonly #sectionsById = new Map<number, PackSection>();
  readonly #verifiedSections = new Set<number>();

  constructor(input: ArrayBuffer | Uint8Array) {
    this.#bytes = asBytes(input);
    this.manifest = parseManifest(this.#bytes);
    for (const section of this.manifest.sections) {
      this.#sectionsById.set(section.id, section);
    }
  }

  hasSection(id: number): boolean {
    return this.#sectionsById.has(id);
  }

  /**
   * Return a zero-copy view of a section after checking its CRC-32 once.
   * The returned bytes share the pack's backing buffer and must not be mutated.
   */
  getSection(id: number): Uint8Array {
    const section = this.#sectionsById.get(id);
    if (!section) {
      throw new PackFormatError('missing-section', `Pack has no section ${id}`);
    }

    const bytes = this.#bytes.subarray(section.offset, section.offset + section.byteLength);
    if (!this.#verifiedSections.has(id)) {
      const actualChecksum = crc32(bytes);
      if (actualChecksum !== section.checksum) {
        throw new PackFormatError(
          'corrupt-section',
          `Section ${id} checksum does not match`
        );
      }
      this.#verifiedSections.add(id);
    }

    return bytes;
  }

  verifyAll(): void {
    for (const section of this.manifest.sections) {
      this.getSection(section.id);
    }
  }
}

export function openPack(input: ArrayBuffer | Uint8Array): PackReader {
  return new PackReader(input);
}
