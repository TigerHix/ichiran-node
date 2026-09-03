import { describe, expect, test } from 'bun:test';
import {
  encodePack,
  openPack,
  PackFormatError,
  PACK_DIRECTORY_ENTRY_BYTES,
  PACK_FORMAT_VERSION,
  PACK_HEADER_BYTES,
  PACK_SECTION_ALIGNMENT
} from '../src/compiler.js';

function expectPackError(action: () => unknown, code: PackFormatError['code']): void {
  try {
    action();
    throw new Error(`Expected PackFormatError with code ${code}`);
  } catch (error) {
    expect(error).toBeInstanceOf(PackFormatError);
    expect((error as PackFormatError).code).toBe(code);
  }
}

describe('portable pack format', () => {
  test('encodes deterministically and reads sorted, aligned sections', () => {
    const first = new Uint8Array([1, 2, 3, 4]);
    const backing = new Uint8Array([99, 5, 6, 7, 98]);
    const secondSubview = backing.subarray(1, 4);
    const input = [
      { id: 9, bytes: first },
      { id: 2, bytes: secondSubview }
    ] as const;

    const encoded = encodePack(input);
    const encodedAgain = encodePack(input);
    expect(encodedAgain).toEqual(encoded);

    const reader = openPack(encoded);
    expect(reader.manifest.formatVersion).toBe(PACK_FORMAT_VERSION);
    expect(reader.manifest.byteLength).toBe(encoded.byteLength);
    expect(reader.manifest.sections.map((section) => section.id)).toEqual([2, 9]);
    expect(reader.manifest.sections[0]!.offset % PACK_SECTION_ALIGNMENT).toBe(0);
    expect(reader.manifest.sections[1]!.offset % PACK_SECTION_ALIGNMENT).toBe(0);
    expect(reader.hasSection(2)).toBe(true);
    expect(reader.hasSection(3)).toBe(false);
    expect(reader.getSection(2)).toEqual(new Uint8Array([5, 6, 7]));
    expect(reader.getSection(9)).toEqual(first);
    reader.verifyAll();

    // Encoding sorts its copy, not the caller's array.
    expect(input.map((section) => section.id)).toEqual([9, 2]);
  });

  test('reads a pack from a Uint8Array with a non-zero byte offset', () => {
    const encoded = encodePack([{ id: 1, bytes: new Uint8Array([4, 8, 15, 16, 23, 42]) }]);
    const wrapped = new Uint8Array(encoded.byteLength + 6);
    wrapped.set(encoded, 3);

    const reader = openPack(wrapped.subarray(3, 3 + encoded.byteLength));
    expect(reader.getSection(1)).toEqual(new Uint8Array([4, 8, 15, 16, 23, 42]));
  });

  test('supports an empty canonical pack', () => {
    const encoded = encodePack([]);
    const reader = openPack(encoded.buffer);

    expect(encoded.byteLength).toBe(PACK_HEADER_BYTES);
    expect(reader.manifest.sections).toEqual([]);
    reader.verifyAll();
  });

  test('rejects invalid and duplicate section IDs', () => {
    expectPackError(
      () => encodePack([{ id: 0, bytes: new Uint8Array() }]),
      'invalid-input'
    );
    expectPackError(
      () => encodePack([
        { id: 7, bytes: new Uint8Array([1]) },
        { id: 7, bytes: new Uint8Array([2]) }
      ]),
      'invalid-input'
    );
  });

  test('rejects bad magic, unsupported versions, and truncation', () => {
    const encoded = encodePack([{ id: 1, bytes: new Uint8Array([1, 2, 3]) }]);

    const badMagic = encoded.slice();
    badMagic[0] ^= 0xff;
    expectPackError(() => openPack(badMagic), 'invalid-header');

    const badVersion = encoded.slice();
    new DataView(badVersion.buffer).setUint16(8, PACK_FORMAT_VERSION + 1, true);
    expectPackError(() => openPack(badVersion), 'unsupported-version');

    expectPackError(
      () => openPack(encoded.subarray(0, encoded.byteLength - 1)),
      'invalid-header'
    );
  });

  test('detects directory and payload corruption separately', () => {
    const encoded = encodePack([{ id: 3, bytes: new Uint8Array([10, 20, 30, 40]) }]);

    const corruptDirectory = encoded.slice();
    corruptDirectory[PACK_HEADER_BYTES] ^= 0x01;
    expectPackError(() => openPack(corruptDirectory), 'invalid-directory');

    const intactReader = openPack(encoded);
    const payloadOffset = intactReader.manifest.sections[0]!.offset;
    const corruptPayload = encoded.slice();
    corruptPayload[payloadOffset] ^= 0x01;
    const corruptReader = openPack(corruptPayload);
    expectPackError(() => corruptReader.getSection(3), 'corrupt-section');
    expectPackError(() => corruptReader.verifyAll(), 'corrupt-section');
  });

  test('rejects non-canonical bytes outside section payloads', () => {
    const encoded = encodePack([{ id: 1, bytes: new Uint8Array([1]) }]);
    const directoryEnd = PACK_HEADER_BYTES + PACK_DIRECTORY_ENTRY_BYTES;
    const firstPayloadOffset = Math.ceil(directoryEnd / PACK_SECTION_ALIGNMENT) * PACK_SECTION_ALIGNMENT;
    expect(firstPayloadOffset).toBeGreaterThanOrEqual(directoryEnd);

    const corruptPadding = encoded.slice();
    const trailingPaddingOffset = firstPayloadOffset + 1;
    corruptPadding[trailingPaddingOffset] = 1;
    expectPackError(() => openPack(corruptPadding), 'invalid-directory');
  });

  test('reports a missing section without returning undefined', () => {
    const reader = openPack(encodePack([{ id: 1, bytes: new Uint8Array([1]) }]));
    expectPackError(() => reader.getSection(99), 'missing-section');
  });
});
