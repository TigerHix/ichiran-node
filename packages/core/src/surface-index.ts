/** Section ID of the route-aware surface automaton in the hot analyzer pack. */
export const SURFACE_INDEX_SECTION_ID = 1;

export const SURFACE_INDEX_MAGIC = 'ICHISURF';
export const SURFACE_INDEX_FORMAT_VERSION = 1;
export const SURFACE_INDEX_HEADER_BYTES = 64;
export const SURFACE_INDEX_STATE_BYTES = 8;
export const SURFACE_INDEX_EDGE_BYTES = 4;

const LITTLE_ENDIAN = true;
const MAX_PACKED_STATE_COUNT = 0x0100_0000;
const DIRECT_TERMINAL_FLAG = 0x4000_0000;
const MORPHOLOGY_TERMINAL_FLAG = 0x8000_0000;

const STATE_COUNT_OFFSET = 16;
const EDGE_COUNT_OFFSET = 20;
const ACCEPTED_COUNT_OFFSET = 24;
const DIRECT_COUNT_OFFSET = 28;
const MORPHOLOGY_COUNT_OFFSET = 32;
const OVERLAP_COUNT_OFFSET = 36;
const INPUT_COUNT_OFFSET = 40;
const ROOT_STATE_OFFSET = 44;
const STATES_OFFSET_OFFSET = 48;
const EDGES_OFFSET_OFFSET = 52;
const TOTAL_BYTES_OFFSET = 56;
const STATE_BYTES_OFFSET = 60;
const EDGE_BYTES_OFFSET = 62;

export type SurfaceRoute = 'kana' | 'kanji';

export type SurfaceIndexFormatErrorCode =
  | 'invalid-header'
  | 'unsupported-version'
  | 'invalid-states'
  | 'invalid-edges'
  | 'out-of-range';

export class SurfaceIndexFormatError extends Error {
  readonly code: SurfaceIndexFormatErrorCode;

  constructor(code: SurfaceIndexFormatErrorCode, message: string) {
    super(message);
    this.name = 'SurfaceIndexFormatError';
    this.code = code;
  }
}

export interface SurfaceIndexManifest {
  readonly byteLength: number;
  readonly stateCount: number;
  readonly edgeCount: number;
  readonly acceptedCount: number;
  readonly directCount: number;
  readonly morphologyCount: number;
  readonly overlapCount: number;
  readonly inputCount: number;
}

export interface SurfaceMatch {
  /** Exclusive UTF-16 offset in the caller's input string. */
  readonly end: number;
  readonly route: SurfaceRoute;
  readonly direct: boolean;
  readonly morphology: boolean;
  /** Zero-based UTF-8 byte-lexicographic rank among active direct surfaces. */
  readonly directRank: number | null;
}

interface Walk {
  state: number;
  directRank: number;
}

function asBytes(input: ArrayBuffer | Uint8Array): Uint8Array {
  return input instanceof Uint8Array ? input : new Uint8Array(input);
}

function hasMagic(bytes: Uint8Array): boolean {
  if (bytes.byteLength < SURFACE_INDEX_MAGIC.length) return false;
  for (let index = 0; index < SURFACE_INDEX_MAGIC.length; index++) {
    if (bytes[index] !== SURFACE_INDEX_MAGIC.charCodeAt(index)) return false;
  }
  return true;
}

function checkedSum(left: number, right: number, label: string): number {
  const value = left + right;
  if (!Number.isSafeInteger(value) || value > 0xffff_ffff) {
    throw new SurfaceIndexFormatError('invalid-states', `${label} exceeds uint32`);
  }
  return value;
}

function isKanaCodePoint(codePoint: number): boolean {
  return (
    (codePoint >= 0x30a1 && codePoint <= 0x30fa)
    || codePoint === 0x30fd
    || codePoint === 0x30fe
    || codePoint === 0x30fc
    || (codePoint >= 0x3041 && codePoint <= 0x3094)
    || codePoint === 0x309d
    || codePoint === 0x309e
  );
}

/** Exact browser equivalent of core's non-empty `testWord(surface, 'kana')`. */
export function surfaceRoute(surface: string): SurfaceRoute {
  if (surface.length === 0) return 'kanji';
  for (let offset = 0; offset < surface.length;) {
    const codePoint = surface.codePointAt(offset)!;
    if (!isKanaCodePoint(codePoint)) return 'kanji';
    offset += codePoint > 0xffff ? 2 : 1;
  }
  return 'kana';
}

function codePointWidth(text: string, offset: number, codePoint: number): number {
  return codePoint > 0xffff && offset + 1 < text.length ? 2 : 1;
}

function scalarValue(codePoint: number): number {
  return codePoint >= 0xd800 && codePoint <= 0xdfff ? 0xfffd : codePoint;
}

function decodeUtf8(bytes: readonly number[]): string {
  const codePoints: number[] = [];
  for (let offset = 0; offset < bytes.length;) {
    const first = bytes[offset++]!;
    if (first < 0x80) {
      codePoints.push(first);
    } else if (first < 0xe0 && offset < bytes.length) {
      codePoints.push(((first & 0x1f) << 6) | (bytes[offset++]! & 0x3f));
    } else if (first < 0xf0 && offset + 1 < bytes.length) {
      codePoints.push(
        ((first & 0x0f) << 12)
        | ((bytes[offset++]! & 0x3f) << 6)
        | (bytes[offset++]! & 0x3f)
      );
    } else if (offset + 2 < bytes.length) {
      codePoints.push(
        ((first & 0x07) << 18)
        | ((bytes[offset++]! & 0x3f) << 12)
        | ((bytes[offset++]! & 0x3f) << 6)
        | (bytes[offset++]! & 0x3f)
      );
    } else {
      codePoints.push(0xfffd);
    }
  }
  return String.fromCodePoint(...codePoints);
}

/** Zero-copy reader over the production route-aware surface automaton. */
export class SurfaceIndex {
  readonly manifest: SurfaceIndexManifest;

  readonly #bytes: Uint8Array;
  readonly #view: DataView;
  readonly #stateCount: number;
  readonly #edgeCount: number;
  readonly #root: number;
  readonly #statesOffset: number;
  readonly #edgesOffset: number;

  constructor(input: ArrayBuffer | Uint8Array) {
    this.#bytes = asBytes(input);
    if (this.#bytes.byteLength < SURFACE_INDEX_HEADER_BYTES || !hasMagic(this.#bytes)) {
      throw new SurfaceIndexFormatError('invalid-header', 'Expected a complete ICHISURF header');
    }
    this.#view = new DataView(
      this.#bytes.buffer,
      this.#bytes.byteOffset,
      this.#bytes.byteLength
    );

    const version = this.#view.getUint16(8, LITTLE_ENDIAN);
    if (version !== SURFACE_INDEX_FORMAT_VERSION) {
      throw new SurfaceIndexFormatError(
        'unsupported-version',
        `Unsupported surface-index version ${version}`
      );
    }
    if (
      this.#view.getUint16(10, LITTLE_ENDIAN) !== SURFACE_INDEX_HEADER_BYTES
      || this.#view.getUint32(12, LITTLE_ENDIAN) !== 0
      || this.#view.getUint16(STATE_BYTES_OFFSET, LITTLE_ENDIAN) !== SURFACE_INDEX_STATE_BYTES
      || this.#view.getUint16(EDGE_BYTES_OFFSET, LITTLE_ENDIAN) !== SURFACE_INDEX_EDGE_BYTES
    ) {
      throw new SurfaceIndexFormatError('invalid-header', 'Header sizes or reserved flags are invalid');
    }

    this.#stateCount = this.#view.getUint32(STATE_COUNT_OFFSET, LITTLE_ENDIAN);
    this.#edgeCount = this.#view.getUint32(EDGE_COUNT_OFFSET, LITTLE_ENDIAN);
    const acceptedCount = this.#view.getUint32(ACCEPTED_COUNT_OFFSET, LITTLE_ENDIAN);
    const directCount = this.#view.getUint32(DIRECT_COUNT_OFFSET, LITTLE_ENDIAN);
    const morphologyCount = this.#view.getUint32(MORPHOLOGY_COUNT_OFFSET, LITTLE_ENDIAN);
    const overlapCount = this.#view.getUint32(OVERLAP_COUNT_OFFSET, LITTLE_ENDIAN);
    const inputCount = this.#view.getUint32(INPUT_COUNT_OFFSET, LITTLE_ENDIAN);
    this.#root = this.#view.getUint32(ROOT_STATE_OFFSET, LITTLE_ENDIAN);
    this.#statesOffset = this.#view.getUint32(STATES_OFFSET_OFFSET, LITTLE_ENDIAN);
    this.#edgesOffset = this.#view.getUint32(EDGES_OFFSET_OFFSET, LITTLE_ENDIAN);
    const totalBytes = this.#view.getUint32(TOTAL_BYTES_OFFSET, LITTLE_ENDIAN);

    if (
      this.#stateCount === 0
      || this.#stateCount > MAX_PACKED_STATE_COUNT
      || this.#root !== this.#stateCount - 1
    ) {
      throw new SurfaceIndexFormatError('invalid-states', 'State count or root state is invalid');
    }
    const expectedEdgesOffset = SURFACE_INDEX_HEADER_BYTES
      + (this.#stateCount + 1) * SURFACE_INDEX_STATE_BYTES;
    const expectedTotalBytes = expectedEdgesOffset + this.#edgeCount * SURFACE_INDEX_EDGE_BYTES;
    if (
      this.#statesOffset !== SURFACE_INDEX_HEADER_BYTES
      || this.#edgesOffset !== expectedEdgesOffset
      || totalBytes !== expectedTotalBytes
      || totalBytes !== this.#bytes.byteLength
    ) {
      throw new SurfaceIndexFormatError('invalid-header', 'Surface-index offsets or byte length are invalid');
    }
    if (
      overlapCount > directCount
      || overlapCount > morphologyCount
      || acceptedCount !== directCount + morphologyCount - overlapCount
      || inputCount < acceptedCount
    ) {
      throw new SurfaceIndexFormatError('invalid-header', 'Surface counts are inconsistent');
    }

    this.#validate(acceptedCount, directCount, morphologyCount, overlapCount);
    this.manifest = Object.freeze({
      byteLength: this.#bytes.byteLength,
      stateCount: this.#stateCount,
      edgeCount: this.#edgeCount,
      acceptedCount,
      directCount,
      morphologyCount,
      overlapCount,
      inputCount
    });
  }

  /** Exact lookup of one complete surface. */
  lookup(surface: string): SurfaceMatch | null {
    if (surface.length === 0) return null;
    const walk: Walk = { state: this.#root, directRank: 0 };
    let kana = true;

    for (let offset = 0; offset < surface.length;) {
      const codePoint = surface.codePointAt(offset)!;
      kana = kana && isKanaCodePoint(codePoint);
      if (!this.#advanceCodePoint(walk, codePoint)) return null;
      offset += codePointWidth(surface, offset, codePoint);
    }

    const flags = this.#stateFlags(walk.state);
    const direct = (flags & DIRECT_TERMINAL_FLAG) !== 0;
    const morphology = (flags & MORPHOLOGY_TERMINAL_FLAG) !== 0;
    if (!direct && !morphology) return null;
    return {
      end: surface.length,
      route: kana ? 'kana' : 'kanji',
      direct,
      morphology,
      directRank: direct ? walk.directRank : null
    };
  }

  /**
   * Return every accepted surface beginning at a UTF-16 input offset.
   * The 50-code-unit default preserves the analyzer's current lookup ceiling.
   */
  scan(text: string, start = 0, maxCodeUnits = 50): SurfaceMatch[] {
    if (!Number.isSafeInteger(start) || start < 0 || start > text.length) {
      throw new SurfaceIndexFormatError('out-of-range', 'Scan start is outside the input string');
    }
    if (!Number.isSafeInteger(maxCodeUnits) || maxCodeUnits < 1) {
      throw new SurfaceIndexFormatError('out-of-range', 'Scan length must be a positive integer');
    }

    const matches: SurfaceMatch[] = [];
    const walk: Walk = { state: this.#root, directRank: 0 };
    let kana = true;
    let offset = start;

    while (offset < text.length) {
      const codePoint = text.codePointAt(offset)!;
      const width = codePointWidth(text, offset, codePoint);
      if (offset + width - start > maxCodeUnits) break;
      kana = kana && isKanaCodePoint(codePoint);
      if (!this.#advanceCodePoint(walk, codePoint)) break;
      offset += width;

      const flags = this.#stateFlags(walk.state);
      const direct = (flags & DIRECT_TERMINAL_FLAG) !== 0;
      const morphology = (flags & MORPHOLOGY_TERMINAL_FLAG) !== 0;
      if (direct || morphology) {
        matches.push({
          end: offset,
          route: kana ? 'kana' : 'kanji',
          direct,
          morphology,
          directRank: direct ? walk.directRank : null
        });
      }
    }
    return matches;
  }

  /** Recover an active direct surface from its dense lexicographic rank. */
  directSurface(rank: number): string {
    if (!Number.isSafeInteger(rank) || rank < 0 || rank >= this.manifest.directCount) {
      throw new SurfaceIndexFormatError('out-of-range', `Direct rank ${rank} is outside the index`);
    }

    let state = this.#root;
    let remaining = rank;
    const bytes: number[] = [];

    for (;;) {
      if ((this.#stateFlags(state) & DIRECT_TERMINAL_FLAG) !== 0) {
        if (remaining === 0) return decodeUtf8(bytes);
        remaining--;
      }

      const start = this.#stateFirstEdge(state);
      const end = this.#stateFirstEdge(state + 1);
      let descended = false;
      for (let edge = start; edge < end; edge++) {
        const target = this.#edgeTarget(edge);
        const count = this.#stateDirectCount(target);
        if (remaining < count) {
          bytes.push(this.#edgeLabel(edge));
          state = target;
          descended = true;
          break;
        }
        remaining -= count;
      }
      if (!descended) {
        throw new SurfaceIndexFormatError('invalid-states', 'Direct rank traversal reached no child');
      }
    }
  }

  #validate(
    acceptedCount: number,
    directCount: number,
    morphologyCount: number,
    overlapCount: number
  ): void {
    if (this.#stateFirstEdge(0) !== 0) {
      throw new SurfaceIndexFormatError('invalid-states', 'First state must begin at edge zero');
    }
    if (
      this.#stateFirstEdge(this.#stateCount) !== this.#edgeCount
      || this.#stateFlags(this.#stateCount) !== 0
      || this.#stateDirectCount(this.#stateCount) !== 0
    ) {
      throw new SurfaceIndexFormatError('invalid-states', 'State sentinel is invalid');
    }

    const languageCounts = new Uint32Array(this.#stateCount * 3);
    for (let state = 0; state < this.#stateCount; state++) {
      const start = this.#stateFirstEdge(state);
      const end = this.#stateFirstEdge(state + 1);
      if (start > end || end > this.#edgeCount) {
        throw new SurfaceIndexFormatError('invalid-states', `State ${state} has an invalid edge span`);
      }

      const flags = this.#stateFlags(state);
      let direct = (flags & DIRECT_TERMINAL_FLAG) !== 0 ? 1 : 0;
      let accepted = (flags & (DIRECT_TERMINAL_FLAG | MORPHOLOGY_TERMINAL_FLAG)) !== 0 ? 1 : 0;
      let morphology = (flags & MORPHOLOGY_TERMINAL_FLAG) !== 0 ? 1 : 0;
      let overlap = (flags & (DIRECT_TERMINAL_FLAG | MORPHOLOGY_TERMINAL_FLAG))
        === (DIRECT_TERMINAL_FLAG | MORPHOLOGY_TERMINAL_FLAG) ? 1 : 0;
      let previousLabel = -1;

      for (let edge = start; edge < end; edge++) {
        const label = this.#edgeLabel(edge);
        const target = this.#edgeTarget(edge);
        if (label <= previousLabel || target >= state) {
          throw new SurfaceIndexFormatError(
            'invalid-edges',
            `State ${state} edges are unsorted or not bottom-up`
          );
        }
        previousLabel = label;
        direct = checkedSum(direct, this.#stateDirectCount(target), 'Direct subtree count');
        accepted = checkedSum(accepted, languageCounts[target * 3]!, 'Accepted subtree count');
        morphology = checkedSum(
          morphology,
          languageCounts[target * 3 + 1]!,
          'Morphology subtree count'
        );
        overlap = checkedSum(overlap, languageCounts[target * 3 + 2]!, 'Overlap subtree count');
      }

      if (direct !== this.#stateDirectCount(state)) {
        throw new SurfaceIndexFormatError('invalid-states', `State ${state} direct count is invalid`);
      }
      languageCounts[state * 3] = accepted;
      languageCounts[state * 3 + 1] = morphology;
      languageCounts[state * 3 + 2] = overlap;
    }

    const rootOffset = this.#root * 3;
    if (
      (this.#stateFlags(this.#root) & (DIRECT_TERMINAL_FLAG | MORPHOLOGY_TERMINAL_FLAG)) !== 0
      || this.#stateDirectCount(this.#root) !== directCount
      || languageCounts[rootOffset] !== acceptedCount
      || languageCounts[rootOffset + 1] !== morphologyCount
      || languageCounts[rootOffset + 2] !== overlapCount
    ) {
      throw new SurfaceIndexFormatError('invalid-states', 'Root language counts are inconsistent');
    }
  }

  #advanceCodePoint(walk: Walk, inputCodePoint: number): boolean {
    const codePoint = scalarValue(inputCodePoint);
    if (codePoint < 0x80) return this.#advanceByte(walk, codePoint);
    if (codePoint < 0x800) {
      return this.#advanceByte(walk, 0xc0 | (codePoint >> 6))
        && this.#advanceByte(walk, 0x80 | (codePoint & 0x3f));
    }
    if (codePoint < 0x10000) {
      return this.#advanceByte(walk, 0xe0 | (codePoint >> 12))
        && this.#advanceByte(walk, 0x80 | ((codePoint >> 6) & 0x3f))
        && this.#advanceByte(walk, 0x80 | (codePoint & 0x3f));
    }
    return this.#advanceByte(walk, 0xf0 | (codePoint >> 18))
      && this.#advanceByte(walk, 0x80 | ((codePoint >> 12) & 0x3f))
      && this.#advanceByte(walk, 0x80 | ((codePoint >> 6) & 0x3f))
      && this.#advanceByte(walk, 0x80 | (codePoint & 0x3f));
  }

  #advanceByte(walk: Walk, label: number): boolean {
    const flags = this.#stateFlags(walk.state);
    if ((flags & DIRECT_TERMINAL_FLAG) !== 0) walk.directRank++;

    const start = this.#stateFirstEdge(walk.state);
    const end = this.#stateFirstEdge(walk.state + 1);
    for (let edge = start; edge < end; edge++) {
      const edgeLabel = this.#edgeLabel(edge);
      const target = this.#edgeTarget(edge);
      if (edgeLabel < label) {
        walk.directRank += this.#stateDirectCount(target);
      } else if (edgeLabel === label) {
        walk.state = target;
        return true;
      } else {
        return false;
      }
    }
    return false;
  }

  #stateFirstEdge(state: number): number {
    return this.#view.getUint32(
      this.#statesOffset + state * SURFACE_INDEX_STATE_BYTES,
      LITTLE_ENDIAN
    );
  }

  #stateSecondWord(state: number): number {
    return this.#view.getUint32(
      this.#statesOffset + state * SURFACE_INDEX_STATE_BYTES + 4,
      LITTLE_ENDIAN
    );
  }

  #stateFlags(state: number): number {
    return this.#stateSecondWord(state) & (DIRECT_TERMINAL_FLAG | MORPHOLOGY_TERMINAL_FLAG);
  }

  #stateDirectCount(state: number): number {
    return this.#stateSecondWord(state) & 0x3fff_ffff;
  }

  #edgeLabel(edge: number): number {
    return this.#bytes[this.#edgesOffset + edge * SURFACE_INDEX_EDGE_BYTES]!;
  }

  #edgeTarget(edge: number): number {
    const offset = this.#edgesOffset + edge * SURFACE_INDEX_EDGE_BYTES + 1;
    return this.#bytes[offset]!
      | (this.#bytes[offset + 1]! << 8)
      | (this.#bytes[offset + 2]! << 16);
  }
}

export function openSurfaceIndex(input: ArrayBuffer | Uint8Array): SurfaceIndex {
  return new SurfaceIndex(input);
}
