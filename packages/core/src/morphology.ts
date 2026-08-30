const MAGIC = 'ICHIMOR1';
const VERSION = 1;
const HEADER_BYTES = 144;
const NONE = 0xffff_ffff;

const POS_BYTES = 4;
const RULE_BYTES = 20;
const SUFFIX_BYTES = 12;
const TEMPLATE_BYTES = 12;
const ROOT_KEY_BYTES = 16;
const ROOT_RECORD_BYTES = 16;
const ROOT_GROUP_BYTES = 12;
const ROOT_FORM_BYTES = 4;
const PATCH_BUCKET_BYTES = 12;
const PATCH_BYTES = 40;
const TOMBSTONE_BYTES = 20;

/** Section ID reserved for reverse morphology in the alpha hot pack. */
export const MORPHOLOGY_SECTION_ID = 3;

export type MorphologyRoute = 'kana' | 'kanji';

export interface MorphologyProperty {
  pos: string;
  type: number;
  negative: boolean | null;
  formal: boolean | null;
  ordinal: number;
}

export interface MorphologyCandidate {
  route: MorphologyRoute;
  surface: string;
  rootSeq: number;
  /** Exact source row used for common/order inheritance. */
  sourceText: string;
  sourceForm: string;
  sourceReading: string;
  form: string;
  reading: string;
  intermediate: string | null;
  /** Stable only within this pinned morphology section; useful for compact overlays. */
  ruleIds: readonly [number] | readonly [number, number];
  path: readonly [MorphologyProperty] | readonly [MorphologyProperty, MorphologyProperty];
  ord: number;
  common: number | null;
  compatibility: 'rule' | 'manual';
}

export interface MorphologyStats {
  byteLength: number;
  positions: number;
  rules: number;
  suffixes: number;
  templates: number;
  rootKeys: number;
  rootRecords: number;
  rootGroups: number;
  rootForms: number;
  patches: number;
  tombstones: number;
}

export class MorphologyFormatError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'MorphologyFormatError';
  }
}

type Header = MorphologyStats & {
  rootHashSlots: number;
  patchBuckets: number;
  strings: number;
  stringCodeUnits: number;
  posOffset: number;
  ruleOffset: number;
  suffixOffset: number;
  templateOffset: number;
  rootKeyOffset: number;
  rootRecordOffset: number;
  rootHashOffset: number;
  rootGroupOffset: number;
  rootFormOffset: number;
  patchBucketOffset: number;
  patchOffset: number;
  stringDirOffset: number;
  stringPoolOffset: number;
  tombstoneOffset: number;
};

function align(value: number, alignment: number): number {
  return Math.ceil(value / alignment) * alignment;
}

function asBytes(input: ArrayBuffer | Uint8Array): Uint8Array {
  return input instanceof Uint8Array ? input : new Uint8Array(input);
}

function routeCode(route: MorphologyRoute): number {
  return route === 'kana' ? 0 : 1;
}

function routeFromCode(code: number): MorphologyRoute {
  if (code === 0) return 'kana';
  if (code === 1) return 'kanji';
  throw new MorphologyFormatError(`Invalid morphology route code ${code}`);
}

function triFromCode(code: number): boolean | null {
  if (code === 0) return false;
  if (code === 1) return true;
  if (code === 2) return null;
  throw new MorphologyFormatError(`Invalid tri-state code ${code}`);
}

function hashRootKey(route: MorphologyRoute, posId: number, text: string): number {
  let hash = 0x811c9dc5;
  hash = Math.imul(hash ^ routeCode(route), 0x01000193);
  hash = Math.imul(hash ^ (posId & 0xff), 0x01000193);
  hash = Math.imul(hash ^ (posId >>> 8), 0x01000193);
  for (let index = 0; index < text.length; index++) {
    const code = text.charCodeAt(index);
    hash = Math.imul(hash ^ (code & 0xff), 0x01000193);
    hash = Math.imul(hash ^ (code >>> 8), 0x01000193);
  }
  return hash >>> 0;
}

function hasMagic(bytes: Uint8Array): boolean {
  if (bytes.byteLength < MAGIC.length) return false;
  for (let index = 0; index < MAGIC.length; index++) {
    if (bytes[index] !== MAGIC.charCodeAt(index)) return false;
  }
  return true;
}

function checkedEnd(offset: number, count: number, stride: number, total: number, label: string): number {
  const end = offset + count * stride;
  if (!Number.isSafeInteger(end) || offset < HEADER_BYTES || end > total) {
    throw new MorphologyFormatError(`${label} lies outside the morphology section`);
  }
  return end;
}

function canonicalCandidateKey(candidate: MorphologyCandidate): string {
  const path = candidate.path.map(property => [
    property.pos,
    property.type,
    property.negative,
    property.formal,
    property.ordinal
  ]);
  return JSON.stringify([
    candidate.route,
    candidate.surface,
    candidate.rootSeq,
    candidate.sourceForm,
    candidate.sourceReading,
    path
  ]);
}

function compareCodeUnits(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

export class MorphologyReader {
  readonly stats: MorphologyStats;

  private readonly bytes: Uint8Array;
  private readonly view: DataView;
  private readonly header: Header;
  private readonly stringCache: Array<string | undefined>;
  private readonly positions: string[];
  private readonly suffixesByLength = new Map<number, Map<string, readonly [number, number]>>();
  private readonly suffixLengths: number[];
  private readonly patchesBySurface = new Map<string, readonly [number, number]>();
  private readonly tombstones = new Set<string>();

  constructor(input: ArrayBuffer | Uint8Array) {
    this.bytes = asBytes(input);
    this.view = new DataView(this.bytes.buffer, this.bytes.byteOffset, this.bytes.byteLength);
    this.header = this.readHeader();
    this.stats = {
      byteLength: this.header.byteLength,
      positions: this.header.positions,
      rules: this.header.rules,
      suffixes: this.header.suffixes,
      templates: this.header.templates,
      rootKeys: this.header.rootKeys,
      rootRecords: this.header.rootRecords,
      rootGroups: this.header.rootGroups,
      rootForms: this.header.rootForms,
      patches: this.header.patches,
      tombstones: this.header.tombstones
    };
    this.stringCache = new Array(this.header.strings);
    this.validateLayout();

    this.positions = new Array(this.header.positions);
    for (let index = 0; index < this.header.positions; index++) {
      const id = this.u32(this.header.posOffset + index * POS_BYTES);
      this.positions[index] = this.string(id);
      if (index > 0 && this.positions[index]! <= this.positions[index - 1]!) {
        throw new MorphologyFormatError('POS table is not strictly sorted');
      }
    }

    let nextTemplate = 0;
    for (let index = 0; index < this.header.suffixes; index++) {
      const at = this.header.suffixOffset + index * SUFFIX_BYTES;
      const suffix = this.string(this.u32(at));
      const first = this.u32(at + 4);
      const count = this.u32(at + 8);
      if (first !== nextTemplate || count === 0 || first + count > this.header.templates) {
        throw new MorphologyFormatError('Suffix buckets do not canonically cover the template table');
      }
      nextTemplate += count;
      let bucket = this.suffixesByLength.get(suffix.length);
      if (!bucket) {
        bucket = new Map();
        this.suffixesByLength.set(suffix.length, bucket);
      }
      if (bucket.has(suffix)) throw new MorphologyFormatError(`Duplicate suffix bucket ${JSON.stringify(suffix)}`);
      bucket.set(suffix, [first, count]);
    }
    if (nextTemplate !== this.header.templates) {
      throw new MorphologyFormatError('Suffix buckets leave templates uncovered');
    }
    this.suffixLengths = [...this.suffixesByLength.keys()].sort((left, right) => left - right);

    let nextPatch = 0;
    for (let index = 0; index < this.header.patchBuckets; index++) {
      const at = this.header.patchBucketOffset + index * PATCH_BUCKET_BYTES;
      const surface = this.string(this.u32(at));
      const first = this.u32(at + 4);
      const count = this.u16(at + 8);
      const route = routeFromCode(this.u8(at + 10));
      if (first !== nextPatch || count === 0 || first + count > this.header.patches) {
        throw new MorphologyFormatError('Patch buckets do not canonically cover the patch table');
      }
      nextPatch += count;
      const key = `${routeCode(route)}\u0000${surface}`;
      if (this.patchesBySurface.has(key)) throw new MorphologyFormatError(`Duplicate patch bucket ${JSON.stringify(key)}`);
      this.patchesBySurface.set(key, [first, count]);
    }
    if (nextPatch !== this.header.patches) {
      throw new MorphologyFormatError('Patch buckets leave patches uncovered');
    }

    for (let index = 0; index < this.header.tombstones; index++) {
      const at = this.header.tombstoneOffset + index * TOMBSTONE_BYTES;
      const rootSeq = this.u32(at);
      const surface = this.string(this.u32(at + 4));
      const firstRule = this.u32(at + 8);
      const secondRule = this.u32(at + 12);
      const route = routeFromCode(this.u8(at + 16));
      this.assertRuleId(firstRule);
      if (secondRule !== NONE) this.assertRuleId(secondRule);
      const key = this.tombstoneKey(route, surface, rootSeq, firstRule, secondRule);
      if (this.tombstones.has(key)) throw new MorphologyFormatError(`Duplicate tombstone ${key}`);
      this.tombstones.add(key);
    }

  }

  lookup(surface: string, route: MorphologyRoute): MorphologyCandidate[] {
    const candidates: MorphologyCandidate[] = [];
    const seen = new Set<string>();

    for (const suffixLength of this.suffixLengths) {
      if (suffixLength > surface.length) continue;
      const suffix = suffixLength === 0 ? '' : surface.slice(-suffixLength);
      const bucket = this.suffixesByLength.get(suffixLength)?.get(suffix);
      if (!bucket) continue;
      const prefix = suffixLength === 0 ? surface : surface.slice(0, -suffixLength);

      for (let relative = 0; relative < bucket[1]; relative++) {
        const templateIndex = bucket[0] + relative;
        const templateAt = this.header.templateOffset + templateIndex * TEMPLATE_BYTES;
        const removed = this.string(this.u32(templateAt));
        const firstRule = this.u32(templateAt + 4);
        const secondRule = this.u32(templateAt + 8);
        this.assertRuleId(firstRule);
        if (secondRule !== NONE) this.assertRuleId(secondRule);

        const sourceText = prefix + removed;
        const posId = this.rulePosId(firstRule);
        const rootKeyIndex = this.findRootKey(route, posId, sourceText);
        if (rootKeyIndex === null) continue;

        const intermediate = this.applyRule(sourceText, firstRule);
        const generated = secondRule === NONE ? intermediate : this.applyRule(intermediate, secondRule);
        if (generated !== surface) continue;

        const rootKeyAt = this.header.rootKeyOffset + rootKeyIndex * ROOT_KEY_BYTES;
        const firstRecord = this.u32(rootKeyAt + 4);
        const recordCount = this.u32(rootKeyAt + 8);
        for (let recordOffset = 0; recordOffset < recordCount; recordOffset++) {
          const recordAt = this.header.rootRecordOffset + (firstRecord + recordOffset) * ROOT_RECORD_BYTES;
          const rootGroup = this.u32(recordAt);
          const rootSeq = this.u32(this.header.rootGroupOffset + rootGroup * ROOT_GROUP_BYTES);
          // Secondary closure is only reachable through a materialized first
          // stage. The build suppresses that first stage when its text is
          // already any lexical form on the root (the check is intentionally
          // route-independent, matching getAllReadings()).
          if (secondRule !== NONE && this.rootHasForm(rootGroup, intermediate)) continue;
          if (this.rootHasForm(rootGroup, surface)) continue;
          if (this.tombstones.has(this.tombstoneKey(route, surface, rootSeq, firstRule, secondRule))) continue;

          const sourceForm = this.string(this.u32(recordAt + 4));
          const sourceReading = this.string(this.u32(recordAt + 8));
          const firstProperty = this.ruleProperty(firstRule);
          const secondProperty = secondRule === NONE ? null : this.ruleProperty(secondRule);
          const path = secondProperty === null
            ? [firstProperty] as const
            : [firstProperty, secondProperty] as const;
          const formIntermediate = this.applyRule(sourceForm, firstRule);
          const readingIntermediate = this.applyRule(sourceReading, firstRule);
          const candidate: MorphologyCandidate = {
            route,
            surface,
            rootSeq,
            sourceText,
            sourceForm,
            sourceReading,
            form: secondRule === NONE ? formIntermediate : this.applyRule(formIntermediate, secondRule),
            reading: secondRule === NONE ? readingIntermediate : this.applyRule(readingIntermediate, secondRule),
            intermediate: secondRule === NONE ? null : intermediate,
            ruleIds: secondRule === NONE ? [firstRule] : [firstRule, secondRule],
            path,
            ord: this.u8(recordAt + 12),
            common: this.u8(recordAt + 13) === 0xff ? null : this.u8(recordAt + 13),
            compatibility: 'rule'
          };
          const key = canonicalCandidateKey(candidate);
          if (!seen.has(key)) {
            seen.add(key);
            candidates.push(candidate);
          }
        }
      }
    }

    const patchBucket = this.patchesBySurface.get(`${routeCode(route)}\u0000${surface}`);
    if (patchBucket) {
      for (let relative = 0; relative < patchBucket[1]; relative++) {
        const at = this.header.patchOffset + (patchBucket[0] + relative) * PATCH_BYTES;
        const firstRule = this.u32(at + 24);
        const secondRule = this.u32(at + 28);
        this.assertRuleId(firstRule);
        if (secondRule !== NONE) this.assertRuleId(secondRule);
        const firstProperty = this.ruleProperty(firstRule);
        const secondProperty = secondRule === NONE ? null : this.ruleProperty(secondRule);
        const path = secondProperty === null
          ? [firstProperty] as const
          : [firstProperty, secondProperty] as const;
        const commonByte = this.u8(at + 37);
        const candidate: MorphologyCandidate = {
          route,
          surface,
          rootSeq: this.u32(at),
          sourceText: this.string(this.u32(at + 4)),
          sourceForm: this.string(this.u32(at + 8)),
          sourceReading: this.string(this.u32(at + 12)),
          form: this.string(this.u32(at + 16)),
          reading: this.string(this.u32(at + 20)),
          intermediate: this.u32(at + 32) === NONE ? null : this.string(this.u32(at + 32)),
          ruleIds: secondRule === NONE ? [firstRule] : [firstRule, secondRule],
          path,
          ord: this.u8(at + 36),
          common: commonByte === 0xff ? null : commonByte,
          compatibility: 'manual'
        };
        const key = canonicalCandidateKey(candidate);
        if (!seen.has(key)) {
          seen.add(key);
          candidates.push(candidate);
        }
      }
    }

    candidates.sort((left, right) => compareCodeUnits(canonicalCandidateKey(left), canonicalCandidateKey(right)));
    return candidates;
  }

  position(index: number): string {
    const value = this.positions[index];
    if (value === undefined) {
      throw new MorphologyFormatError(`Missing morphology position ${index}`);
    }
    return value;
  }

  private readHeader(): Header {
    if (this.bytes.byteLength < HEADER_BYTES || !hasMagic(this.bytes)) {
      throw new MorphologyFormatError('Invalid morphology magic or truncated header');
    }
    const version = this.u16(8);
    const headerBytes = this.u16(10);
    const byteLength = this.u32(12);
    if (version !== VERSION) throw new MorphologyFormatError(`Unsupported morphology version ${version}`);
    if (headerBytes !== HEADER_BYTES) throw new MorphologyFormatError(`Invalid morphology header size ${headerBytes}`);
    if (byteLength !== this.bytes.byteLength) {
      throw new MorphologyFormatError(`Morphology header declares ${byteLength} bytes, received ${this.bytes.byteLength}`);
    }
    return {
      byteLength,
      positions: this.u32(16),
      rules: this.u32(20),
      suffixes: this.u32(24),
      templates: this.u32(28),
      rootKeys: this.u32(32),
      rootRecords: this.u32(36),
      rootHashSlots: this.u32(40),
      rootGroups: this.u32(44),
      rootForms: this.u32(48),
      patchBuckets: this.u32(52),
      patches: this.u32(56),
      strings: this.u32(60),
      stringCodeUnits: this.u32(64),
      posOffset: this.u32(68),
      ruleOffset: this.u32(72),
      suffixOffset: this.u32(76),
      templateOffset: this.u32(80),
      rootKeyOffset: this.u32(84),
      rootRecordOffset: this.u32(88),
      rootHashOffset: this.u32(92),
      rootGroupOffset: this.u32(96),
      rootFormOffset: this.u32(100),
      patchBucketOffset: this.u32(104),
      patchOffset: this.u32(108),
      stringDirOffset: this.u32(112),
      stringPoolOffset: this.u32(116),
      tombstones: this.u32(120),
      tombstoneOffset: this.u32(124)
    };
  }

  private validateLayout(): void {
    const h = this.header;
    if (h.rootHashSlots < 2 || (h.rootHashSlots & (h.rootHashSlots - 1)) !== 0) {
      throw new MorphologyFormatError('Root hash table size must be a power of two');
    }
    let expected = HEADER_BYTES;
    const expectTable = (actual: number, count: number, stride: number, label: string): void => {
      if (actual !== expected) throw new MorphologyFormatError(`${label} has non-canonical offset ${actual}; expected ${expected}`);
      expected = checkedEnd(actual, count, stride, h.byteLength, label);
    };
    expectTable(h.posOffset, h.positions, POS_BYTES, 'POS table');
    expectTable(h.ruleOffset, h.rules, RULE_BYTES, 'rule table');
    expectTable(h.suffixOffset, h.suffixes, SUFFIX_BYTES, 'suffix table');
    expectTable(h.templateOffset, h.templates, TEMPLATE_BYTES, 'template table');
    expectTable(h.rootKeyOffset, h.rootKeys, ROOT_KEY_BYTES, 'root-key table');
    expectTable(h.rootRecordOffset, h.rootRecords, ROOT_RECORD_BYTES, 'root-record table');
    expectTable(h.rootHashOffset, h.rootHashSlots, 4, 'root-hash table');
    expectTable(h.rootGroupOffset, h.rootGroups, ROOT_GROUP_BYTES, 'root-group table');
    expectTable(h.rootFormOffset, h.rootForms, ROOT_FORM_BYTES, 'root-form table');
    expectTable(h.patchBucketOffset, h.patchBuckets, PATCH_BUCKET_BYTES, 'patch-bucket table');
    expectTable(h.patchOffset, h.patches, PATCH_BYTES, 'patch table');
    expectTable(h.tombstoneOffset, h.tombstones, TOMBSTONE_BYTES, 'tombstone table');
    expectTable(h.stringDirOffset, h.strings + 1, 4, 'string directory');
    expected = align(expected, 2);
    if (h.stringPoolOffset !== expected) throw new MorphologyFormatError('String pool has a non-canonical offset');
    expected = checkedEnd(h.stringPoolOffset, h.stringCodeUnits, 2, h.byteLength, 'string pool');
    if (align(expected, 4) !== h.byteLength) throw new MorphologyFormatError('Morphology section has trailing or missing bytes');
    for (let index = expected; index < h.byteLength; index++) {
      if (this.u8(index) !== 0) throw new MorphologyFormatError('Morphology trailing padding is not zero');
    }

    let previous = 0;
    for (let index = 0; index <= h.strings; index++) {
      const current = this.u32(h.stringDirOffset + index * 4);
      if (current < previous || current > h.stringCodeUnits) {
        throw new MorphologyFormatError('String directory is not monotonic');
      }
      previous = current;
    }
    if (previous !== h.stringCodeUnits) throw new MorphologyFormatError('String directory does not cover the pool');

    let rootHashEntries = 0;
    for (let slot = 0; slot < h.rootHashSlots; slot++) {
      const entry = this.u32(h.rootHashOffset + slot * 4);
      if (entry > h.rootKeys) throw new MorphologyFormatError('Root hash slot references a missing key');
      if (entry !== 0) rootHashEntries++;
    }
    if (rootHashEntries !== h.rootKeys) throw new MorphologyFormatError('Root hash does not contain every root key exactly once');

    let nextRootRecord = 0;
    for (let index = 0; index < h.rootKeys; index++) {
      const at = h.rootKeyOffset + index * ROOT_KEY_BYTES;
      this.assertStringId(this.u32(at));
      const first = this.u32(at + 4);
      const count = this.u32(at + 8);
      if (first !== nextRootRecord || count === 0 || first + count > h.rootRecords) {
        throw new MorphologyFormatError('Root keys do not canonically cover root records');
      }
      nextRootRecord += count;
      if (this.u16(at + 12) >= h.positions) throw new MorphologyFormatError('Root key references a missing POS');
      routeFromCode(this.u8(at + 14));
    }
    if (nextRootRecord !== h.rootRecords) throw new MorphologyFormatError('Root records are not fully covered');

    for (let index = 0; index < h.rootRecords; index++) {
      const at = h.rootRecordOffset + index * ROOT_RECORD_BYTES;
      if (this.u32(at) >= h.rootGroups) throw new MorphologyFormatError('Root record references a missing root group');
      this.assertStringId(this.u32(at + 4));
      this.assertStringId(this.u32(at + 8));
    }

    let nextRootForm = 0;
    for (let index = 0; index < h.rootGroups; index++) {
      const at = h.rootGroupOffset + index * ROOT_GROUP_BYTES;
      if (this.u32(at) === 0) throw new MorphologyFormatError('Root sequence must be non-zero');
      const first = this.u32(at + 4);
      const count = this.u32(at + 8);
      if (first !== nextRootForm || first + count > h.rootForms) {
        throw new MorphologyFormatError('Root groups do not canonically cover root forms');
      }
      nextRootForm += count;
    }
    if (nextRootForm !== h.rootForms) throw new MorphologyFormatError('Root forms are not fully covered');
    for (let index = 0; index < h.rootForms; index++) {
      this.assertStringId(this.u32(h.rootFormOffset + index * ROOT_FORM_BYTES));
    }

    for (let index = 0; index < h.rules; index++) {
      const at = h.ruleOffset + index * RULE_BYTES;
      if (this.u16(at) >= h.positions) throw new MorphologyFormatError('Rule references a missing POS');
      triFromCode(this.u8(at + 3) & 3);
      triFromCode((this.u8(at + 3) >>> 2) & 3);
      this.assertStringId(this.u32(at + 8));
      this.assertStringId(this.u32(at + 12));
      this.assertStringId(this.u32(at + 16));
    }
    for (let index = 0; index < h.templates; index++) {
      const at = h.templateOffset + index * TEMPLATE_BYTES;
      this.assertStringId(this.u32(at));
      this.assertRuleId(this.u32(at + 4));
      const second = this.u32(at + 8);
      if (second !== NONE) this.assertRuleId(second);
    }
  }

  private findRootKey(route: MorphologyRoute, posId: number, sourceText: string): number | null {
    const hash = hashRootKey(route, posId, sourceText);
    const mask = this.header.rootHashSlots - 1;
    let slot = hash & mask;
    for (let probes = 0; probes < this.header.rootHashSlots; probes++) {
      const entry = this.u32(this.header.rootHashOffset + slot * 4);
      if (entry === 0) return null;
      const index = entry - 1;
      const at = this.header.rootKeyOffset + index * ROOT_KEY_BYTES;
      if (
        this.u16(at + 12) === posId
        && this.u8(at + 14) === routeCode(route)
        && this.stringEquals(this.u32(at), sourceText)
      ) return index;
      slot = (slot + 1) & mask;
    }
    throw new MorphologyFormatError('Root hash probe exhausted without an empty slot');
  }

  private rootHasForm(rootGroup: number, surface: string): boolean {
    const at = this.header.rootGroupOffset + rootGroup * ROOT_GROUP_BYTES;
    const first = this.u32(at + 4);
    const count = this.u32(at + 8);
    for (let index = 0; index < count; index++) {
      const stringId = this.u32(this.header.rootFormOffset + (first + index) * ROOT_FORM_BYTES);
      if (this.stringEquals(stringId, surface)) return true;
    }
    return false;
  }

  private applyRule(word: string, ruleId: number): string {
    const at = this.header.ruleOffset + ruleId * RULE_BYTES;
    const stem = this.u8(at + 5);
    const kana = /^[ァ-ヺヽヾーぁ-ゔゝゞー]+$/.test(word.slice(Math.max(0, word.length - 2)));
    const euphr = this.string(this.u32(at + 12));
    const euphk = this.string(this.u32(at + 16));
    const euphony = kana ? euphr : euphk;
    const extraStem = euphony.length > 0 ? 1 : 0;
    return word.slice(0, word.length - stem - extraStem) + euphony + this.string(this.u32(at + 8));
  }

  private rulePosId(ruleId: number): number {
    return this.u16(this.header.ruleOffset + ruleId * RULE_BYTES);
  }

  private ruleProperty(ruleId: number): MorphologyProperty {
    const at = this.header.ruleOffset + ruleId * RULE_BYTES;
    const flags = this.u8(at + 3);
    return {
      pos: this.positions[this.u16(at)]!,
      type: this.u8(at + 2),
      negative: triFromCode(flags & 3),
      formal: triFromCode((flags >>> 2) & 3),
      ordinal: this.u8(at + 4)
    };
  }

  private tombstoneKey(
    route: MorphologyRoute,
    surface: string,
    rootSeq: number,
    firstRule: number,
    secondRule: number
  ): string {
    return `${routeCode(route)}\u0000${surface}\u0000${rootSeq}\u0000${firstRule}\u0000${secondRule}`;
  }

  private string(id: number): string {
    this.assertStringId(id);
    const cached = this.stringCache[id];
    if (cached !== undefined) return cached;
    const start = this.u32(this.header.stringDirOffset + id * 4);
    const end = this.u32(this.header.stringDirOffset + (id + 1) * 4);
    let value = '';
    const chunk: number[] = [];
    for (let offset = start; offset < end; offset++) {
      chunk.push(this.u16(this.header.stringPoolOffset + offset * 2));
      if (chunk.length === 4096) {
        value += String.fromCharCode(...chunk);
        chunk.length = 0;
      }
    }
    if (chunk.length > 0) value += String.fromCharCode(...chunk);
    this.stringCache[id] = value;
    return value;
  }

  private stringEquals(id: number, value: string): boolean {
    this.assertStringId(id);
    const start = this.u32(this.header.stringDirOffset + id * 4);
    const end = this.u32(this.header.stringDirOffset + (id + 1) * 4);
    if (end - start !== value.length) return false;
    for (let index = 0; index < value.length; index++) {
      if (this.u16(this.header.stringPoolOffset + (start + index) * 2) !== value.charCodeAt(index)) return false;
    }
    return true;
  }

  private assertStringId(id: number): void {
    if (id >= this.header.strings) throw new MorphologyFormatError(`Invalid string ID ${id}`);
  }

  private assertRuleId(id: number): void {
    if (id >= this.header.rules) throw new MorphologyFormatError(`Invalid rule ID ${id}`);
  }

  private u8(offset: number): number {
    return this.view.getUint8(offset);
  }

  private u16(offset: number): number {
    return this.view.getUint16(offset, true);
  }

  private u32(offset: number): number {
    return this.view.getUint32(offset, true);
  }
}

export function openMorphology(input: ArrayBuffer | Uint8Array): MorphologyReader {
  return new MorphologyReader(input);
}
