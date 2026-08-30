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

export type MorphologyRoute = 'kana' | 'kanji';

export interface CompiledMorphologyRule {
  pos: string;
  type: number;
  negative: boolean | null;
  formal: boolean | null;
  ordinal: number;
  stem: number;
  okuri: string;
  euphr: string;
  euphk: string;
}

export interface CompiledMorphologyTemplate {
  suffix: string;
  removed: string;
  firstRule: number;
  secondRule: number | null;
}

export interface CompiledMorphologyRootRecord {
  rootGroup: number;
  sourceForm: string;
  sourceReading: string;
  ord: number;
  common: number | null;
}

export interface CompiledMorphologyRootKey {
  route: MorphologyRoute;
  pos: string;
  sourceText: string;
  records: CompiledMorphologyRootRecord[];
}

export interface CompiledMorphologyRootGroup {
  seq: number;
  forms: string[];
}

export interface CompiledMorphologyPatch {
  route: MorphologyRoute;
  surface: string;
  rootSeq: number;
  sourceText: string;
  sourceForm: string;
  sourceReading: string;
  form: string;
  reading: string;
  firstRule: number;
  secondRule: number | null;
  intermediate: string | null;
  ord: number;
  common: number | null;
}

export interface CompiledMorphologyTombstone {
  route: MorphologyRoute;
  surface: string;
  rootSeq: number;
  firstRule: number;
  secondRule: number | null;
}

export interface CompiledMorphologyArtifact {
  positions: string[];
  rules: CompiledMorphologyRule[];
  templates: CompiledMorphologyTemplate[];
  rootKeys: CompiledMorphologyRootKey[];
  rootGroups: CompiledMorphologyRootGroup[];
  patches: CompiledMorphologyPatch[];
  tombstones: CompiledMorphologyTombstone[];
}

function align(value: number, alignment: number): number {
  return Math.ceil(value / alignment) * alignment;
}

function routeCode(route: MorphologyRoute): number {
  return route === 'kana' ? 0 : 1;
}

function triCode(value: boolean | null): number {
  return value === null ? 2 : value ? 1 : 0;
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

function assertInteger(value: number, min: number, max: number, label: string): void {
  if (!Number.isSafeInteger(value) || value < min || value > max) {
    throw new Error(`${label} must be an integer in [${min}, ${max}]`);
  }
}

function assertSortedUnique<T>(values: readonly T[], key: (value: T) => string, label: string): void {
  let previous: string | null = null;
  for (const value of values) {
    const current = key(value);
    if (previous !== null && current <= previous) {
      throw new Error(`${label} must be strictly sorted; saw ${JSON.stringify(current)} after ${JSON.stringify(previous)}`);
    }
    previous = current;
  }
}

function pushStrings(target: Set<string>, values: readonly (string | null)[]): void {
  for (const value of values) if (value !== null) target.add(value);
}

function writeMagic(bytes: Uint8Array): void {
  for (let index = 0; index < MAGIC.length; index++) bytes[index] = MAGIC.charCodeAt(index);
}

/**
 * Encode the morphology section consumed by `@ichiran/core`.
 *
 * The compiler supplies already-canonical arrays. Requiring that order here is
 * deliberate: unstable database iteration becomes a loud build error instead
 * of being hidden by a second, subtly different canonicalizer.
 */
export function encodeMorphologyArtifact(source: CompiledMorphologyArtifact): Uint8Array {
  assertSortedUnique(source.positions, value => value, 'positions');
  const posIds = new Map(source.positions.map((pos, index) => [pos, index]));

  assertSortedUnique(source.rules, rule => JSON.stringify(rule), 'rules');
  assertSortedUnique(
    source.templates,
    template => `${template.suffix}\u0000${template.removed}\u0000${template.firstRule.toString().padStart(8, '0')}\u0000${String(template.secondRule ?? NONE).padStart(10, '0')}`,
    'templates'
  );
  assertSortedUnique(
    source.rootKeys,
    key => `${routeCode(key.route)}\u0000${posIds.get(key.pos)?.toString().padStart(5, '0')}\u0000${key.sourceText}`,
    'root keys'
  );
  assertSortedUnique(source.rootGroups, group => group.seq.toString().padStart(10, '0'), 'root groups');
  assertSortedUnique(
    source.patches,
    patch => `${routeCode(patch.route)}\u0000${patch.surface}\u0000${patch.rootSeq.toString().padStart(10, '0')}\u0000${patch.sourceText}\u0000${patch.firstRule.toString().padStart(8, '0')}\u0000${String(patch.secondRule ?? NONE).padStart(10, '0')}`,
    'patches'
  );
  assertSortedUnique(
    source.tombstones,
    tombstone => `${routeCode(tombstone.route)}\u0000${tombstone.surface}\u0000${tombstone.rootSeq.toString().padStart(10, '0')}\u0000${tombstone.firstRule.toString().padStart(8, '0')}\u0000${String(tombstone.secondRule ?? NONE).padStart(10, '0')}`,
    'tombstones'
  );

  const strings = new Set<string>();
  pushStrings(strings, source.positions);
  for (const rule of source.rules) pushStrings(strings, [rule.okuri, rule.euphr, rule.euphk]);
  for (const template of source.templates) pushStrings(strings, [template.suffix, template.removed]);
  for (const key of source.rootKeys) {
    pushStrings(strings, [key.sourceText]);
    for (const record of key.records) pushStrings(strings, [record.sourceForm, record.sourceReading]);
  }
  for (const group of source.rootGroups) pushStrings(strings, group.forms);
  for (const patch of source.patches) {
    pushStrings(strings, [
      patch.surface,
      patch.sourceText,
      patch.sourceForm,
      patch.sourceReading,
      patch.form,
      patch.reading,
      patch.intermediate
    ]);
  }
  for (const tombstone of source.tombstones) pushStrings(strings, [tombstone.surface]);

  const stringList = [...strings].sort();
  const stringIds = new Map(stringList.map((value, index) => [value, index]));
  const stringOffsets = new Uint32Array(stringList.length + 1);
  let stringCodeUnits = 0;
  for (let index = 0; index < stringList.length; index++) {
    stringOffsets[index] = stringCodeUnits;
    stringCodeUnits += stringList[index]!.length;
  }
  stringOffsets[stringList.length] = stringCodeUnits;

  const suffixBuckets: Array<{ suffix: string; first: number; count: number }> = [];
  for (let index = 0; index < source.templates.length;) {
    const suffix = source.templates[index]!.suffix;
    let end = index + 1;
    while (end < source.templates.length && source.templates[end]!.suffix === suffix) end++;
    suffixBuckets.push({ suffix, first: index, count: end - index });
    index = end;
  }

  let rootRecordCount = 0;
  for (const key of source.rootKeys) rootRecordCount += key.records.length;
  let rootFormCount = 0;
  for (const group of source.rootGroups) rootFormCount += group.forms.length;

  const patchBuckets: Array<{ route: MorphologyRoute; surface: string; first: number; count: number }> = [];
  for (let index = 0; index < source.patches.length;) {
    const { route, surface } = source.patches[index]!;
    let end = index + 1;
    while (
      end < source.patches.length
      && source.patches[end]!.route === route
      && source.patches[end]!.surface === surface
    ) end++;
    patchBuckets.push({ route, surface, first: index, count: end - index });
    index = end;
  }

  let rootHashSlots = 2;
  while (rootHashSlots * 7 < source.rootKeys.length * 10) rootHashSlots *= 2;

  let offset = HEADER_BYTES;
  const posOffset = offset; offset += source.positions.length * POS_BYTES;
  const ruleOffset = offset; offset += source.rules.length * RULE_BYTES;
  const suffixOffset = offset; offset += suffixBuckets.length * SUFFIX_BYTES;
  const templateOffset = offset; offset += source.templates.length * TEMPLATE_BYTES;
  const rootKeyOffset = offset; offset += source.rootKeys.length * ROOT_KEY_BYTES;
  const rootRecordOffset = offset; offset += rootRecordCount * ROOT_RECORD_BYTES;
  const rootHashOffset = offset; offset += rootHashSlots * 4;
  const rootGroupOffset = offset; offset += source.rootGroups.length * ROOT_GROUP_BYTES;
  const rootFormOffset = offset; offset += rootFormCount * ROOT_FORM_BYTES;
  const patchBucketOffset = offset; offset += patchBuckets.length * PATCH_BUCKET_BYTES;
  const patchOffset = offset; offset += source.patches.length * PATCH_BYTES;
  const tombstoneOffset = offset; offset += source.tombstones.length * TOMBSTONE_BYTES;
  const stringDirOffset = offset; offset += stringOffsets.byteLength;
  const stringPoolOffset = align(offset, 2); offset = stringPoolOffset + stringCodeUnits * 2;
  const totalBytes = align(offset, 4);

  const bytes = new Uint8Array(totalBytes);
  const view = new DataView(bytes.buffer);
  writeMagic(bytes);
  view.setUint16(8, VERSION, true);
  view.setUint16(10, HEADER_BYTES, true);
  view.setUint32(12, totalBytes, true);
  view.setUint32(16, source.positions.length, true);
  view.setUint32(20, source.rules.length, true);
  view.setUint32(24, suffixBuckets.length, true);
  view.setUint32(28, source.templates.length, true);
  view.setUint32(32, source.rootKeys.length, true);
  view.setUint32(36, rootRecordCount, true);
  view.setUint32(40, rootHashSlots, true);
  view.setUint32(44, source.rootGroups.length, true);
  view.setUint32(48, rootFormCount, true);
  view.setUint32(52, patchBuckets.length, true);
  view.setUint32(56, source.patches.length, true);
  view.setUint32(60, stringList.length, true);
  view.setUint32(64, stringCodeUnits, true);
  view.setUint32(68, posOffset, true);
  view.setUint32(72, ruleOffset, true);
  view.setUint32(76, suffixOffset, true);
  view.setUint32(80, templateOffset, true);
  view.setUint32(84, rootKeyOffset, true);
  view.setUint32(88, rootRecordOffset, true);
  view.setUint32(92, rootHashOffset, true);
  view.setUint32(96, rootGroupOffset, true);
  view.setUint32(100, rootFormOffset, true);
  view.setUint32(104, patchBucketOffset, true);
  view.setUint32(108, patchOffset, true);
  view.setUint32(112, stringDirOffset, true);
  view.setUint32(116, stringPoolOffset, true);
  view.setUint32(120, source.tombstones.length, true);
  view.setUint32(124, tombstoneOffset, true);

  const stringId = (value: string): number => {
    const id = stringIds.get(value);
    if (id === undefined) throw new Error(`Missing string-table value ${JSON.stringify(value)}`);
    return id;
  };

  source.positions.forEach((pos, index) => {
    view.setUint32(posOffset + index * POS_BYTES, stringId(pos), true);
  });

  source.rules.forEach((rule, index) => {
    const at = ruleOffset + index * RULE_BYTES;
    const posId = posIds.get(rule.pos);
    if (posId === undefined) throw new Error(`Rule uses undeclared POS ${rule.pos}`);
    assertInteger(posId, 0, 0xffff, 'rule POS ID');
    assertInteger(rule.type, 0, 0xff, 'conjugation type');
    assertInteger(rule.ordinal, 0, 0xff, 'rule ordinal');
    assertInteger(rule.stem, 0, 0xff, 'rule stem');
    view.setUint16(at, posId, true);
    view.setUint8(at + 2, rule.type);
    view.setUint8(at + 3, triCode(rule.negative) | (triCode(rule.formal) << 2));
    view.setUint8(at + 4, rule.ordinal);
    view.setUint8(at + 5, rule.stem);
    view.setUint32(at + 8, stringId(rule.okuri), true);
    view.setUint32(at + 12, stringId(rule.euphr), true);
    view.setUint32(at + 16, stringId(rule.euphk), true);
  });

  suffixBuckets.forEach((bucket, index) => {
    const at = suffixOffset + index * SUFFIX_BYTES;
    view.setUint32(at, stringId(bucket.suffix), true);
    view.setUint32(at + 4, bucket.first, true);
    view.setUint32(at + 8, bucket.count, true);
  });

  source.templates.forEach((template, index) => {
    const at = templateOffset + index * TEMPLATE_BYTES;
    assertInteger(template.firstRule, 0, source.rules.length - 1, 'first rule');
    if (template.secondRule !== null) assertInteger(template.secondRule, 0, source.rules.length - 1, 'second rule');
    view.setUint32(at, stringId(template.removed), true);
    view.setUint32(at + 4, template.firstRule, true);
    view.setUint32(at + 8, template.secondRule ?? NONE, true);
  });

  let nextRootRecord = 0;
  source.rootKeys.forEach((key, index) => {
    const at = rootKeyOffset + index * ROOT_KEY_BYTES;
    const posId = posIds.get(key.pos);
    if (posId === undefined) throw new Error(`Root key uses undeclared POS ${key.pos}`);
    view.setUint32(at, stringId(key.sourceText), true);
    view.setUint32(at + 4, nextRootRecord, true);
    view.setUint32(at + 8, key.records.length, true);
    view.setUint16(at + 12, posId, true);
    view.setUint8(at + 14, routeCode(key.route));
    for (const record of key.records) {
      const recordAt = rootRecordOffset + nextRootRecord * ROOT_RECORD_BYTES;
      assertInteger(record.rootGroup, 0, source.rootGroups.length - 1, 'root group');
      assertInteger(record.ord, 0, 0xff, 'root form ord');
      if (record.common !== null) assertInteger(record.common, 0, 0xfe, 'root form common');
      view.setUint32(recordAt, record.rootGroup, true);
      view.setUint32(recordAt + 4, stringId(record.sourceForm), true);
      view.setUint32(recordAt + 8, stringId(record.sourceReading), true);
      view.setUint8(recordAt + 12, record.ord);
      view.setUint8(recordAt + 13, record.common ?? 0xff);
      nextRootRecord++;
    }
  });

  const rootHash = new Uint32Array(bytes.buffer, rootHashOffset, rootHashSlots);
  source.rootKeys.forEach((key, index) => {
    const posId = posIds.get(key.pos)!;
    const hash = hashRootKey(key.route, posId, key.sourceText);
    let slot = hash & (rootHashSlots - 1);
    while (rootHash[slot] !== 0) slot = (slot + 1) & (rootHashSlots - 1);
    rootHash[slot] = index + 1;
  });

  let nextRootForm = 0;
  source.rootGroups.forEach((group, index) => {
    const at = rootGroupOffset + index * ROOT_GROUP_BYTES;
    assertInteger(group.seq, 1, 0xffff_ffff, 'root seq');
    view.setUint32(at, group.seq, true);
    view.setUint32(at + 4, nextRootForm, true);
    view.setUint32(at + 8, group.forms.length, true);
    for (const form of group.forms) {
      view.setUint32(rootFormOffset + nextRootForm * ROOT_FORM_BYTES, stringId(form), true);
      nextRootForm++;
    }
  });

  patchBuckets.forEach((bucket, index) => {
    const at = patchBucketOffset + index * PATCH_BUCKET_BYTES;
    view.setUint32(at, stringId(bucket.surface), true);
    view.setUint32(at + 4, bucket.first, true);
    view.setUint16(at + 8, bucket.count, true);
    view.setUint8(at + 10, routeCode(bucket.route));
  });

  source.patches.forEach((patch, index) => {
    const at = patchOffset + index * PATCH_BYTES;
    assertInteger(patch.rootSeq, 1, 0xffff_ffff, 'patch root seq');
    assertInteger(patch.ord, 0, 0xff, 'patch source ord');
    if (patch.common !== null) assertInteger(patch.common, 0, 0xfe, 'patch source common');
    view.setUint32(at, patch.rootSeq, true);
    view.setUint32(at + 4, stringId(patch.sourceText), true);
    view.setUint32(at + 8, stringId(patch.sourceForm), true);
    view.setUint32(at + 12, stringId(patch.sourceReading), true);
    view.setUint32(at + 16, stringId(patch.form), true);
    view.setUint32(at + 20, stringId(patch.reading), true);
    view.setUint32(at + 24, patch.firstRule, true);
    view.setUint32(at + 28, patch.secondRule ?? NONE, true);
    view.setUint32(at + 32, patch.intermediate === null ? NONE : stringId(patch.intermediate), true);
    view.setUint8(at + 36, patch.ord);
    view.setUint8(at + 37, patch.common ?? 0xff);
  });

  source.tombstones.forEach((tombstone, index) => {
    const at = tombstoneOffset + index * TOMBSTONE_BYTES;
    view.setUint32(at, tombstone.rootSeq, true);
    view.setUint32(at + 4, stringId(tombstone.surface), true);
    view.setUint32(at + 8, tombstone.firstRule, true);
    view.setUint32(at + 12, tombstone.secondRule ?? NONE, true);
    view.setUint8(at + 16, routeCode(tombstone.route));
  });

  for (let index = 0; index < stringOffsets.length; index++) {
    view.setUint32(stringDirOffset + index * 4, stringOffsets[index]!, true);
  }
  let poolAt = stringPoolOffset;
  for (const value of stringList) {
    for (let index = 0; index < value.length; index++) {
      view.setUint16(poolAt, value.charCodeAt(index), true);
      poolAt += 2;
    }
  }

  return bytes;
}
