import { gzipSync } from 'node:zlib';
import type {
  AnalyzerSupportGeneratedMemberSource,
  AnalyzerSupportGeneratedSource,
  AnalyzerSupportHintSource,
  AnalyzerSupportSplitPartSource,
  AnalyzerSupportSplitSource
} from './analyzer-support.js';

export const ANALYZER_ANNOTATIONS_MAGIC = 'IANAN001';
export const ANALYZER_ANNOTATIONS_FORMAT_VERSION = 4;
export const ANALYZER_ANNOTATIONS_HEADER_BYTES = 184;
export const ANALYZER_ANNOTATIONS_BLOCK_BYTES = 24;
export const ANALYZER_ANNOTATIONS_SECTION_ID = 5;
export const ANALYZER_GENERATED_BLOCK_BYTES = 24;
export const ANALYZER_GENERATED_ROOT_BYTES = 8;
/** u32 semantic key + property order, u8 count fact, u24 physical identity, u16 conj_prop. */
export const ANALYZER_GENERATED_RECORD_BYTES = 10;
/** u22 semantic/direct locator + u6 global precedence level + four reserved bits. */
export const ANALYZER_LOOKUP_ORDER_RECORD_BYTES = 4;
export const ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES = 16;
export const ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES = 8;
export const ANALYZER_GENERATED_BLOCK_TARGET_BYTES = 256 * 1024;

const ALIGNMENT = 8;
const UTF8 = new TextEncoder();
const GENERATED_ALIAS_BITS = 11;
const GENERATED_KEY_BITS = GENERATED_ALIAS_BITS * 2;
const GENERATED_KEY_MASK = (1 << GENERATED_KEY_BITS) - 1;
const GENERATED_ALIAS_MAX = (1 << GENERATED_ALIAS_BITS) - 2;
const GENERATED_PROP_ORD_MAX = (1 << (32 - GENERATED_KEY_BITS)) - 1;
const GENERATED_PHYSICAL_GROUP_MAX = (1 << 18) - 1;
const GENERATED_MEMBER_ORD_MAX = 6;
const GENERATED_VIA_MEMBER_NONE = 7;
const GENERATED_POS_ID_MAX = (1 << 5) - 1;
const GENERATED_PROPERTY_TYPE_MAX = (1 << 6) - 1;
const GENERATED_PROPERTY_NONE = 0xffff;

const CRC32_POLYNOMIAL = 0xedb8_8320;
const CRC32_TABLE = new Uint32Array(256);
for (let value = 0; value < CRC32_TABLE.length; value++) {
  let checksum = value;
  for (let bit = 0; bit < 8; bit++) {
    checksum = (checksum & 1) === 1 ? CRC32_POLYNOMIAL ^ (checksum >>> 1) : checksum >>> 1;
  }
  CRC32_TABLE[value] = checksum >>> 0;
}

type EncodedPart =
  | 1
  | 2
  | [
      0, number, number, string, string | null, number, number | null, string, number,
      Array<[number, number, string, number, boolean | null, boolean | null]> | null
    ];

type EncodedSplit = [
  number, string, number, EncodedPart[], number, number, string, number[]
];

type EncodedHint = [number, string, string, string];

interface EncodedAnnotationBlock {
  seq: number;
  splits: number;
  hints: number;
  uncompressed: Uint8Array;
  compressed: Uint8Array;
}

interface EncodedGeneratedRecord {
  key: number;
  fact: number;
  physical: number;
  property: number;
}

interface EncodedLookupOrder {
  key: number;
  rank: number;
}

interface EncodedGeneratedRoot {
  seq: number;
  records: EncodedGeneratedRecord[];
  orders: EncodedLookupOrder[];
}

interface EncodedGeneratedBlock {
  firstRootSeq: number;
  roots: EncodedGeneratedRoot[];
  recordCount: number;
  orderCount: number;
  uncompressed: Uint8Array;
  compressed: Uint8Array;
}

interface EncodedLookupOrderException {
  route: number;
  surface: Uint8Array;
  orders: Array<{ rootSeq: number; key: number; rank: number }>;
}

export interface AnalyzerAnnotationsBuild {
  readonly bytes: Uint8Array;
  readonly stats: {
    readonly blocks: number;
    readonly splits: number;
    readonly hints: number;
    readonly generatedBlocks: number;
    readonly generatedRoots: number;
    readonly generatedRecords: number;
    readonly lookupOrderRecords: number;
    readonly lookupOrderRoots: number;
    readonly lookupOrderBytes: number;
    readonly lookupOrderSourceRows: number;
    readonly lookupOrderSurfaces: number;
    readonly lookupOrderClasses: number;
    readonly lookupOrderEquivalenceClasses: number;
    readonly lookupOrderEdges: number;
    readonly lookupOrderMaxRank: number;
    readonly lookupOrderComponents: number;
    readonly lookupOrderCyclicComponents: number;
    readonly lookupOrderExceptionSurfaces: number;
    readonly lookupOrderExceptionClasses: number;
    readonly lookupOrderExceptionLocators: number;
    readonly lookupOrderExceptionBytes: number;
    readonly generatedPhysicalGroups: number;
    readonly generatedFactPairs: number;
    readonly indexBytes: number;
    readonly uncompressedBytes: number;
    readonly compressedBytes: number;
    readonly annotationUncompressedBytes: number;
    readonly annotationCompressedBytes: number;
    readonly generatedUncompressedBytes: number;
    readonly generatedCompressedBytes: number;
    readonly totalBytes: number;
    readonly largestUncompressedBlock: number;
    readonly largestGeneratedBlock: number;
    readonly largestGeneratedCompressedBlock: number;
  };
}

function crc32(bytes: Uint8Array): number {
  let checksum = 0xffff_ffff;
  for (const byte of bytes) checksum = CRC32_TABLE[(checksum ^ byte) & 0xff]! ^ (checksum >>> 8);
  return (checksum ^ 0xffff_ffff) >>> 0;
}

function align(value: number): number {
  return Math.ceil(value / ALIGNMENT) * ALIGNMENT;
}

function routeCode(route: 'kana' | 'kanji'): number {
  return route === 'kana' ? 0 : 1;
}

function checkedInteger(value: number, minimum: number, maximum: number, label: string): number {
  if (!Number.isSafeInteger(value) || value < minimum || value > maximum) {
    throw new RangeError(`${label} must be an integer in [${minimum}, ${maximum}]`);
  }
  return value;
}

function part(part: AnalyzerSupportSplitPartSource): EncodedPart {
  if (part === ':score') return 1;
  if (part === ':pscore') return 2;
  return [
    0,
    routeCode(part.route),
    part.seq,
    part.text,
    part.best,
    checkedInteger(part.ord, 0, 0xffff, 'Split-part ordinal'),
    part.common,
    part.commonTags,
    (part.conjugatable ? 1 : 0) | (part.nokanji ? 2 : 0),
    part.generated?.map(value => [
      value.from,
      value.via ? 1 : 0,
      value.pos,
      value.type,
      value.negative,
      value.formal
    ]) ?? null
  ];
}

function split(value: AnalyzerSupportSplitSource): EncodedSplit {
  return [
    routeCode(value.route),
    value.surface,
    value.kind === 'split' ? 0 : 1,
    value.parts.map(part),
    checkedInteger(value.score, -0x8000_0000, 0x7fff_ffff, 'Split score'),
    checkedInteger(value.primary, 0, 0xff, 'Split primary'),
    value.connector,
    [...value.root]
  ];
}

function hint(value: AnalyzerSupportHintSource): EncodedHint {
  return [routeCode(value.route), value.surface, value.reading, value.hint];
}

function canonicalJson(seq: number, splits: readonly AnalyzerSupportSplitSource[], hints: readonly AnalyzerSupportHintSource[]): string {
  const compareJson = (left: unknown, right: unknown): number => {
    const leftText = JSON.stringify(left);
    const rightText = JSON.stringify(right);
    return leftText < rightText ? -1 : leftText > rightText ? 1 : 0;
  };
  const encodedSplits = splits.map(split).sort(compareJson);
  const encodedHints = hints.map(hint).sort(compareJson);
  return JSON.stringify([ANALYZER_ANNOTATIONS_FORMAT_VERSION, seq, encodedSplits, encodedHints]);
}

function generatedKey(firstAlias: number, secondAlias: number | null): number {
  checkedInteger(firstAlias, 0, GENERATED_ALIAS_MAX, 'Generated first alias');
  if (secondAlias !== null) {
    checkedInteger(secondAlias, 0, GENERATED_ALIAS_MAX, 'Generated second alias');
  }
  const key = (firstAlias << GENERATED_ALIAS_BITS) | (secondAlias === null ? 0 : secondAlias + 1);
  if (key === GENERATED_KEY_MASK) {
    throw new RangeError('Generated key collides with the direct lookup-order sentinel');
  }
  return key;
}

function generatedProperty(
  property: AnalyzerSupportGeneratedMemberSource['property']
): number {
  checkedInteger(property.posId, 0, GENERATED_POS_ID_MAX, 'Generated property position');
  checkedInteger(property.type, 0, GENERATED_PROPERTY_TYPE_MAX, 'Generated property type');
  const tri = (value: boolean | null): number => value === null ? 2 : Number(value);
  return property.posId
    | (property.type << 5)
    | (tri(property.negative) << 11)
    | (tri(property.formal) << 13);
}

function compareBytes(left: Uint8Array, right: Uint8Array): number {
  const shared = Math.min(left.byteLength, right.byteLength);
  for (let index = 0; index < shared; index++) {
    const difference = left[index]! - right[index]!;
    if (difference !== 0) return difference;
  }
  return left.byteLength - right.byteLength;
}

function encodeLookupOrderExceptions(
  generated: AnalyzerSupportGeneratedSource
): {
  exceptions: EncodedLookupOrderException[];
  strings: Uint8Array;
  classes: number;
  locators: number;
  maxRank: number;
} {
  const exceptions = generated.lookupOrderExceptions.map(value => {
    checkedInteger(
      value.orders.length,
      1,
      0xffff,
      'Exception lookup-order locator count'
    );
    return {
      route: routeCode(value.route),
      surface: UTF8.encode(value.surface),
      orders: value.orders.map(order => {
        checkedInteger(order.rootSeq, 1, 0xffff_ffff, 'Exception lookup-order root seq');
        let key = GENERATED_KEY_MASK;
        if (order.firstAlias !== null) {
          if (order.firstAlias >= generated.aliasCount
            || (order.secondAlias !== null && order.secondAlias >= generated.aliasCount)) {
            throw new RangeError(
              `Exception lookup-order record for ${order.rootSeq} references an unknown alias`
            );
          }
          key = generatedKey(order.firstAlias, order.secondAlias);
        } else if (order.secondAlias !== null) {
          throw new RangeError('Direct exception lookup-order locator cannot have a second alias');
        }
        return {
          rootSeq: order.rootSeq,
          key,
          rank: checkedInteger(order.rank, 0, 0x3f, 'Exception lookup-order rank')
        };
      })
    };
  }).sort((left, right) => left.route - right.route || compareBytes(left.surface, right.surface));

  let classes = 0;
  let locators = 0;
  let maxRank = 0;
  let stringBytes = 0;
  const keys = new Set<string>();
  for (const exception of exceptions) {
    if (exception.surface.byteLength === 0 || exception.surface.byteLength > 0xffff) {
      throw new RangeError('Lookup-order exception surface must fit uint16 UTF-8 bytes');
    }
    const surfaceKey = `${exception.route}\u0000${Array.from(exception.surface).join(',')}`;
    if (keys.has(surfaceKey)) throw new RangeError('Duplicate lookup-order exception surface');
    keys.add(surfaceKey);
    stringBytes += exception.surface.byteLength;
    exception.orders.sort((left, right) => left.rootSeq - right.rootSeq || left.key - right.key);
    const locatorKeys = new Set<string>();
    const ranks = new Set<number>();
    for (const order of exception.orders) {
      const key = `${order.rootSeq}\u0000${order.key}`;
      if (locatorKeys.has(key)) throw new RangeError('Duplicate exception lookup-order locator');
      locatorKeys.add(key);
      ranks.add(order.rank);
      maxRank = Math.max(maxRank, order.rank);
    }
    const orderedRanks = [...ranks].sort((left, right) => left - right);
    if (
      exception.orders.length === 0
      || orderedRanks.length < 2
      || orderedRanks.some((rank, index) => rank !== index)
    ) throw new RangeError('Exception lookup-order ranks must be dense and contain multiple classes');
    classes += orderedRanks.length;
    locators += exception.orders.length;
  }
  if (
    classes !== generated.lookupOrderExceptionClasses
    || locators !== generated.lookupOrderExceptionLocators
  ) throw new RangeError('Lookup-order exception statistics disagree with source');
  const strings = new Uint8Array(stringBytes);
  let offset = 0;
  for (const exception of exceptions) {
    strings.set(exception.surface, offset);
    offset += exception.surface.byteLength;
  }
  return { exceptions, strings, classes, locators, maxRank };
}

function encodeGeneratedBlock(roots: EncodedGeneratedRoot[]): Uint8Array {
  const recordCount = roots.reduce((sum, root) => sum + root.records.length, 0);
  const orderCount = roots.reduce((sum, root) => sum + root.orders.length, 0);
  const recordsOffset = 12 + roots.length * 20;
  const ordersOffset = recordsOffset + recordCount * ANALYZER_GENERATED_RECORD_BYTES;
  const bytes = new Uint8Array(ordersOffset + orderCount * ANALYZER_LOOKUP_ORDER_RECORD_BYTES);
  const view = new DataView(bytes.buffer);
  view.setUint32(0, roots.length, true);
  view.setUint32(4, recordCount, true);
  view.setUint32(8, orderCount, true);
  let firstRecord = 0;
  let firstOrder = 0;
  for (let index = 0; index < roots.length; index++) {
    const root = roots[index]!;
    const at = 12 + index * 20;
    view.setUint32(at, root.seq, true);
    view.setUint32(at + 4, firstRecord, true);
    view.setUint32(at + 8, root.records.length, true);
    view.setUint32(at + 12, firstOrder, true);
    view.setUint32(at + 16, root.orders.length, true);
    for (let recordIndex = 0; recordIndex < root.records.length; recordIndex++) {
      const record = root.records[recordIndex]!;
      const recordAt = recordsOffset + (firstRecord + recordIndex) * ANALYZER_GENERATED_RECORD_BYTES;
      view.setUint32(recordAt, record.key, true);
      view.setUint8(recordAt + 4, record.fact);
      view.setUint8(recordAt + 5, record.physical & 0xff);
      view.setUint8(recordAt + 6, (record.physical >>> 8) & 0xff);
      view.setUint8(recordAt + 7, (record.physical >>> 16) & 0xff);
      view.setUint16(recordAt + 8, record.property, true);
    }
    for (let orderIndex = 0; orderIndex < root.orders.length; orderIndex++) {
      const order = root.orders[orderIndex]!;
      const orderAt = ordersOffset
        + (firstOrder + orderIndex) * ANALYZER_LOOKUP_ORDER_RECORD_BYTES;
      view.setUint32(orderAt, order.key | (order.rank << GENERATED_KEY_BITS), true);
    }
    firstRecord += root.records.length;
    firstOrder += root.orders.length;
  }
  if (firstOrder !== orderCount) {
    throw new RangeError('Lookup-order records are not covered');
  }
  return bytes;
}

function generatedBlocks(
  generated: AnalyzerSupportGeneratedSource
): {
  blocks: EncodedGeneratedBlock[];
  roots: Array<{ seq: number; block: number }>;
  facts: Array<readonly [number, number]>;
  orderRoots: number;
} {
  checkedInteger(generated.aliasCount, 0, GENERATED_ALIAS_MAX + 1, 'Generated alias count');
  const factKeys = [...new Set(generated.records.flatMap(record =>
    record.counts === null ? [] : [`${record.counts[0]}\u0000${record.counts[1]}`]))]
    .sort((left, right) => {
      const [leftKanji, leftKana] = left.split('\u0000').map(Number);
      const [rightKanji, rightKana] = right.split('\u0000').map(Number);
      return leftKanji! - rightKanji! || leftKana! - rightKana!;
    });
  if (factKeys.length > 0xff) throw new RangeError('Generated count facts exceed one-byte codes');
  const facts = factKeys.map(value => {
    const [nKanji, nKana] = value.split('\u0000').map(Number);
    return [
      checkedInteger(nKanji!, 0, 0xff, 'Generated nKanji'),
      checkedInteger(nKana!, 0, 0xff, 'Generated nKana')
    ] as const;
  });
  const factIds = new Map(factKeys.map((value, index) => [value, index + 1]));

  const seen = new Set<string>();
  const groups = new Set<number>();
  const rootsBySeq = new Map<number, EncodedGeneratedRecord[]>();
  const ordersBySeq = new Map<number, EncodedLookupOrder[]>();
  let physicalMembers = 0;
  let maxMemberOrd = 0;
  let maxViaMemberOrd = 0;
  let maxPropOrd = 0;
  for (const record of generated.records) {
    checkedInteger(record.rootSeq, 0, 0xffff_ffff, 'Generated root seq');
    if (record.firstAlias >= generated.aliasCount
      || (record.secondAlias !== null && record.secondAlias >= generated.aliasCount)) {
      throw new RangeError(`Generated record for ${record.rootSeq} references an unknown alias`);
    }
    const key = generatedKey(record.firstAlias, record.secondAlias);
    const unique = `${record.rootSeq}\u0000${key}`;
    if (seen.has(unique)) throw new RangeError(`Duplicate generated record ${unique}`);
    seen.add(unique);
    let fact = 0;
    if (record.counts !== null) {
      const factKey = `${record.counts[0]}\u0000${record.counts[1]}`;
      fact = factIds.get(factKey)!;
    }
    const values = rootsBySeq.get(record.rootSeq) ?? [];
    if (record.members === null) {
      if (record.physicalGroup !== null) {
        throw new RangeError(`Generated count-only record ${unique} has a physical group`);
      }
      values.push({
        key, fact, physical: 0, property: GENERATED_PROPERTY_NONE
      });
    } else {
      if (record.members.length === 0) {
        throw new RangeError(`Generated record ${unique} has an empty member list`);
      }
      const group = record.physicalGroup === null ? 0 : checkedInteger(
        record.physicalGroup, 1, GENERATED_PHYSICAL_GROUP_MAX, 'Generated physical group'
      );
      if (group !== 0) groups.add(group);
      for (const member of record.members) {
        const memberOrd = checkedInteger(
          member.memberOrd, 0, GENERATED_MEMBER_ORD_MAX, 'Generated member order'
        );
        const propOrd = checkedInteger(
          member.propOrd, 0, GENERATED_PROP_ORD_MAX, 'Generated property order'
        );
        const viaMemberOrd = member.viaMemberOrd === null
          ? GENERATED_VIA_MEMBER_NONE
          : checkedInteger(
            member.viaMemberOrd,
            0,
            GENERATED_MEMBER_ORD_MAX,
            'Generated via-member order'
          );
        maxMemberOrd = Math.max(maxMemberOrd, memberOrd);
        maxViaMemberOrd = Math.max(
          maxViaMemberOrd,
          member.viaMemberOrd === null ? 0 : member.viaMemberOrd
        );
        maxPropOrd = Math.max(maxPropOrd, propOrd);
        physicalMembers++;
        values.push({
          key: key | (propOrd << GENERATED_KEY_BITS),
          fact,
          physical: group | (memberOrd << 18) | (viaMemberOrd << 21),
          property: generatedProperty(member.property)
        });
      }
    }
    rootsBySeq.set(record.rootSeq, values);
  }
  let maximumGroup = 0;
  for (const group of groups) maximumGroup = Math.max(maximumGroup, group);
  if (groups.size !== generated.physicalGroups
    || maximumGroup !== groups.size) {
    throw new RangeError('Generated physical groups must be dense and agree with source statistics');
  }
  if (physicalMembers !== generated.physicalMembers
    || maxMemberOrd !== generated.maxMemberOrd
    || maxViaMemberOrd !== generated.maxViaMemberOrd
    || maxPropOrd !== generated.maxPropOrd) {
    throw new RangeError('Generated member statistics disagree with source');
  }

  const orderSeen = new Set<string>();
  for (const order of generated.lookupOrders) {
    checkedInteger(order.rootSeq, 1, 0xffff_ffff, 'Lookup-order root seq');
    let key = GENERATED_KEY_MASK;
    if (order.firstAlias !== null) {
      if (order.firstAlias >= generated.aliasCount
        || (order.secondAlias !== null && order.secondAlias >= generated.aliasCount)) {
        throw new RangeError(`Lookup-order record for ${order.rootSeq} references an unknown alias`);
      }
      key = generatedKey(order.firstAlias, order.secondAlias);
    } else if (order.secondAlias !== null) {
      throw new RangeError('Direct lookup-order locator cannot have a second alias');
    }
    checkedInteger(order.rank, 0, 0x3f, 'Lookup-order rank');
    const unique = `${order.rootSeq}\u0000${key}`;
    if (orderSeen.has(unique)) throw new RangeError(`Duplicate lookup-order locator ${unique}`);
    orderSeen.add(unique);
    const values = ordersBySeq.get(order.rootSeq) ?? [];
    values.push({ key, rank: order.rank });
    ordersBySeq.set(order.rootSeq, values);
  }
  let maximumOrderRank = 0;
  for (const order of generated.lookupOrders) maximumOrderRank = Math.max(maximumOrderRank, order.rank);
  if (orderSeen.size !== generated.lookupOrders.length
    || maximumOrderRank !== generated.lookupOrderMaxRank) {
    throw new RangeError('Lookup-order statistics disagree with source');
  }

  const encodedRoots: EncodedGeneratedRoot[] = [...new Set([
    ...rootsBySeq.keys(), ...ordersBySeq.keys()
  ])].sort((left, right) => left - right)
    .map(seq => {
      const records = rootsBySeq.get(seq) ?? [];
      return {
        seq,
        records: records.sort((left, right) => {
          const leftVia = (left.physical >>> 21) & 7;
          const rightVia = (right.physical >>> 21) & 7;
          return (left.key & GENERATED_KEY_MASK) - (right.key & GENERATED_KEY_MASK)
            || ((left.physical >>> 18) & 7) - ((right.physical >>> 18) & 7)
            || (left.key >>> GENERATED_KEY_BITS) - (right.key >>> GENERATED_KEY_BITS)
            || (leftVia === GENERATED_VIA_MEMBER_NONE ? -1 : leftVia)
              - (rightVia === GENERATED_VIA_MEMBER_NONE ? -1 : rightVia)
            || left.property - right.property;
        }),
        orders: (ordersBySeq.get(seq) ?? []).sort((left, right) => left.key - right.key)
      };
    });
  const groupedRoots: EncodedGeneratedRoot[][] = [];
  let current: EncodedGeneratedRoot[] = [];
  let currentRecords = 0;
  let currentOrderBytes = 0;
  for (const root of encodedRoots) {
    const nextBytes = 12
      + (current.length + 1) * 20
      + (currentRecords + root.records.length) * ANALYZER_GENERATED_RECORD_BYTES
      + currentOrderBytes
      + root.orders.length * ANALYZER_LOOKUP_ORDER_RECORD_BYTES;
    if (current.length > 0 && nextBytes > ANALYZER_GENERATED_BLOCK_TARGET_BYTES) {
      groupedRoots.push(current);
      current = [];
      currentRecords = 0;
      currentOrderBytes = 0;
    }
    current.push(root);
    currentRecords += root.records.length;
    currentOrderBytes += root.orders.length * ANALYZER_LOOKUP_ORDER_RECORD_BYTES;
  }
  if (current.length > 0) groupedRoots.push(current);

  const blocks = groupedRoots.map(roots => {
    const uncompressed = encodeGeneratedBlock(roots);
    return {
      firstRootSeq: roots[0]!.seq,
      roots,
      recordCount: roots.reduce((sum, root) => sum + root.records.length, 0),
      orderCount: roots.reduce((sum, root) => sum + root.orders.length, 0),
      uncompressed,
      compressed: gzipSync(uncompressed, { level: 9 })
    };
  });
  const rootIndex = blocks.flatMap((block, blockIndex) =>
    block.roots.map(root => ({ seq: root.seq, block: blockIndex })));
  return { blocks, roots: rootIndex, facts, orderRoots: ordersBySeq.size };
}

export function buildAnalyzerAnnotations(
  splits: readonly AnalyzerSupportSplitSource[],
  hints: readonly AnalyzerSupportHintSource[],
  generated: AnalyzerSupportGeneratedSource = {
    ruleAliases: [], aliasCount: 0, records: [], semanticPaths: 0,
    matchedPaths: 0, countExceptions: 0, physicalGroups: 0, physicalMembers: 0,
    lookupOrders: [], lookupOrderSourceRows: 0, lookupOrderSourceSha256: '',
    lookupOrderSurfaces: 0,
    lookupOrderClasses: 0, lookupOrderEquivalenceClasses: 0,
    lookupOrderComponents: 0, lookupOrderCyclicComponents: 0,
    lookupOrderEdges: 0, lookupOrderMaxRank: 0,
    lookupOrderProjectionSha256: '',
    lookupOrderExceptions: [], lookupOrderExceptionClasses: 0,
    lookupOrderExceptionLocators: 0,
    propertyOverrides: 0, maxMemberOrd: 0, maxViaMemberOrd: 0, maxPropOrd: 0,
    projectionSha256: ''
  }
): AnalyzerAnnotationsBuild {
  const splitKeys = new Set<string>();
  for (const value of splits) {
    const key = JSON.stringify([value.definitionSeq, value.route, value.surface, value.kind]);
    if (splitKeys.has(key)) throw new RangeError(`Duplicate split annotation ${key}`);
    splitKeys.add(key);
  }
  const hintKeys = new Set<string>();
  for (const value of hints) {
    const key = JSON.stringify([value.definitionSeq, value.route, value.surface, value.reading]);
    if (hintKeys.has(key)) throw new RangeError(`Duplicate hint annotation ${key}`);
    hintKeys.add(key);
  }
  const splitsBySeq = new Map<number, AnalyzerSupportSplitSource[]>();
  for (const value of splits) {
    checkedInteger(value.definitionSeq, 0, 0xffff_ffff, 'Split definition seq');
    const values = splitsBySeq.get(value.definitionSeq) ?? [];
    values.push(value);
    splitsBySeq.set(value.definitionSeq, values);
  }
  const hintsBySeq = new Map<number, AnalyzerSupportHintSource[]>();
  for (const value of hints) {
    checkedInteger(value.definitionSeq, 0, 0xffff_ffff, 'Hint definition seq');
    const values = hintsBySeq.get(value.definitionSeq) ?? [];
    values.push(value);
    hintsBySeq.set(value.definitionSeq, values);
  }
  const seqs = [...new Set([
    ...splits.map(value => value.definitionSeq),
    ...hints.map(value => value.definitionSeq)
  ])].sort((left, right) => left - right);
  const annotationBlocks: EncodedAnnotationBlock[] = seqs.map(seq => {
    const seqSplits = splitsBySeq.get(seq) ?? [];
    const seqHints = hintsBySeq.get(seq) ?? [];
    checkedInteger(seqSplits.length, 0, 0xffff, `Split count for ${seq}`);
    checkedInteger(seqHints.length, 0, 0xffff, `Hint count for ${seq}`);
    const uncompressed = UTF8.encode(canonicalJson(seq, seqSplits, seqHints));
    return {
      seq,
      splits: seqSplits.length,
      hints: seqHints.length,
      uncompressed,
      compressed: gzipSync(uncompressed, { level: 9 })
    };
  });

  const encodedGenerated = generatedBlocks(generated);
  const encodedExceptions = encodeLookupOrderExceptions(generated);
  const generatedRecordCount = encodedGenerated.blocks.reduce((sum, block) => sum + block.recordCount, 0);
  const expectedGeneratedRows = generated.records.reduce(
    (sum, record) => sum + (record.members?.length ?? 1),
    0
  );
  if (generatedRecordCount !== expectedGeneratedRows) {
    throw new RangeError('Generated block records disagree with source members');
  }

  const annotationBlocksOffset = ANALYZER_ANNOTATIONS_HEADER_BYTES;
  const generatedBlocksOffset = annotationBlocksOffset
    + annotationBlocks.length * ANALYZER_ANNOTATIONS_BLOCK_BYTES;
  const generatedRootsOffset = generatedBlocksOffset
    + encodedGenerated.blocks.length * ANALYZER_GENERATED_BLOCK_BYTES;
  const generatedFactsOffset = generatedRootsOffset
    + encodedGenerated.roots.length * ANALYZER_GENERATED_ROOT_BYTES;
  const exceptionEntriesOffset = align(generatedFactsOffset + encodedGenerated.facts.length * 2);
  const exceptionLocatorsOffset = exceptionEntriesOffset
    + encodedExceptions.exceptions.length * ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES;
  const exceptionStringsOffset = exceptionLocatorsOffset
    + encodedExceptions.locators * ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES;
  const annotationDataOffset = align(exceptionStringsOffset + encodedExceptions.strings.byteLength);
  const annotationCompressedBytes = annotationBlocks
    .reduce((sum, block) => sum + block.compressed.byteLength, 0);
  const generatedDataOffset = align(annotationDataOffset + annotationCompressedBytes);
  const generatedCompressedBytes = encodedGenerated.blocks
    .reduce((sum, block) => sum + block.compressed.byteLength, 0);
  const totalBytes = align(generatedDataOffset + generatedCompressedBytes);
  const bytes = new Uint8Array(totalBytes);
  const view = new DataView(bytes.buffer);
  for (let index = 0; index < ANALYZER_ANNOTATIONS_MAGIC.length; index++) {
    bytes[index] = ANALYZER_ANNOTATIONS_MAGIC.charCodeAt(index);
  }
  view.setUint16(8, ANALYZER_ANNOTATIONS_FORMAT_VERSION, true);
  view.setUint16(10, ANALYZER_ANNOTATIONS_HEADER_BYTES, true);
  view.setUint32(12, totalBytes, true);
  view.setUint32(24, annotationBlocks.length, true);
  view.setUint32(28, splits.length, true);
  view.setUint32(32, hints.length, true);
  view.setUint32(36, ANALYZER_ANNOTATIONS_BLOCK_BYTES, true);
  view.setUint32(40, annotationBlocksOffset, true);
  view.setUint32(44, annotationDataOffset, true);
  view.setUint32(48, annotationCompressedBytes, true);
  view.setUint32(52, encodedGenerated.blocks.length, true);
  view.setUint32(56, encodedGenerated.roots.length, true);
  view.setUint32(60, generatedRecordCount, true);
  view.setUint32(64, generated.physicalGroups, true);
  view.setUint32(68, encodedGenerated.facts.length, true);
  view.setUint32(72, ANALYZER_GENERATED_BLOCK_BYTES, true);
  view.setUint32(76, ANALYZER_GENERATED_ROOT_BYTES, true);
  view.setUint32(80, ANALYZER_GENERATED_RECORD_BYTES, true);
  view.setUint32(84, generatedBlocksOffset, true);
  view.setUint32(88, generatedRootsOffset, true);
  view.setUint32(92, generatedFactsOffset, true);
  view.setUint32(96, generatedDataOffset, true);
  view.setUint32(100, generatedCompressedBytes, true);
  const generatedUncompressedBytes = encodedGenerated.blocks
    .reduce((sum, block) => sum + block.uncompressed.byteLength, 0);
  const annotationUncompressedBytes = annotationBlocks
    .reduce((sum, block) => sum + block.uncompressed.byteLength, 0);
  const largestGeneratedBlock = Math.max(0, ...encodedGenerated.blocks
    .map(block => block.uncompressed.byteLength));
  const largestGeneratedCompressedBlock = Math.max(0, ...encodedGenerated.blocks
    .map(block => block.compressed.byteLength));
  const largestAnnotationBlock = Math.max(0, ...annotationBlocks
    .map(block => block.uncompressed.byteLength));
  view.setUint32(104, generatedUncompressedBytes, true);
  view.setUint32(108, largestGeneratedBlock, true);
  view.setUint32(112, annotationUncompressedBytes, true);
  view.setUint32(116, largestAnnotationBlock, true);
  view.setUint32(120, ANALYZER_GENERATED_BLOCK_TARGET_BYTES, true);
  view.setUint32(124, largestGeneratedCompressedBlock, true);
  view.setUint32(128, generated.lookupOrders.length, true);
  view.setUint32(132, encodedGenerated.orderRoots, true);
  view.setUint32(136, generated.lookupOrderMaxRank, true);
  view.setUint32(140, ANALYZER_LOOKUP_ORDER_RECORD_BYTES, true);
  view.setUint32(144, encodedExceptions.exceptions.length, true);
  view.setUint32(148, encodedExceptions.locators, true);
  view.setUint32(152, encodedExceptions.classes, true);
  view.setUint32(156, encodedExceptions.maxRank, true);
  view.setUint32(160, ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES, true);
  view.setUint32(164, ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES, true);
  view.setUint32(168, exceptionEntriesOffset, true);
  view.setUint32(172, exceptionLocatorsOffset, true);
  view.setUint32(176, exceptionStringsOffset, true);
  view.setUint32(180, encodedExceptions.strings.byteLength, true);

  let dataWrite = annotationDataOffset;
  let relativeData = 0;
  for (let index = 0; index < annotationBlocks.length; index++) {
    const block = annotationBlocks[index]!;
    const at = annotationBlocksOffset + index * ANALYZER_ANNOTATIONS_BLOCK_BYTES;
    view.setUint32(at, block.seq, true);
    view.setUint32(at + 4, relativeData, true);
    view.setUint32(at + 8, block.compressed.byteLength, true);
    view.setUint32(at + 12, block.uncompressed.byteLength, true);
    view.setUint32(at + 16, crc32(block.uncompressed), true);
    view.setUint16(at + 20, block.splits, true);
    view.setUint16(at + 22, block.hints, true);
    bytes.set(block.compressed, dataWrite);
    dataWrite += block.compressed.byteLength;
    relativeData += block.compressed.byteLength;
  }

  let generatedWrite = generatedDataOffset;
  let generatedRelative = 0;
  for (let index = 0; index < encodedGenerated.blocks.length; index++) {
    const block = encodedGenerated.blocks[index]!;
    const at = generatedBlocksOffset + index * ANALYZER_GENERATED_BLOCK_BYTES;
    view.setUint32(at, block.firstRootSeq, true);
    view.setUint32(at + 4, generatedRelative, true);
    view.setUint32(at + 8, block.compressed.byteLength, true);
    view.setUint32(at + 12, block.uncompressed.byteLength, true);
    view.setUint32(at + 16, crc32(block.uncompressed), true);
    view.setUint16(at + 20, checkedInteger(block.roots.length, 1, 0xffff, 'Generated block roots'), true);
    view.setUint16(at + 22, checkedInteger(block.orderCount, 0, 0xffff, 'Generated block orders'), true);
    bytes.set(block.compressed, generatedWrite);
    generatedWrite += block.compressed.byteLength;
    generatedRelative += block.compressed.byteLength;
  }
  encodedGenerated.roots.forEach((root, index) => {
    const at = generatedRootsOffset + index * ANALYZER_GENERATED_ROOT_BYTES;
    view.setUint32(at, root.seq, true);
    view.setUint16(at + 4, checkedInteger(root.block, 0, 0xffff, 'Generated root block'), true);
  });
  encodedGenerated.facts.forEach((fact, index) => {
    const at = generatedFactsOffset + index * 2;
    view.setUint8(at, fact[0]);
    view.setUint8(at + 1, fact[1]);
  });

  let exceptionStringOffset = 0;
  let exceptionLocatorIndex = 0;
  encodedExceptions.exceptions.forEach((exception, index) => {
    const at = exceptionEntriesOffset + index * ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES;
    let exceptionMaxRank = 0;
    for (const order of exception.orders) {
      exceptionMaxRank = Math.max(exceptionMaxRank, order.rank);
    }
    view.setUint32(at, exceptionStringOffset, true);
    view.setUint32(at + 4, exceptionLocatorIndex, true);
    view.setUint16(at + 8, exception.surface.byteLength, true);
    view.setUint16(at + 10, checkedInteger(
      exception.orders.length,
      1,
      0xffff,
      'Exception lookup-order locator count'
    ), true);
    view.setUint8(at + 12, exception.route);
    view.setUint8(at + 13, exceptionMaxRank);
    view.setUint16(at + 14, 0, true);
    for (const order of exception.orders) {
      const locatorAt = exceptionLocatorsOffset
        + exceptionLocatorIndex * ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES;
      view.setUint32(locatorAt, order.rootSeq, true);
      view.setUint32(locatorAt + 4, order.key | (order.rank << GENERATED_KEY_BITS), true);
      exceptionLocatorIndex++;
    }
    exceptionStringOffset += exception.surface.byteLength;
  });
  bytes.set(encodedExceptions.strings, exceptionStringsOffset);
  if (
    exceptionLocatorIndex !== encodedExceptions.locators
    || exceptionStringOffset !== encodedExceptions.strings.byteLength
  ) throw new RangeError('Lookup-order exception regions are not covered');

  view.setUint32(
    20,
    crc32(bytes.subarray(ANALYZER_ANNOTATIONS_HEADER_BYTES, annotationDataOffset)),
    true
  );
  const header = bytes.slice(0, ANALYZER_ANNOTATIONS_HEADER_BYTES);
  new DataView(header.buffer).setUint32(16, 0, true);
  view.setUint32(16, crc32(header), true);

  return {
    bytes,
    stats: {
      blocks: annotationBlocks.length,
      splits: splits.length,
      hints: hints.length,
      generatedBlocks: encodedGenerated.blocks.length,
      generatedRoots: encodedGenerated.roots.length,
      generatedRecords: generatedRecordCount,
      lookupOrderRecords: generated.lookupOrders.length,
      lookupOrderRoots: encodedGenerated.orderRoots,
      lookupOrderBytes: generated.lookupOrders.length * ANALYZER_LOOKUP_ORDER_RECORD_BYTES,
      lookupOrderSourceRows: generated.lookupOrderSourceRows,
      lookupOrderSurfaces: generated.lookupOrderSurfaces,
      lookupOrderClasses: generated.lookupOrderClasses,
      lookupOrderEquivalenceClasses: generated.lookupOrderEquivalenceClasses,
      lookupOrderEdges: generated.lookupOrderEdges,
      lookupOrderMaxRank: generated.lookupOrderMaxRank,
      lookupOrderComponents: generated.lookupOrderComponents,
      lookupOrderCyclicComponents: generated.lookupOrderCyclicComponents,
      lookupOrderExceptionSurfaces: encodedExceptions.exceptions.length,
      lookupOrderExceptionClasses: encodedExceptions.classes,
      lookupOrderExceptionLocators: encodedExceptions.locators,
      lookupOrderExceptionBytes:
        encodedExceptions.exceptions.length * ANALYZER_LOOKUP_ORDER_EXCEPTION_BYTES
        + encodedExceptions.locators * ANALYZER_LOOKUP_ORDER_EXCEPTION_LOCATOR_BYTES
        + encodedExceptions.strings.byteLength,
      generatedPhysicalGroups: generated.physicalGroups,
      generatedFactPairs: encodedGenerated.facts.length,
      indexBytes: annotationDataOffset,
      uncompressedBytes: annotationUncompressedBytes + generatedUncompressedBytes,
      compressedBytes: annotationCompressedBytes + generatedCompressedBytes,
      annotationUncompressedBytes,
      annotationCompressedBytes,
      generatedUncompressedBytes,
      generatedCompressedBytes,
      totalBytes,
      largestUncompressedBlock: largestAnnotationBlock,
      largestGeneratedBlock,
      largestGeneratedCompressedBlock
    }
  };
}
