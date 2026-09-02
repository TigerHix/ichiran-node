import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { openPack } from '../packages/core/src/pack.js';
import { ANALYZER_SUPPORT_SECTION_ID } from '../packages/core/src/analyzer-support.js';
import { ANALYZER_ANNOTATIONS_SECTION_ID } from '../packages/data/src/browser-pack/analyzer-annotations.js';
import type {
  AnalyzerSupportGeneratedMemberSource,
  AnalyzerSupportGeneratedRecordSource,
  AnalyzerSupportLookupOrderExceptionSource,
  AnalyzerSupportLookupOrderSource
} from '../packages/data/src/browser-pack/analyzer-support.js';

const GENERATED_ALIAS_BITS = 11;
const GENERATED_KEY_BITS = 22;
const GENERATED_KEY_MASK = (1 << GENERATED_KEY_BITS) - 1;
const GENERATED_PROPERTY_NONE = 0xffff;
const GENERATED_VIA_MEMBER_NONE = 7;

export interface QualifiedGeneratedProjection {
  readonly records: AnalyzerSupportGeneratedRecordSource[];
  readonly lookupOrders: AnalyzerSupportLookupOrderSource[];
  readonly lookupOrderExceptions: AnalyzerSupportLookupOrderExceptionSource[];
  readonly physicalGroups: number;
  readonly factPairs: number;
  readonly collisions: readonly QualifiedCollisionLocator[];
}

export interface QualifiedCollisionLocator {
  readonly rootSeq: number;
  readonly collisionSeq: number;
  readonly route: 'kana' | 'kanji';
  readonly surface: string;
  readonly firstRule: number;
  readonly secondRule: number | null;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
}

function u24(view: DataView, offset: number): number {
  return view.getUint8(offset) | (view.getUint8(offset + 1) << 8)
    | (view.getUint8(offset + 2) << 16);
}

function decodeProperty(value: number): AnalyzerSupportGeneratedMemberSource['property'] {
  const tri = (bits: number): boolean | null => bits === 2 ? null : bits === 1;
  return {
    posId: value & 31,
    type: (value >>> 5) & 63,
    negative: tri((value >>> 11) & 3),
    formal: tri((value >>> 13) & 3)
  };
}

function decodeKey(key: number): { firstAlias: number | null; secondAlias: number | null } {
  if (key === GENERATED_KEY_MASK) return { firstAlias: null, secondAlias: null };
  const second = key & ((1 << GENERATED_ALIAS_BITS) - 1);
  return { firstAlias: key >>> GENERATED_ALIAS_BITS, secondAlias: second === 0 ? null : second - 1 };
}

/** Proof-only decoder for generated annotation facts in the immutable qualified pack. */
export function decodeQualifiedGenerated(hotPath: string): QualifiedGeneratedProjection {
  const hot = gunzipSync(readFileSync(hotPath));
  const pack = openPack(hot);
  const bytes = pack.getSection(ANALYZER_ANNOTATIONS_SECTION_ID);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const generatedBlocks = view.getUint32(52, true);
  const physicalGroups = view.getUint32(64, true);
  const factPairs = view.getUint32(68, true);
  const generatedBlocksOffset = view.getUint32(84, true);
  const factsOffset = view.getUint32(92, true);
  const generatedDataOffset = view.getUint32(96, true);
  const exceptionSurfaces = view.getUint32(144, true);
  const exceptionLocators = view.getUint32(148, true);
  const exceptionEntriesOffset = view.getUint32(168, true);
  const exceptionLocatorsOffset = view.getUint32(172, true);
  const exceptionStringsOffset = view.getUint32(176, true);
  const facts = Array.from({ length: factPairs }, (_, index) => [
    view.getUint8(factsOffset + index * 2),
    view.getUint8(factsOffset + index * 2 + 1)
  ] as const);
  const records: AnalyzerSupportGeneratedRecordSource[] = [];
  const lookupOrders: AnalyzerSupportLookupOrderSource[] = [];
  for (let block = 0; block < generatedBlocks; block++) {
    const indexAt = generatedBlocksOffset + block * 24;
    const offset = view.getUint32(indexAt + 4, true);
    const compressed = view.getUint32(indexAt + 8, true);
    const raw = gunzipSync(bytes.subarray(
      generatedDataOffset + offset,
      generatedDataOffset + offset + compressed
    ));
    const blockView = new DataView(raw.buffer, raw.byteOffset, raw.byteLength);
    const roots = blockView.getUint32(0, true);
    const recordCount = blockView.getUint32(4, true);
    const recordsOffset = 12 + roots * 20;
    const ordersOffset = recordsOffset + recordCount * 10;
    for (let root = 0; root < roots; root++) {
      const rootAt = 12 + root * 20;
      const rootSeq = blockView.getUint32(rootAt, true);
      const firstRecord = blockView.getUint32(rootAt + 4, true);
      const rootRecords = blockView.getUint32(rootAt + 8, true);
      const firstOrder = blockView.getUint32(rootAt + 12, true);
      const rootOrders = blockView.getUint32(rootAt + 16, true);
      for (let index = 0; index < rootRecords;) {
        const at = recordsOffset + (firstRecord + index) * 10;
        const storedKey = blockView.getUint32(at, true);
        const baseKey = storedKey & GENERATED_KEY_MASK;
        const decoded = decodeKey(baseKey);
        if (decoded.firstAlias === null) throw new Error('Generated record uses direct sentinel');
        const fact = blockView.getUint8(at + 4);
        const physical = u24(blockView, at + 5);
        const group = physical & ((1 << 18) - 1);
        const members: AnalyzerSupportGeneratedMemberSource[] = [];
        let countOnly = false;
        do {
          const memberAt = recordsOffset + (firstRecord + index) * 10;
          const property = blockView.getUint16(memberAt + 8, true);
          const memberPhysical = u24(blockView, memberAt + 5);
          if (property === GENERATED_PROPERTY_NONE) countOnly = true;
          else {
            const via = (memberPhysical >>> 21) & 7;
            members.push({
              property: decodeProperty(property),
              memberOrd: (memberPhysical >>> 18) & 7,
              propOrd: blockView.getUint32(memberAt, true) >>> GENERATED_KEY_BITS,
              viaMemberOrd: via === GENERATED_VIA_MEMBER_NONE ? null : via
            });
          }
          index++;
        } while (index < rootRecords
          && (blockView.getUint32(recordsOffset + (firstRecord + index) * 10, true)
            & GENERATED_KEY_MASK) === baseKey);
        records.push({
          rootSeq,
          firstAlias: decoded.firstAlias,
          secondAlias: decoded.secondAlias,
          counts: fact === 0 ? null : facts[fact - 1]!,
          physicalGroup: group === 0 ? null : group,
          members: countOnly ? null : members
        });
      }
      for (let index = 0; index < rootOrders; index++) {
        const packed = blockView.getUint32(ordersOffset + (firstOrder + index) * 4, true);
        lookupOrders.push({ rootSeq, ...decodeKey(packed & GENERATED_KEY_MASK), rank: packed >>> 22 });
      }
    }
  }
  const decoder = new TextDecoder('utf8', { fatal: true });
  const lookupOrderExceptions: AnalyzerSupportLookupOrderExceptionSource[] = [];
  let locatorTotal = 0;
  for (let exception = 0; exception < exceptionSurfaces; exception++) {
    const at = exceptionEntriesOffset + exception * 16;
    const stringOffset = view.getUint32(at, true);
    const firstLocator = view.getUint32(at + 4, true);
    const stringBytes = view.getUint16(at + 8, true);
    const locatorCount = view.getUint16(at + 10, true);
    const route = view.getUint8(at + 12) === 0 ? 'kana' as const : 'kanji' as const;
    const surface = decoder.decode(bytes.subarray(
      exceptionStringsOffset + stringOffset,
      exceptionStringsOffset + stringOffset + stringBytes
    ));
    const orders: AnalyzerSupportLookupOrderSource[] = [];
    for (let locator = 0; locator < locatorCount; locator++) {
      const locatorAt = exceptionLocatorsOffset + (firstLocator + locator) * 8;
      const packed = view.getUint32(locatorAt + 4, true);
      orders.push({
        rootSeq: view.getUint32(locatorAt, true),
        ...decodeKey(packed & GENERATED_KEY_MASK),
        rank: (packed >>> GENERATED_KEY_BITS) & 0x3f
      });
      locatorTotal++;
    }
    lookupOrderExceptions.push({ route, surface, orders });
  }
  if (locatorTotal !== exceptionLocators) throw new Error('Qualified exception locator count disagrees');
  const support = pack.getSection(ANALYZER_SUPPORT_SECTION_ID);
  const supportView = new DataView(support.buffer, support.byteOffset, support.byteLength);
  const collisionCount = supportView.getUint32(76, true);
  const collisionOffset = supportView.getUint32(140, true);
  const stringOffsets = supportView.getUint32(144, true);
  const stringData = supportView.getUint32(148, true);
  const generatedRules = supportView.getUint32(152, true);
  const generatedAliases = supportView.getUint32(156, true);
  const generatedRuleAliases = supportView.getUint32(160, true);
  const supportDecoder = new TextDecoder('utf8', { fatal: true });
  const supportString = (id: number): string => {
    const start = supportView.getUint32(stringOffsets + id * 4, true);
    const end = supportView.getUint32(stringOffsets + (id + 1) * 4, true);
    return supportDecoder.decode(support.subarray(stringData + start, stringData + end));
  };
  const ruleAlias = (rule: number): number => {
    if (rule < 0 || rule >= generatedRules) throw new Error(`Qualified collision rule ${rule} is out of range`);
    const alias = supportView.getUint16(generatedRuleAliases + rule * 2, true);
    if (alias >= generatedAliases) throw new Error(`Qualified collision alias ${alias} is out of range`);
    return alias;
  };
  const collisions: QualifiedCollisionLocator[] = [];
  for (let index = 0; index < collisionCount; index++) {
    const at = collisionOffset + index * 36;
    const second = supportView.getUint32(at + 16, true);
    const flags = supportView.getUint16(at + 30, true);
    const firstRule = supportView.getUint32(at + 12, true);
    const secondRule = second === 0xffff_ffff ? null : second;
    collisions.push({
      rootSeq: supportView.getUint32(at, true),
      collisionSeq: supportView.getUint32(at + 4, true),
      route: (flags & 1) === 0 ? 'kana' : 'kanji',
      surface: supportString(supportView.getUint32(at + 8, true)),
      firstRule,
      secondRule,
      firstAlias: ruleAlias(firstRule),
      secondAlias: secondRule === null ? null : ruleAlias(secondRule)
    });
  }
  return {
    records,
    lookupOrders,
    lookupOrderExceptions,
    physicalGroups,
    factPairs,
    collisions
  };
}
