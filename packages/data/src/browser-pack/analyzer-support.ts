import type postgres from 'postgres';
import {
  resetAllCaches,
  withConnectionOverride
} from '@ichiran/reference-postgres/src/conn.js';
import { testWord } from '@ichiran/reference-postgres/src/characters.js';
import {
  COPULAE,
  FINAL_PRT,
  NO_KANJI_BREAK_PENALTY,
  NON_FINAL_PRT,
  SEMI_FINAL_PRT,
  SKIP_WORDS
} from '@ichiran/reference-postgres/src/dict/errata.js';
import { ensureCounterCache } from '@ichiran/reference-postgres/src/dict/counters.js';
import { hintMap, segsplitMap, splitMap } from '@ichiran/reference-postgres/src/dict/splitMaps.js';
import type { AsyncSplitFunction, HintFunction } from '@ichiran/reference-postgres/src/dict/splitMaps.js';
import '@ichiran/reference-postgres/src/dict/splitDefinitions.js';
import {
  getSuffixCache,
  getSuffixClass,
  initSuffixes
} from '@ichiran/reference-postgres/src/grammar/suffixCache.js';
import type { KanaText, Reading } from '@ichiran/reference-postgres/src/types.js';
import { compileMorphology } from './morphology-compiler.js';
import type {
  CompiledMorphologyArtifact,
  CompiledMorphologyRule
} from './morphology-format.js';
import { loadAnalyzerGeneratedSource } from './analyzer-generated.js';
import {
  loadUpstream260118GataiForms,
  UPSTREAM_260118_GATAI_CLASS,
  UPSTREAM_260118_GATAI_KEYWORD,
  UPSTREAM_260118_NEBA_ABBREVIATION,
  UPSTREAM_260118_SKIP_WORD_ADDED,
  UPSTREAM_260118_SKIP_WORD_REMOVED,
  upstream260118HintMap,
  upstream260118SplitMap
} from './analyzer-upstream-260118.js';

const MAGIC = 'IANSUP01';
const VERSION = 2;
const HEADER_BYTES = 224;
const NONE = 0xffff_ffff;
const ALIGNMENT = 8;

const SUFFIX_KEY_BYTES = 12;
const SUFFIX_VALUE_BYTES = 8;
const SUFFIX_FORM_BYTES = 32;
const SUFFIX_CONJUGATION_BYTES = 24;
const SUFFIX_CLASS_BYTES = 8;
const COUNTER_KEY_BYTES = 12;
const COUNTER_VARIANT_BYTES = 64;
const DIGIT_OPTION_BYTES = 12;
const SPLIT_BYTES = 36;
const SPLIT_PART_BYTES = 28;
const HINT_BYTES = 20;
const COLLISION_BYTES = 36;
const GENERATED_RULE_ALIAS_BYTES = 2;

const CRC32_POLYNOMIAL = 0xedb8_8320;
const CRC32_TABLE = new Uint32Array(256);
for (let value = 0; value < CRC32_TABLE.length; value++) {
  let checksum = value;
  for (let bit = 0; bit < 8; bit++) {
    checksum = (checksum & 1) === 1 ? CRC32_POLYNOMIAL ^ (checksum >>> 1) : checksum >>> 1;
  }
  CRC32_TABLE[value] = checksum >>> 0;
}

const UTF8 = new TextEncoder();

export type AnalyzerSupportRoute = 'kana' | 'kanji';
export type AnalyzerSupportSplitKind = 'split' | 'segsplit';
export type AnalyzerSupportCounterClass =
  | 'CounterText'
  | 'NumberText'
  | 'CounterHalfhour'
  | 'CounterTsu'
  | 'CounterHifumi'
  | 'CounterDaysKun'
  | 'CounterDaysOn'
  | 'CounterMonths'
  | 'CounterPeople'
  | 'CounterWari'
  | 'CounterAge';

export interface AnalyzerSupportSuffixFormSource {
  readonly seq: number;
  readonly text: string;
  readonly bestKanji: string | null;
  readonly commonTags: string;
  readonly ord: number;
  readonly common: number | null;
  readonly conjugatable: boolean;
  readonly nokanji: boolean;
  readonly conjugations: ':root' | readonly AnalyzerSupportConjugationSource[] | null;
}

export interface AnalyzerSupportConjugationSource {
  readonly seq: number;
  readonly from: number;
  readonly via: number | null;
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
}

export interface AnalyzerSupportSuffixSource {
  readonly text: string;
  /** Cache order is observable for the single joined abbreviation bucket. */
  readonly values: readonly {
    readonly keyword: string;
    readonly form: AnalyzerSupportSuffixFormSource | null;
  }[];
}

export interface AnalyzerSupportCounterSource {
  readonly key: string;
  /** Runtime order within one cache key; the analyzer tries variants in this order. */
  readonly order: number;
  readonly className: AnalyzerSupportCounterClass;
  readonly text: string;
  readonly kana: string;
  readonly suffix: string | null;
  readonly source: {
    readonly seq: number;
    readonly route: AnalyzerSupportRoute;
    readonly text: string;
    readonly ord: number;
  } | null;
  readonly ordinal: boolean;
  readonly foreign: boolean;
  readonly common: number | null;
  readonly suffixDescriptions: readonly string[];
  readonly digitOptions: readonly (readonly [number | ':off', ...string[]])[];
  readonly digitSet: readonly number[];
  readonly allowed: readonly number[];
}

export type AnalyzerSupportSplitPartSource =
  | ':score'
  | ':pscore'
  | {
      readonly seq: number;
      readonly route: AnalyzerSupportRoute;
      readonly text: string;
      readonly best: string | null;
      readonly ord: number;
      readonly common: number | null;
      readonly commonTags: string;
      readonly conjugatable: boolean;
      readonly nokanji: boolean;
      /**
       * Semantic locator for a generated physical text row. Generated target
       * sequence ids are otherwise absent from the portable morphology pack.
       */
      readonly generated?: readonly AnalyzerSupportSplitConjugationSource[] | null;
    };

export interface AnalyzerSupportSplitConjugationSource {
  readonly from: number;
  readonly via: boolean;
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
}

export interface AnalyzerSupportSplitSource {
  readonly definitionSeq: number;
  readonly route: AnalyzerSupportRoute;
  readonly surface: string;
  readonly kind: AnalyzerSupportSplitKind;
  readonly parts: readonly AnalyzerSupportSplitPartSource[];
  readonly score: number;
  readonly primary: number;
  readonly connector: string;
  readonly root: readonly number[];
}

export interface AnalyzerSupportHintSource {
  readonly definitionSeq: number;
  readonly route: AnalyzerSupportRoute;
  readonly surface: string;
  readonly reading: string;
  readonly hint: string;
}

export interface AnalyzerSupportCollisionSource {
  readonly rootSeq: number;
  readonly collisionSeq: number;
  readonly viaSeq: number | null;
  readonly route: AnalyzerSupportRoute;
  readonly surface: string;
  readonly ruleIds: readonly [number] | readonly [number, number];
  readonly nKanji: number;
  readonly nKana: number;
  readonly primaryNokanji: boolean;
  readonly archived: boolean;
  readonly preferKana: boolean;
  readonly preferKanaOnOrdinalZero: boolean;
  readonly pos: readonly string[];
  readonly skipWord: boolean;
  readonly finalParticle: boolean;
  readonly semiFinalParticle: boolean;
  readonly nonFinalParticle: boolean;
  readonly copula: boolean;
  readonly noKanjiBreakPenalty: boolean;
}

/**
 * Exact facts for ordinary (non-root target) generated entries. The resident
 * support section stores only ruleAliases; records live in the seekable
 * annotation section so the browser does not retain the full overlay.
 */
export interface AnalyzerSupportGeneratedRecordSource {
  readonly rootSeq: number;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
  /** Null means the generated entry has the same counts as its root entry. */
  readonly counts: readonly [nKanji: number, nKana: number] | null;
  /** Dense, deterministic identity shared by semantic paths to one physical target. */
  readonly physicalGroup: number | null;
  /** Null means the semantic rule itself is the sole presentation member. */
  readonly members: readonly AnalyzerSupportGeneratedMemberSource[] | null;
}

export interface AnalyzerSupportGeneratedMemberSource {
  readonly property: {
    /** Index in the morphology artifact's canonical positions array. */
    readonly posId: number;
    readonly type: number;
    /** Physical conj_prop rows retain database tri-state flags exactly. */
    readonly negative: boolean | null;
    readonly formal: boolean | null;
  };
  /** Dense physical conjugation.id order at the final stage. */
  readonly memberOrd: number;
  /** Dense conj_prop.id order inside memberOrd. */
  readonly propOrd: number;
  /** Matching prefix-stage member for two-rule paths. */
  readonly viaMemberOrd: number | null;
}

export interface AnalyzerSupportGeneratedSource {
  /** Morphology rule ID -> canonical semantic alias ID. */
  readonly ruleAliases: readonly number[];
  readonly aliasCount: number;
  readonly records: readonly AnalyzerSupportGeneratedRecordSource[];
  readonly semanticPaths: number;
  /** Physical rule/property matches before canonical semantic collapse. */
  readonly matchedPaths: number;
  readonly countExceptions: number;
  /** Sparse exact physical lookup ordering on ambiguous active surfaces. */
  readonly lookupOrders: readonly AnalyzerSupportLookupOrderSource[];
  /** Surface-local semantic locator occurrences used to prove the global relation. */
  readonly lookupOrderSourceRows: number;
  /** Full sorted semantic `(route,surface,physicalRank,locator)` source digest. */
  readonly lookupOrderSourceSha256: string;
  readonly lookupOrderSurfaces: number;
  readonly lookupOrderClasses: number;
  readonly lookupOrderEquivalenceClasses: number;
  readonly lookupOrderComponents: number;
  readonly lookupOrderCyclicComponents: number;
  readonly lookupOrderEdges: number;
  readonly lookupOrderMaxRank: number;
  readonly lookupOrderProjectionSha256: string;
  /** Exact local ranks for surfaces that cannot use the SCC-condensed global levels. */
  readonly lookupOrderExceptions: readonly AnalyzerSupportLookupOrderExceptionSource[];
  readonly lookupOrderExceptionClasses: number;
  readonly lookupOrderExceptionLocators: number;
  readonly physicalGroups: number;
  readonly physicalMembers: number;
  readonly propertyOverrides: number;
  readonly maxMemberOrd: number;
  readonly maxViaMemberOrd: number;
  readonly maxPropOrd: number;
  readonly projectionSha256: string;
}

export interface AnalyzerSupportLookupOrderSource {
  readonly rootSeq: number;
  /** Null is the lexical/direct locator for rootSeq. */
  readonly firstAlias: number | null;
  readonly secondAlias: number | null;
  /** SCC-condensed global level; exact on every non-exception surface. */
  readonly rank: number;
}

export interface AnalyzerSupportLookupOrderExceptionSource {
  readonly route: 'kana' | 'kanji';
  readonly surface: string;
  /** Complete locator set for the surface, with dense physical-class ranks. */
  readonly orders: readonly AnalyzerSupportLookupOrderSource[];
}

export interface AnalyzerSupportSource {
  readonly suffixes: readonly AnalyzerSupportSuffixSource[];
  readonly suffixClasses: readonly { readonly seq: number; readonly keyword: string }[];
  readonly counters: readonly AnalyzerSupportCounterSource[];
  readonly splits: readonly AnalyzerSupportSplitSource[];
  readonly hints: readonly AnalyzerSupportHintSource[];
  readonly collisions: readonly AnalyzerSupportCollisionSource[];
  readonly generated?: AnalyzerSupportGeneratedSource;
  /** Existing-runtime failures encountered while freezing otherwise valid forms. */
  readonly issues?: readonly AnalyzerSupportCompileIssue[];
}

export interface AnalyzerSupportCompileIssue {
  readonly kind: 'hint-runtime-error';
  readonly definitionSeq: number;
  readonly route: AnalyzerSupportRoute;
  readonly surface: string;
  readonly reading: string;
  readonly message: string;
}

export interface AnalyzerSupportBuildStats {
  readonly counts: {
    readonly suffixKeys: number;
    readonly suffixValues: number;
    readonly suffixForms: number;
    readonly suffixConjugations: number;
    readonly suffixClasses: number;
    readonly counterKeys: number;
    readonly counterVariants: number;
    readonly digitOptions: number;
    readonly listMembers: number;
    readonly numberMembers: number;
    readonly splits: number;
    readonly splitParts: number;
    readonly hints: number;
    readonly collisions: number;
    readonly generatedRules: number;
    readonly generatedAliases: number;
    readonly strings: number;
  };
  readonly bytes: number;
}

export interface AnalyzerSupportBuild {
  readonly bytes: Uint8Array;
  readonly stats: AnalyzerSupportBuildStats;
}

export class AnalyzerSupportEncodingError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'AnalyzerSupportEncodingError';
  }
}

function crc32(bytes: Uint8Array): number {
  let checksum = 0xffff_ffff;
  for (const byte of bytes) checksum = CRC32_TABLE[(checksum ^ byte) & 0xff]! ^ (checksum >>> 8);
  return (checksum ^ 0xffff_ffff) >>> 0;
}

function align(value: number): number {
  return Math.ceil(value / ALIGNMENT) * ALIGNMENT;
}

function checked(value: number, max: number, label: string): number {
  if (!Number.isSafeInteger(value) || value < 0 || value > max) {
    throw new AnalyzerSupportEncodingError(`${label} must be an integer in [0, ${max}]`);
  }
  return value;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function routeCode(route: AnalyzerSupportRoute): number {
  return route === 'kana' ? 0 : 1;
}

function triCode(value: boolean | null): number {
  return value === false ? 0 : value === true ? 1 : 2;
}

const COUNTER_CLASSES: readonly AnalyzerSupportCounterClass[] = [
  'CounterText', 'NumberText', 'CounterHalfhour', 'CounterTsu', 'CounterHifumi',
  'CounterDaysKun', 'CounterDaysOn', 'CounterMonths', 'CounterPeople', 'CounterWari', 'CounterAge'
];

function suffixFormKey(form: AnalyzerSupportSuffixFormSource): string {
  return JSON.stringify([
    form.seq, form.text, form.bestKanji, form.commonTags, form.ord, form.common,
    form.conjugatable, form.nokanji, form.conjugations
  ]);
}

function splitKey(value: AnalyzerSupportSplitSource): string {
  return `${value.definitionSeq.toString().padStart(10, '0')}\u0000${routeCode(value.route)}\u0000${value.surface}\u0000${value.kind}`;
}

function hintKey(value: AnalyzerSupportHintSource): string {
  return `${value.definitionSeq.toString().padStart(10, '0')}\u0000${routeCode(value.route)}\u0000${value.surface}\u0000${value.reading}`;
}

function collisionKey(value: AnalyzerSupportCollisionSource): string {
  return `${value.rootSeq.toString().padStart(10, '0')}\u0000${value.ruleIds[0].toString().padStart(10, '0')}\u0000${(value.ruleIds[1] ?? NONE).toString().padStart(10, '0')}\u0000${routeCode(value.route)}\u0000${value.surface}`;
}

function assertUnique<T>(values: readonly T[], key: (value: T) => string, label: string): void {
  const seen = new Set<string>();
  for (const value of values) {
    const current = key(value);
    if (seen.has(current)) throw new AnalyzerSupportEncodingError(`Duplicate ${label} ${JSON.stringify(current)}`);
    seen.add(current);
  }
}

function addStrings(target: Set<string>, values: readonly (string | null)[]): void {
  for (const value of values) if (value !== null) target.add(value);
}

export function buildAnalyzerSupport(source: AnalyzerSupportSource): AnalyzerSupportBuild {
  assertUnique(source.suffixes, value => value.text, 'suffix key');
  assertUnique(source.suffixClasses, value => String(value.seq), 'suffix class');
  assertUnique(source.counters, value => `${value.key}\u0000${value.order}`, 'counter variant');
  assertUnique(source.splits, splitKey, 'split');
  assertUnique(source.hints, hintKey, 'hint');
  assertUnique(source.collisions, collisionKey, 'collision');

  const generated = source.generated ?? {
    ruleAliases: [], aliasCount: 0, records: [], semanticPaths: 0,
    countExceptions: 0, physicalGroups: 0, physicalMembers: 0, propertyOverrides: 0,
    maxPropOrd: 0,
    projectionSha256: ''
  };
  checked(generated.aliasCount, 0xffff, 'Generated alias count');
  generated.ruleAliases.forEach((alias, ruleId) => {
    checked(alias, 0xffff, `Generated alias for rule ${ruleId}`);
    if (alias >= generated.aliasCount) {
      throw new AnalyzerSupportEncodingError(
        `Generated alias ${alias} for rule ${ruleId} lies outside ${generated.aliasCount} aliases`
      );
    }
  });

  const suffixes = [...source.suffixes].sort((left, right) => compareText(left.text, right.text));
  const suffixClasses = [...source.suffixClasses].sort((left, right) => left.seq - right.seq);
  const counters = [...source.counters].sort((left, right) =>
    compareText(left.key, right.key) || left.order - right.order);
  const splits = [...source.splits].sort((left, right) => compareText(splitKey(left), splitKey(right)));
  const hints = [...source.hints].sort((left, right) => compareText(hintKey(left), hintKey(right)));
  const collisions = [...source.collisions].map(value => ({ ...value, pos: [...new Set(value.pos)].sort(compareText) }))
    .sort((left, right) => compareText(collisionKey(left), collisionKey(right)));

  const formMap = new Map<string, AnalyzerSupportSuffixFormSource>();
  for (const suffix of suffixes) {
    if (suffix.values.length === 0) throw new AnalyzerSupportEncodingError('Suffix keys cannot be empty');
    for (const value of suffix.values) if (value.form) formMap.set(suffixFormKey(value.form), value.form);
  }
  const suffixForms = [...formMap.values()].sort((left, right) => compareText(suffixFormKey(left), suffixFormKey(right)));
  const suffixFormIds = new Map(suffixForms.map((form, index) => [suffixFormKey(form), index]));

  const strings = new Set<string>();
  for (const suffix of suffixes) {
    strings.add(suffix.text);
    for (const value of suffix.values) strings.add(value.keyword);
  }
  for (const value of suffixClasses) strings.add(value.keyword);
  for (const form of suffixForms) {
    addStrings(strings, [form.text, form.bestKanji, form.commonTags]);
    if (Array.isArray(form.conjugations)) {
      for (const conjugation of form.conjugations) strings.add(conjugation.pos);
    }
  }
  for (const counter of counters) {
    addStrings(strings, [counter.key, counter.text, counter.kana, counter.suffix, counter.source?.text ?? null]);
    addStrings(strings, counter.suffixDescriptions);
    for (const option of counter.digitOptions) addStrings(strings, option.slice(1) as string[]);
  }
  for (const split of splits) {
    addStrings(strings, [split.surface, split.connector]);
    for (const part of split.parts) {
      if (typeof part !== 'string') addStrings(strings, [part.text, part.best, part.commonTags]);
    }
  }
  for (const value of hints) addStrings(strings, [value.surface, value.reading, value.hint]);
  for (const value of collisions) addStrings(strings, [value.surface, ...value.pos]);
  strings.add('');

  const stringList = [...strings].sort(compareText);
  const stringIds = new Map(stringList.map((value, index) => [value, index]));
  const stringId = (value: string): number => {
    const id = stringIds.get(value);
    if (id === undefined) throw new AnalyzerSupportEncodingError(`Missing string ${JSON.stringify(value)}`);
    return id;
  };
  const encodedStrings = stringList.map(value => UTF8.encode(value));
  const stringOffsets = new Uint32Array(stringList.length + 1);
  let stringBytes = 0;
  encodedStrings.forEach((value, index) => {
    stringOffsets[index] = stringBytes;
    stringBytes += value.byteLength;
  });
  stringOffsets[stringList.length] = stringBytes;

  const listMembers: number[] = [];
  const numberMembers: number[] = [];
  const listSpan = (values: readonly string[]): readonly [number, number] => {
    const first = listMembers.length;
    for (const value of values) listMembers.push(stringId(value));
    return [first, values.length];
  };
  const numberSpan = (values: readonly number[]): readonly [number, number] => {
    const first = numberMembers.length;
    for (const value of values) numberMembers.push(checked(value, 0xffff_ffff, 'Number-list member'));
    return [first, values.length];
  };

  const suffixConjugations: AnalyzerSupportConjugationSource[] = [];
  const suffixFormSpans = suffixForms.map(form => {
    if (form.conjugations === ':root' || form.conjugations === null) return [suffixConjugations.length, 0] as const;
    const first = suffixConjugations.length;
    for (const value of form.conjugations) suffixConjugations.push(value);
    return [first, form.conjugations.length] as const;
  });

  const suffixValues = suffixes.flatMap(value => value.values);
  const suffixKeySpans: Array<readonly [number, number]> = [];
  let suffixValueOffset = 0;
  for (const suffix of suffixes) {
    suffixKeySpans.push([suffixValueOffset, suffix.values.length]);
    suffixValueOffset += suffix.values.length;
  }

  const counterKeySpans: Array<{ key: string; first: number; count: number }> = [];
  for (let index = 0; index < counters.length;) {
    const key = counters[index]!.key;
    let end = index + 1;
    while (end < counters.length && counters[end]!.key === key) end++;
    counterKeySpans.push({ key, first: index, count: end - index });
    index = end;
  }

  interface CounterLayout {
    descriptions: readonly [number, number];
    digitOptions: readonly [number, number];
    digitSet: readonly [number, number];
    allowed: readonly [number, number];
  }
  const digitOptions: Array<{ digit: number; options: readonly [number, number] }> = [];
  const counterLayouts: CounterLayout[] = [];
  for (const counter of counters) {
    const firstOption = digitOptions.length;
    for (const option of counter.digitOptions) {
      const digit = option[0] === ':off' ? -1 : checked(option[0], 0x7fff, 'Counter digit');
      digitOptions.push({ digit, options: listSpan(option.slice(1) as string[]) });
    }
    counterLayouts.push({
      descriptions: listSpan(counter.suffixDescriptions),
      digitOptions: [firstOption, counter.digitOptions.length],
      digitSet: numberSpan(counter.digitSet),
      allowed: numberSpan(counter.allowed)
    });
  }

  const splitPartFirst: number[] = [];
  const splitParts = splits.flatMap(split => {
    splitPartFirst.push(splitPartFirst.length === 0 ? 0 : 0);
    return split.parts;
  });
  splitPartFirst.length = 0;
  let nextPart = 0;
  for (const split of splits) {
    splitPartFirst.push(nextPart);
    nextPart += split.parts.length;
  }
  const splitRootSpans = splits.map(split => numberSpan(split.root));
  const collisionPosSpans = collisions.map(value => listSpan(value.pos));

  const counts = {
    suffixKeys: suffixes.length,
    suffixValues: suffixValues.length,
    suffixForms: suffixForms.length,
    suffixConjugations: suffixConjugations.length,
    suffixClasses: suffixClasses.length,
    counterKeys: counterKeySpans.length,
    counterVariants: counters.length,
    digitOptions: digitOptions.length,
    listMembers: listMembers.length,
    numberMembers: numberMembers.length,
    splits: splits.length,
    splitParts: splitParts.length,
    hints: hints.length,
    collisions: collisions.length,
    generatedRules: generated.ruleAliases.length,
    generatedAliases: generated.aliasCount,
    strings: stringList.length
  };

  let offset = HEADER_BYTES;
  const suffixKeysOffset = offset; offset += counts.suffixKeys * SUFFIX_KEY_BYTES;
  const suffixValuesOffset = offset; offset += counts.suffixValues * SUFFIX_VALUE_BYTES;
  const suffixFormsOffset = offset; offset += counts.suffixForms * SUFFIX_FORM_BYTES;
  const suffixConjugationsOffset = offset; offset += counts.suffixConjugations * SUFFIX_CONJUGATION_BYTES;
  const suffixClassesOffset = offset; offset += counts.suffixClasses * SUFFIX_CLASS_BYTES;
  const counterKeysOffset = offset; offset += counts.counterKeys * COUNTER_KEY_BYTES;
  const counterVariantsOffset = offset; offset += counts.counterVariants * COUNTER_VARIANT_BYTES;
  const digitOptionsOffset = offset; offset += counts.digitOptions * DIGIT_OPTION_BYTES;
  const listMembersOffset = offset; offset += counts.listMembers * 4;
  const numberMembersOffset = offset; offset += counts.numberMembers * 4;
  const splitsOffset = offset; offset += counts.splits * SPLIT_BYTES;
  const splitPartsOffset = offset; offset += counts.splitParts * SPLIT_PART_BYTES;
  const hintsOffset = offset; offset += counts.hints * HINT_BYTES;
  const collisionsOffset = offset; offset += counts.collisions * COLLISION_BYTES;
  const stringOffsetsOffset = offset; offset += stringOffsets.byteLength;
  const stringDataOffset = offset; offset += stringBytes;
  const generatedRuleAliasesOffset = align(offset);
  offset = generatedRuleAliasesOffset + counts.generatedRules * GENERATED_RULE_ALIAS_BYTES;
  const totalBytes = align(offset);

  const bytes = new Uint8Array(totalBytes);
  const view = new DataView(bytes.buffer);
  for (let index = 0; index < MAGIC.length; index++) bytes[index] = MAGIC.charCodeAt(index);
  view.setUint16(8, VERSION, true);
  view.setUint16(10, HEADER_BYTES, true);
  view.setUint32(12, totalBytes, true);

  const countValues = [
    counts.suffixKeys, counts.suffixValues, counts.suffixForms, counts.suffixConjugations,
    counts.suffixClasses, counts.counterKeys, counts.counterVariants, counts.digitOptions,
    counts.listMembers, counts.numberMembers, counts.splits, counts.splitParts, counts.hints,
    counts.collisions, counts.strings, stringBytes
  ];
  countValues.forEach((value, index) => view.setUint32(24 + index * 4, value, true));
  const offsetValues = [
    suffixKeysOffset, suffixValuesOffset, suffixFormsOffset, suffixConjugationsOffset,
    suffixClassesOffset, counterKeysOffset, counterVariantsOffset, digitOptionsOffset,
    listMembersOffset, numberMembersOffset, splitsOffset, splitPartsOffset, hintsOffset,
    collisionsOffset, stringOffsetsOffset, stringDataOffset
  ];
  offsetValues.forEach((value, index) => view.setUint32(88 + index * 4, value, true));
  view.setUint32(152, counts.generatedRules, true);
  view.setUint32(156, counts.generatedAliases, true);
  view.setUint32(160, generatedRuleAliasesOffset, true);

  suffixes.forEach((suffix, index) => {
    const at = suffixKeysOffset + index * SUFFIX_KEY_BYTES;
    const span = suffixKeySpans[index]!;
    view.setUint32(at, stringId(suffix.text), true);
    view.setUint32(at + 4, span[0], true);
    view.setUint16(at + 8, checked(span[1], 0xffff, 'Suffix value count'), true);
  });
  suffixValues.forEach((value, index) => {
    const at = suffixValuesOffset + index * SUFFIX_VALUE_BYTES;
    view.setUint32(at, stringId(value.keyword), true);
    view.setUint32(at + 4, value.form ? suffixFormIds.get(suffixFormKey(value.form))! : NONE, true);
  });
  suffixForms.forEach((form, index) => {
    const at = suffixFormsOffset + index * SUFFIX_FORM_BYTES;
    const span = suffixFormSpans[index]!;
    checked(form.ord, 0xffff, 'Suffix form ordinal');
    if (form.common !== null) checked(form.common, 0xfe, 'Suffix common rank');
    view.setUint32(at, checked(form.seq, 0xffff_ffff, 'Suffix form seq'), true);
    view.setUint32(at + 4, stringId(form.text), true);
    view.setUint32(at + 8, form.bestKanji === null ? NONE : stringId(form.bestKanji), true);
    view.setUint32(at + 12, stringId(form.commonTags), true);
    view.setUint32(at + 16, span[0], true);
    view.setUint16(at + 20, form.ord, true);
    view.setUint16(at + 22, checked(span[1], 0xffff, 'Suffix conjugation count'), true);
    view.setUint8(at + 24, form.common ?? 0xff);
    view.setUint8(at + 25,
      (form.conjugatable ? 1 : 0) | (form.nokanji ? 2 : 0) | (form.conjugations === ':root' ? 4 : 0));
  });
  suffixConjugations.forEach((value, index) => {
    const at = suffixConjugationsOffset + index * SUFFIX_CONJUGATION_BYTES;
    view.setUint32(at, checked(value.seq, 0xffff_ffff, 'Suffix conjugation seq'), true);
    view.setUint32(at + 4, checked(value.from, 0xffff_ffff, 'Suffix conjugation root'), true);
    view.setUint32(at + 8, value.via === null ? NONE : checked(value.via, 0xffff_ffff, 'Suffix conjugation via'), true);
    view.setUint32(at + 12, stringId(value.pos), true);
    view.setUint16(at + 16, checked(value.type, 0xffff, 'Suffix conjugation type'), true);
    view.setUint8(at + 18, triCode(value.negative) | (triCode(value.formal) << 2));
  });
  suffixClasses.forEach((value, index) => {
    const at = suffixClassesOffset + index * SUFFIX_CLASS_BYTES;
    view.setUint32(at, checked(value.seq, 0xffff_ffff, 'Suffix class seq'), true);
    view.setUint32(at + 4, stringId(value.keyword), true);
  });

  counterKeySpans.forEach((value, index) => {
    const at = counterKeysOffset + index * COUNTER_KEY_BYTES;
    view.setUint32(at, stringId(value.key), true);
    view.setUint32(at + 4, value.first, true);
    view.setUint16(at + 8, checked(value.count, 0xffff, 'Counter variant count'), true);
  });
  counters.forEach((counter, index) => {
    const at = counterVariantsOffset + index * COUNTER_VARIANT_BYTES;
    const layout = counterLayouts[index]!;
    const classId = COUNTER_CLASSES.indexOf(counter.className);
    if (classId < 0) throw new AnalyzerSupportEncodingError(`Unknown counter class ${counter.className}`);
    if (counter.common !== null) checked(counter.common, 0xfe, 'Counter common rank');
    view.setUint32(at, stringId(counter.text), true);
    view.setUint32(at + 4, stringId(counter.kana), true);
    view.setUint32(at + 8, counter.suffix === null ? NONE : stringId(counter.suffix), true);
    view.setUint32(at + 12, counter.source?.seq ?? 0, true);
    view.setUint32(at + 16, counter.source ? stringId(counter.source.text) : NONE, true);
    view.setUint32(at + 20, layout.descriptions[0], true);
    view.setUint16(at + 24, checked(layout.descriptions[1], 0xffff, 'Description count'), true);
    view.setUint32(at + 28, layout.digitOptions[0], true);
    view.setUint16(at + 32, checked(layout.digitOptions[1], 0xffff, 'Digit-option count'), true);
    view.setUint32(at + 36, layout.digitSet[0], true);
    view.setUint16(at + 40, checked(layout.digitSet[1], 0xffff, 'Digit-set count'), true);
    view.setUint32(at + 44, layout.allowed[0], true);
    view.setUint16(at + 48, checked(layout.allowed[1], 0xffff, 'Allowed count'), true);
    view.setUint8(at + 50, classId);
    view.setUint8(at + 51, counter.source ? routeCode(counter.source.route) : 0);
    view.setUint8(at + 52, (counter.ordinal ? 1 : 0) | (counter.foreign ? 2 : 0));
    view.setUint8(at + 53, counter.common ?? 0xff);
    view.setUint16(at + 54, checked(counter.source?.ord ?? 0, 0xffff, 'Counter source ordinal'), true);
  });
  digitOptions.forEach((value, index) => {
    const at = digitOptionsOffset + index * DIGIT_OPTION_BYTES;
    view.setInt16(at, value.digit, true);
    view.setUint16(at + 2, checked(value.options[1], 0xffff, 'Digit-option token count'), true);
    view.setUint32(at + 4, value.options[0], true);
  });
  listMembers.forEach((value, index) => view.setUint32(listMembersOffset + index * 4, value, true));
  numberMembers.forEach((value, index) => view.setUint32(numberMembersOffset + index * 4, value, true));

  splits.forEach((split, index) => {
    const at = splitsOffset + index * SPLIT_BYTES;
    const rootSpan = splitRootSpans[index]!;
    checked(split.primary, 0xff, 'Split primary');
    view.setUint32(at, checked(split.definitionSeq, 0xffff_ffff, 'Split definition seq'), true);
    view.setUint32(at + 4, stringId(split.surface), true);
    view.setUint32(at + 8, splitPartFirst[index]!, true);
    view.setInt32(at + 12, split.score, true);
    view.setUint32(at + 16, split.connector === ' ' ? NONE : stringId(split.connector), true);
    view.setUint32(at + 20, rootSpan[0], true);
    view.setUint16(at + 24, checked(split.parts.length, 0xffff, 'Split part count'), true);
    view.setUint16(at + 26, checked(rootSpan[1], 0xffff, 'Split root count'), true);
    view.setUint8(at + 28, split.primary);
    view.setUint8(at + 29, routeCode(split.route));
    view.setUint8(at + 30, split.kind === 'split' ? 0 : 1);
  });
  splitParts.forEach((part, index) => {
    const at = splitPartsOffset + index * SPLIT_PART_BYTES;
    if (part === ':score' || part === ':pscore') {
      view.setUint8(at, part === ':score' ? 1 : 2);
      return;
    }
    if (part.common !== null) checked(part.common, 0xfe, 'Split-part common rank');
    view.setUint8(at, 0);
    view.setUint8(at + 1, routeCode(part.route));
    view.setUint8(at + 2, (part.conjugatable ? 1 : 0) | (part.nokanji ? 2 : 0));
    view.setUint8(at + 3, part.common ?? 0xff);
    view.setUint32(at + 4, checked(part.seq, 0xffff_ffff, 'Split-part seq'), true);
    view.setUint32(at + 8, stringId(part.text), true);
    view.setUint32(at + 12, part.best === null ? NONE : stringId(part.best), true);
    view.setUint32(at + 16, stringId(part.commonTags), true);
    view.setUint16(at + 20, checked(part.ord, 0xffff, 'Split-part ordinal'), true);
  });
  hints.forEach((value, index) => {
    const at = hintsOffset + index * HINT_BYTES;
    view.setUint32(at, checked(value.definitionSeq, 0xffff_ffff, 'Hint definition seq'), true);
    view.setUint32(at + 4, stringId(value.surface), true);
    view.setUint32(at + 8, stringId(value.reading), true);
    view.setUint32(at + 12, stringId(value.hint), true);
    view.setUint8(at + 16, routeCode(value.route));
  });
  collisions.forEach((value, index) => {
    const at = collisionsOffset + index * COLLISION_BYTES;
    const posSpan = collisionPosSpans[index]!;
    let flags = routeCode(value.route);
    if (value.primaryNokanji) flags |= 1 << 1;
    if (value.archived) flags |= 1 << 2;
    if (value.preferKana) flags |= 1 << 3;
    if (value.preferKanaOnOrdinalZero) flags |= 1 << 4;
    if (value.skipWord) flags |= 1 << 5;
    if (value.finalParticle) flags |= 1 << 6;
    if (value.semiFinalParticle) flags |= 1 << 7;
    if (value.nonFinalParticle) flags |= 1 << 8;
    if (value.copula) flags |= 1 << 9;
    if (value.noKanjiBreakPenalty) flags |= 1 << 10;
    view.setUint32(at, checked(value.rootSeq, 0xffff_ffff, 'Collision root seq'), true);
    view.setUint32(at + 4, checked(value.collisionSeq, 0xffff_ffff, 'Collision target seq'), true);
    view.setUint32(at + 8, stringId(value.surface), true);
    view.setUint32(at + 12, checked(value.ruleIds[0], 0xffff_ffff, 'First rule ID'), true);
    view.setUint32(at + 16, value.ruleIds[1] ?? NONE, true);
    view.setUint16(at + 20, checked(value.nKanji, 0xffff, 'Collision nKanji'), true);
    view.setUint16(at + 22, checked(value.nKana, 0xffff, 'Collision nKana'), true);
    view.setUint32(at + 24, posSpan[0], true);
    view.setUint16(at + 28, checked(posSpan[1], 0xffff, 'Collision POS count'), true);
    view.setUint16(at + 30, flags, true);
    view.setUint32(
      at + 32,
      value.viaSeq === null ? NONE : checked(value.viaSeq, 0xffff_ffff, 'Collision via seq'),
      true
    );
  });

  const offsetsBytes = new Uint8Array(stringOffsets.buffer);
  bytes.set(offsetsBytes, stringOffsetsOffset);
  let stringWrite = stringDataOffset;
  for (const value of encodedStrings) {
    bytes.set(value, stringWrite);
    stringWrite += value.byteLength;
  }
  generated.ruleAliases.forEach((alias, index) => {
    view.setUint16(generatedRuleAliasesOffset + index * GENERATED_RULE_ALIAS_BYTES, alias, true);
  });

  view.setUint32(20, crc32(bytes.subarray(HEADER_BYTES)), true);
  const headerCopy = bytes.slice(0, HEADER_BYTES);
  new DataView(headerCopy.buffer).setUint32(16, 0, true);
  view.setUint32(16, crc32(headerCopy), true);

  return { bytes, stats: { counts, bytes: totalBytes } };
}

/**
 * Build only the always-resident support tables. Split/hint annotations belong
 * in the seekable cold store built by `buildAnalyzerAnnotations`.
 */
export function buildAnalyzerSupportCore(source: AnalyzerSupportSource): AnalyzerSupportBuild {
  return buildAnalyzerSupport({ ...source, splits: [], hints: [] });
}

interface DirectFormRow {
  seq: number;
  route: AnalyzerSupportRoute;
  text: string;
  ord: number;
  common: number | null;
  commonTags: string;
  conjugatable: boolean;
  nokanji: boolean;
  best: string | null;
}

interface AnnotationCandidate {
  rootSeq: number;
  route: AnalyzerSupportRoute;
  surface: string;
  form: string;
  reading: string;
  ord: number;
  common: number | null;
  ruleIds: readonly [number] | readonly [number, number] | null;
}

interface CollisionPathRow {
  collisionSeq: number;
  rootSeq: number;
  via: number | null;
  pos: string;
  conjType: number;
  negative: boolean | null;
  formal: boolean | null;
  sourceText: string;
  surface: string;
}

interface CollisionEntryRow {
  seq: number;
  nKanji: number;
  nKana: number;
  primaryNokanji: boolean;
  archived: boolean;
  preferKana: boolean;
  preferKanaOnOrdinalZero: boolean;
  pos: string[] | null;
}

interface RawSuffixFormSource extends Omit<AnalyzerSupportSuffixFormSource, 'conjugations'> {
  readonly conjugations: ':root' | readonly number[] | null;
}

interface RawSuffixSource extends Omit<AnalyzerSupportSuffixSource, 'values'> {
  readonly values: readonly {
    readonly keyword: string;
    readonly form: RawSuffixFormSource | null;
  }[];
}

interface SuffixConjugationRow extends AnalyzerSupportConjugationSource {
  conjugationId: number;
  propertyId: number;
  surface: string;
}

function applyMorphologyRule(word: string, rule: CompiledMorphologyRule): string {
  const kana = /^[ァ-ヺヽヾーぁ-ゔゝゞー]+$/.test(word.slice(Math.max(0, word.length - 2)));
  const euphony = kana ? rule.euphr : rule.euphk;
  return word.slice(0, word.length - rule.stem - (euphony.length > 0 ? 1 : 0)) + euphony + rule.okuri;
}

function annotationCandidateKey(value: AnnotationCandidate): string {
  return JSON.stringify([
    value.rootSeq, value.route, value.surface, value.form, value.reading, value.ruleIds
  ]);
}

function enumerateMorphologyCandidates(
  artifact: CompiledMorphologyArtifact,
  selectedRoots: ReadonlySet<number>
): AnnotationCandidate[] {
  const templatesByPos = new Map<string, typeof artifact.templates>();
  for (const template of artifact.templates) {
    const pos = artifact.rules[template.firstRule]!.pos;
    let values = templatesByPos.get(pos);
    if (!values) {
      values = [];
      templatesByPos.set(pos, values);
    }
    (values as typeof artifact.templates[number][]).push(template);
  }
  const rootForms = new Map(artifact.rootGroups.map(group => [group.seq, new Set(group.forms)]));
  const tombstones = new Set(artifact.tombstones.map(value => JSON.stringify([
    value.route, value.surface, value.rootSeq, value.firstRule, value.secondRule
  ])));
  const candidates = new Map<string, AnnotationCandidate>();

  for (const key of artifact.rootKeys) {
    const templates = templatesByPos.get(key.pos) ?? [];
    for (const record of key.records) {
      const group = artifact.rootGroups[record.rootGroup]!;
      if (!selectedRoots.has(group.seq)) continue;
      for (const template of templates) {
        const first = artifact.rules[template.firstRule]!;
        const second = template.secondRule === null ? null : artifact.rules[template.secondRule]!;
        const intermediateSurface = applyMorphologyRule(key.sourceText, first);
        const surface = second ? applyMorphologyRule(intermediateSurface, second) : intermediateSurface;
        if (rootForms.get(group.seq)?.has(surface)) continue;
        if (tombstones.has(JSON.stringify([
          key.route, surface, group.seq, template.firstRule, template.secondRule
        ]))) continue;
        const intermediateForm = applyMorphologyRule(record.sourceForm, first);
        const intermediateReading = applyMorphologyRule(record.sourceReading, first);
        const value: AnnotationCandidate = {
          rootSeq: group.seq,
          route: key.route,
          surface,
          form: second ? applyMorphologyRule(intermediateForm, second) : intermediateForm,
          reading: second ? applyMorphologyRule(intermediateReading, second) : intermediateReading,
          ord: record.ord,
          common: record.common,
          ruleIds: template.secondRule === null
            ? [template.firstRule]
            : [template.firstRule, template.secondRule]
        };
        candidates.set(annotationCandidateKey(value), value);
      }
    }
  }
  for (const patch of artifact.patches) {
    if (!selectedRoots.has(patch.rootSeq)) continue;
    const value: AnnotationCandidate = {
      rootSeq: patch.rootSeq,
      route: patch.route,
      surface: patch.surface,
      form: patch.form,
      reading: patch.reading,
      ord: patch.ord,
      common: patch.common,
      ruleIds: patch.secondRule === null
        ? [patch.firstRule]
        : [patch.firstRule, patch.secondRule]
    };
    candidates.set(annotationCandidateKey(value), value);
  }
  return [...candidates.values()].sort((left, right) =>
    compareText(annotationCandidateKey(left), annotationCandidateKey(right)));
}

function ruleMatches(
  rule: CompiledMorphologyRule,
  row: Pick<CollisionPathRow, 'pos' | 'conjType' | 'negative' | 'formal' | 'sourceText' | 'surface'>
): boolean {
  return rule.pos === row.pos
    && rule.type === row.conjType
    && (rule.negative === null || rule.negative === row.negative)
    && (rule.formal === null || rule.formal === row.formal)
    && applyMorphologyRule(row.sourceText, rule) === row.surface;
}

async function loadCollisionSources(
  sql: postgres.Sql,
  artifact: CompiledMorphologyArtifact
): Promise<AnalyzerSupportCollisionSource[]> {
  const pathRows = await sql<CollisionPathRow[]>`
    SELECT c.seq AS collision_seq, c."from" AS root_seq, c.via,
           cp.pos, cp.conj_type, cp.neg AS negative, cp.fml AS formal,
           csr.source_text, csr.text AS surface
    FROM conjugation c
    JOIN entry target ON target.seq = c.seq AND target.root_p
    JOIN conj_prop cp ON cp.conj_id = c.id
    JOIN conj_source_reading csr ON csr.conj_id = c.id
    ORDER BY c."from", c.seq, c.id, cp.id, csr.source_text COLLATE "C", csr.text COLLATE "C"
  `;
  const collisionSeqs = [...new Set(pathRows.map(row => row.collisionSeq))];
  if (collisionSeqs.length === 0) return [];
  const entryRows = await sql<CollisionEntryRow[]>`
    WITH archived AS (
      SELECT sense.seq
      FROM sense
      LEFT JOIN sense_prop sp ON sp.sense_id = sense.id
        AND sp.tag = 'misc' AND sp.text IN ('arch', 'obsc', 'rare')
      WHERE sense.seq = ANY(${collisionSeqs})
      GROUP BY sense.seq
      HAVING EVERY(sp.id IS NOT NULL)
    ), facts AS (
      SELECT e.seq, e.n_kanji, e.n_kana, e.primary_nokanji,
             EXISTS (SELECT 1 FROM archived a WHERE a.seq = e.seq) AS archived,
             EXISTS (
               SELECT 1 FROM sense_prop sp
               WHERE sp.seq = e.seq AND sp.tag = 'misc' AND sp.text = 'uk'
             ) AS prefer_kana,
             EXISTS (
               SELECT 1 FROM sense_prop sp JOIN sense s ON s.id = sp.sense_id
               WHERE sp.seq = e.seq AND sp.tag = 'misc' AND sp.text = 'uk' AND s.ord = 0
             ) AS prefer_kana_on_ordinal_zero,
             ARRAY(
               SELECT selected.text FROM (
                 SELECT DISTINCT sp1.text
                 FROM sense_prop sp1
                 LEFT JOIN sense_prop sp2 ON sp1.sense_id = sp2.sense_id
                   AND sp2.tag = 'misc' AND sp2.text IN ('arch', 'obsc', 'rare')
                 WHERE sp1.seq = e.seq AND sp1.tag = 'pos' AND sp2.id IS NULL
               ) selected ORDER BY selected.text COLLATE "C"
             ) AS pos
      FROM entry e WHERE e.seq = ANY(${collisionSeqs})
    )
    SELECT * FROM facts ORDER BY seq
  `;
  const entries = new Map(entryRows.map(row => [row.seq, row]));

  const direct = new Map<string, { target: number; via: number | null }>();
  const byIntermediate = new Map<number, CollisionPathRow[]>();
  for (const row of pathRows) {
    if (row.via === null) {
      artifact.rules.forEach((rule, ruleId) => {
        if (ruleMatches(rule, row)) {
          const key = JSON.stringify([row.rootSeq, row.collisionSeq, row.surface, ruleId, NONE]);
          direct.set(key, { target: row.collisionSeq, via: null });
        }
      });
    } else {
      let values = byIntermediate.get(row.via);
      if (!values) {
        values = [];
        byIntermediate.set(row.via, values);
      }
      values.push(row);
    }
  }

  if (byIntermediate.size > 0) {
    const viaSeqs = [...byIntermediate.keys()];
    const firstRows = await sql<CollisionPathRow[]>`
      SELECT c.seq AS collision_seq, c."from" AS root_seq, c.via,
             cp.pos, cp.conj_type, cp.neg AS negative, cp.fml AS formal,
             csr.source_text, csr.text AS surface
      FROM conjugation c
      JOIN conj_prop cp ON cp.conj_id = c.id
      JOIN conj_source_reading csr ON csr.conj_id = c.id
      WHERE c.seq = ANY(${viaSeqs})
      ORDER BY c."from", c.seq, c.id, cp.id, csr.source_text COLLATE "C", csr.text COLLATE "C"
    `;
    const firstByVia = new Map<number, CollisionPathRow[]>();
    for (const row of firstRows) {
      let values = firstByVia.get(row.collisionSeq);
      if (!values) {
        values = [];
        firstByVia.set(row.collisionSeq, values);
      }
      values.push(row);
    }
    for (const [via, finalRows] of byIntermediate) {
      for (const finalRow of finalRows) {
        for (const firstRow of firstByVia.get(via) ?? []) {
          if (firstRow.rootSeq !== finalRow.rootSeq || firstRow.surface !== finalRow.sourceText) continue;
          artifact.rules.forEach((firstRule, firstRuleId) => {
            if (!ruleMatches(firstRule, firstRow)) return;
            artifact.rules.forEach((secondRule, secondRuleId) => {
              if (!ruleMatches(secondRule, finalRow)) return;
              const key = JSON.stringify([
                finalRow.rootSeq, finalRow.collisionSeq, finalRow.surface, firstRuleId, secondRuleId
              ]);
              direct.set(key, { target: finalRow.collisionSeq, via });
            });
          });
        }
      }
    }
  }

  // Manual compatibility patches use an explicit rule and may not be
  // reproducible by applying that rule to the irregular source mapping.
  const pathByRootSurface = new Map<string, Array<{ target: number; via: number | null }>>();
  for (const row of pathRows) {
    const key = JSON.stringify([row.rootSeq, row.surface]);
    let values = pathByRootSurface.get(key);
    if (!values) {
      values = [];
      pathByRootSurface.set(key, values);
    }
    if (!values.some(value => value.target === row.collisionSeq && value.via === row.via)) {
      values.push({ target: row.collisionSeq, via: row.via });
    }
  }
  for (const patch of artifact.patches) {
    const targets = pathByRootSurface.get(JSON.stringify([patch.rootSeq, patch.surface]));
    if (!targets) continue;
    for (const target of targets) {
      direct.set(JSON.stringify([
        patch.rootSeq, target.target, patch.surface, patch.firstRule, patch.secondRule ?? NONE
      ]), target);
    }
  }

  const collisionRoots = new Set(pathRows.map(row => row.rootSeq));
  const emitted = enumerateMorphologyCandidates(artifact, collisionRoots);
  const output = new Map<string, AnalyzerSupportCollisionSource>();
  for (const candidate of emitted) {
    if (!candidate.ruleIds) continue;
    const first = candidate.ruleIds[0];
    const second = candidate.ruleIds[1] ?? NONE;
    const possible = pathRows.filter(row =>
      row.rootSeq === candidate.rootSeq && row.surface === candidate.surface);
    for (const row of possible) {
      const match = direct.get(JSON.stringify([
        candidate.rootSeq, row.collisionSeq, candidate.surface, first, second
      ]));
      if (match === undefined) continue;
      const target = match.target;
      const entry = entries.get(target);
      if (!entry) throw new AnalyzerSupportEncodingError(`Collision ${target} has no entry facts`);
      const value: AnalyzerSupportCollisionSource = {
        rootSeq: candidate.rootSeq,
        collisionSeq: target,
        viaSeq: match.via,
        route: candidate.route,
        surface: candidate.surface,
        ruleIds: candidate.ruleIds,
        nKanji: entry.nKanji,
        nKana: entry.nKana,
        primaryNokanji: entry.primaryNokanji,
        archived: entry.archived,
        preferKana: entry.preferKana,
        preferKanaOnOrdinalZero: entry.preferKanaOnOrdinalZero,
        pos: entry.pos ?? [],
        skipWord: target === UPSTREAM_260118_SKIP_WORD_ADDED
          || (target !== UPSTREAM_260118_SKIP_WORD_REMOVED && SKIP_WORDS.includes(target)),
        finalParticle: FINAL_PRT.includes(target),
        semiFinalParticle: SEMI_FINAL_PRT.includes(target),
        nonFinalParticle: NON_FINAL_PRT.includes(target),
        copula: COPULAE.includes(target),
        noKanjiBreakPenalty: NO_KANJI_BREAK_PENALTY.includes(target)
      };
      const key = collisionKey(value);
      const prior = output.get(key);
      if (prior && JSON.stringify(prior) !== JSON.stringify(value)) {
        throw new AnalyzerSupportEncodingError(`Conflicting collision facts for ${key}`);
      }
      output.set(key, value);
    }
  }
  return [...output.values()].sort((left, right) => compareText(collisionKey(left), collisionKey(right)));
}

function rawSuffixForm(form: KanaText): RawSuffixFormSource {
  return {
    seq: form.seq,
    text: form.text,
    bestKanji: form.bestKanji,
    commonTags: form.commonTags,
    ord: form.ord,
    common: form.common,
    conjugatable: form.conjugateP,
    nokanji: form.nokanji,
    conjugations: form.conjugations ?? null
  };
}

async function suffixSources(): Promise<{
  suffixes: RawSuffixSource[];
  suffixClasses: Array<{ seq: number; keyword: string }>;
}> {
  const cache = getSuffixCache();
  const classes = getSuffixClass();
  if (!cache || !classes) throw new AnalyzerSupportEncodingError('Suffix cache was not initialized');
  const suffixes = new Map<string, RawSuffixSource>();
  for (const [text, entry] of cache) {
    const rawValues = Array.isArray(entry[0]) ? entry as Array<[string, KanaText | null]> : [entry as [string, KanaText | null]];
    suffixes.set(text, {
      text,
      values: rawValues.map(([keyword, form]) => ({
        keyword,
        form: form === null ? null : rawSuffixForm(form)
      }))
    });
  }

  // These are compiler-owned overlays, equivalent to upstream's load-conjs
  // and load-abbr calls, without mutating the frozen reference suffix cache.
  const suffixClasses = new Map<number, string>(classes);
  for (const form of await loadUpstream260118GataiForms()) {
    suffixes.set(form.text, {
      text: form.text,
      values: [{ keyword: UPSTREAM_260118_GATAI_KEYWORD, form: rawSuffixForm(form) }]
    });
    suffixClasses.set(form.seq, UPSTREAM_260118_GATAI_CLASS);
  }
  suffixes.set(UPSTREAM_260118_NEBA_ABBREVIATION.text, {
    text: UPSTREAM_260118_NEBA_ABBREVIATION.text,
    values: [{ keyword: UPSTREAM_260118_NEBA_ABBREVIATION.keyword, form: null }]
  });

  return {
    suffixes: [...suffixes.values()],
    suffixClasses: [...suffixClasses].map(([seq, keyword]) => ({ seq, keyword }))
  };
}

async function hydrateSuffixConjugations(
  sql: postgres.Sql,
  suffixes: readonly RawSuffixSource[]
): Promise<AnalyzerSupportSuffixSource[]> {
  const forms = suffixes.flatMap(suffix => suffix.values.flatMap(value =>
    value.form === null ? [] : [value.form]));
  const seqs = [...new Set(forms
    .filter(form => form.conjugations !== ':root')
    .map(form => form.seq))];
  const rows = seqs.length === 0 ? [] : await sql<SuffixConjugationRow[]>`
    SELECT c.id AS conjugation_id, cp.id AS property_id,
           c.seq, c."from", c.via, cp.pos, cp.conj_type AS type,
           cp.neg AS negative, cp.fml AS formal, csr.text AS surface
    FROM conjugation c
    JOIN conj_prop cp ON cp.conj_id = c.id
    JOIN conj_source_reading csr ON csr.conj_id = c.id
    WHERE c.seq = ANY(${seqs})
    ORDER BY c.id, cp.id, csr.text COLLATE "C"
  `;
  const rowsByForm = new Map<string, SuffixConjugationRow[]>();
  const seen = new Map<string, Set<string>>();
  for (const row of rows) {
    const key = JSON.stringify([row.seq, row.surface]);
    const rowKey = `${row.conjugationId}\u0000${row.propertyId}`;
    let rowSeen = seen.get(key);
    if (!rowSeen) {
      rowSeen = new Set();
      seen.set(key, rowSeen);
    }
    if (rowSeen.has(rowKey)) continue;
    rowSeen.add(rowKey);
    const values = rowsByForm.get(key) ?? [];
    values.push(row);
    rowsByForm.set(key, values);
  }
  return suffixes.map(suffix => ({
    text: suffix.text,
    values: suffix.values.map(value => {
      const raw = value.form;
      if (raw === null) return { keyword: value.keyword, form: null };
      if (raw.conjugations === ':root') {
        return { keyword: value.keyword, form: { ...raw, conjugations: ':root' } };
      }
      const selectedIds = raw.conjugations && raw.conjugations.length > 0
        ? new Set(raw.conjugations)
        : null;
      const conjugations = (rowsByForm.get(JSON.stringify([raw.seq, raw.text])) ?? [])
        .filter(row => selectedIds === null || selectedIds.has(row.conjugationId))
        .map(({ conjugationId: _conjugationId, propertyId: _propertyId, surface: _surface, ...row }) => row);
      return {
        keyword: value.keyword,
        form: { ...raw, conjugations: conjugations.length === 0 ? null : conjugations }
      };
    })
  }));
}

async function counterSources(): Promise<AnalyzerSupportCounterSource[]> {
  const cache = await ensureCounterCache();
  const output: AnalyzerSupportCounterSource[] = [];
  for (const [key, variants] of cache) {
    for (let order = 0; order < variants.length; order++) {
      const [counterClass, options] = variants[order]!;
      const className = counterClass.name as AnalyzerSupportCounterClass;
      if (!COUNTER_CLASSES.includes(className)) {
        throw new AnalyzerSupportEncodingError(`Unsupported counter class ${counterClass.name}`);
      }
      const source = options.source ?? null;
      output.push({
        key,
        order,
        className,
        text: options.text,
        kana: options.kana,
        suffix: options.suffix ?? null,
        source: source === null ? null : {
          seq: source.seq,
          route: testWord(source.text, 'kana') ? 'kana' : 'kanji',
          text: source.text,
          ord: source.ord
        },
        ordinal: options.ordinalp ?? false,
        foreign: options.foreign ?? false,
        common: options.common ?? null,
        suffixDescriptions: options.suffixDescriptions ?? [],
        digitOptions: (options.digitOpts ?? []).map(option => {
          const [digit, ...tokens] = option;
          if (digit !== ':off' && typeof digit !== 'number') {
            throw new AnalyzerSupportEncodingError(`Unsupported counter digit ${JSON.stringify(digit)}`);
          }
          return [digit, ...tokens] as readonly [number | ':off', ...string[]];
        }),
        digitSet: options.digitSet ?? [],
        allowed: options.allowed ?? []
      });
    }
  }
  return output;
}

async function loadDirectForms(sql: postgres.Sql, seqs: readonly number[]): Promise<DirectFormRow[]> {
  if (seqs.length === 0) return [];
  return sql<DirectFormRow[]>`
    SELECT * FROM (
      SELECT k.seq, 'kanji'::text AS route, k.text, k.ord, k.common,
             k.common_tags, k.conjugate_p AS conjugatable, k.nokanji, k.best_kana AS best
      FROM kanji_text k JOIN entry e USING (seq)
      WHERE e.root_p AND k.seq = ANY(${seqs})
      UNION ALL
      SELECT r.seq, 'kana'::text AS route, r.text, r.ord, r.common,
             r.common_tags, r.conjugate_p AS conjugatable, r.nokanji, r.best_kanji AS best
      FROM kana_text r JOIN entry e USING (seq)
      WHERE e.root_p AND r.seq = ANY(${seqs})
    ) forms
    ORDER BY seq, route COLLATE "C", text COLLATE "C", ord
  `;
}

function directCandidate(row: DirectFormRow): AnnotationCandidate {
  return {
    rootSeq: row.seq,
    route: row.route,
    surface: row.text,
    form: row.route === 'kanji' ? row.text : row.best ?? row.text,
    reading: row.route === 'kana' ? row.text : row.best ?? row.text,
    ord: row.ord,
    common: row.common,
    ruleIds: null
  };
}

function readingFor(candidate: AnnotationCandidate, definitionSeq: number): Reading {
  const common = candidate.common;
  if (candidate.route === 'kana') {
    return {
      id: 0,
      seq: definitionSeq,
      text: candidate.surface,
      ord: candidate.ord,
      common,
      commonTags: '',
      conjugateP: false,
      nokanji: false,
      bestKanji: candidate.form === candidate.reading ? null : candidate.form,
      hintedp: true
    };
  }
  return {
    id: 0,
    seq: definitionSeq,
    text: candidate.surface,
    ord: candidate.ord,
    common,
    commonTags: '',
    conjugateP: false,
    nokanji: false,
    bestKana: candidate.reading,
    hintedp: true
  };
}

function splitPartSource(part: unknown): AnalyzerSupportSplitPartSource {
  if (part === ':score' || part === ':pscore') return part;
  if (!part || typeof part !== 'object' || !('text' in part) || !('seq' in part)) {
    throw new AnalyzerSupportEncodingError(`Unsupported split part ${JSON.stringify(part)}`);
  }
  const word = part as {
    seq: number; text: string; ord: number; common: number | null; commonTags: string;
    conjugateP: boolean; nokanji: boolean; bestKana?: string | null; bestKanji?: string | null;
  };
  const route: AnalyzerSupportRoute = testWord(word.text, 'kana') ? 'kana' : 'kanji';
  return {
    seq: word.seq,
    route,
    text: word.text,
    best: route === 'kana' ? word.bestKanji ?? null : word.bestKana ?? null,
    ord: word.ord,
    common: word.common,
    commonTags: word.commonTags,
    conjugatable: word.conjugateP,
    nokanji: word.nokanji,
    generated: null
  };
}

interface SplitConjugationRow extends AnalyzerSupportSplitConjugationSource {
  readonly seq: number;
  readonly viaSeq: number | null;
}

async function splitGeneratedLocators(
  sql: postgres.Sql,
  splits: readonly AnalyzerSupportSplitSource[]
): Promise<ReadonlyMap<number, readonly AnalyzerSupportSplitConjugationSource[]>> {
  const seqs = [...new Set(splits.flatMap(split => split.parts.flatMap(part =>
    typeof part === 'string' ? [] : [part.seq])))];
  if (seqs.length === 0) return new Map();
  const rows = await sql<SplitConjugationRow[]>`
    SELECT c.seq, c."from", c.via AS via_seq,
           cp.pos, cp.conj_type AS type,
           cp.neg AS negative, cp.fml AS formal
    FROM conjugation c
    JOIN conj_prop cp ON cp.conj_id = c.id
    WHERE c.seq = ANY(${seqs})
    ORDER BY c.seq, c."from", c.via NULLS FIRST,
             cp.pos COLLATE "C", cp.conj_type,
             cp.neg NULLS FIRST, cp.fml NULLS FIRST
  `;
  const output = new Map<number, AnalyzerSupportSplitConjugationSource[]>();
  const seen = new Map<number, Set<string>>();
  for (const row of rows) {
    const key = JSON.stringify([
      row.from, row.viaSeq !== null, row.pos, row.type, row.negative, row.formal
    ]);
    const seqSeen = seen.get(row.seq) ?? new Set<string>();
    if (seqSeen.has(key)) continue;
    seqSeen.add(key);
    seen.set(row.seq, seqSeen);
    const values = output.get(row.seq) ?? [];
    values.push({
      from: row.from,
      via: row.viaSeq !== null,
      pos: row.pos,
      type: row.type,
      negative: row.negative,
      formal: row.formal
    });
    output.set(row.seq, values);
  }
  return output;
}

async function annotationSources(
  sql: postgres.Sql,
  candidates: readonly AnnotationCandidate[],
  collisions: readonly AnalyzerSupportCollisionSource[],
  activeSplitMap: ReadonlyMap<number, AsyncSplitFunction>,
  activeSegsplitMap: ReadonlyMap<number, AsyncSplitFunction>,
  activeHintMap: ReadonlyMap<number, HintFunction>
): Promise<{
  splits: AnalyzerSupportSplitSource[];
  hints: AnalyzerSupportHintSource[];
  issues: AnalyzerSupportCompileIssue[];
}> {
  const collisionMap = new Map(collisions.map(value => [collisionKey(value), value]));
  const splitOutput = new Map<string, AnalyzerSupportSplitSource>();
  const hintOutput = new Map<string, AnalyzerSupportHintSource>();
  const issueOutput = new Map<string, AnalyzerSupportCompileIssue>();

  const collisionFor = (candidate: AnnotationCandidate): AnalyzerSupportCollisionSource | null => {
    if (!candidate.ruleIds) return null;
    const key = `${candidate.rootSeq.toString().padStart(10, '0')}\u0000${candidate.ruleIds[0].toString().padStart(10, '0')}\u0000${(candidate.ruleIds[1] ?? NONE).toString().padStart(10, '0')}\u0000${routeCode(candidate.route)}\u0000${candidate.surface}`;
    return collisionMap.get(key) ?? null;
  };

  for (const candidate of candidates) {
    const collision = collisionFor(candidate);
    for (const [kind, map] of [
      ['split', activeSplitMap],
      ['segsplit', activeSegsplitMap]
    ] as const) {
      const definitionSeq = collision && map.has(collision.collisionSeq)
        ? collision.collisionSeq
        : map.has(candidate.rootSeq) ? candidate.rootSeq : null;
      if (definitionSeq === null) continue;
      const result = await map.get(definitionSeq)!(readingFor(candidate, definitionSeq));
      if (!result || result[0].some(part => part === null)) continue;
      const attrs = result[1];
      const value: AnalyzerSupportSplitSource = {
        definitionSeq,
        route: candidate.route,
        surface: candidate.surface,
        kind,
        parts: result[0].map(splitPartSource),
        score: typeof attrs === 'number' ? attrs : attrs.score,
        primary: typeof attrs === 'number' ? 0 : attrs.primary ?? 0,
        connector: typeof attrs === 'number' ? ' ' : attrs.connector ?? ' ',
        root: typeof attrs === 'number' ? [] : attrs.root ?? []
      };
      const key = splitKey(value);
      const prior = splitOutput.get(key);
      if (prior && JSON.stringify(prior) !== JSON.stringify(value)) {
        throw new AnalyzerSupportEncodingError(`Split output depends on unkeyed state for ${key}`);
      }
      splitOutput.set(key, value);
    }

    const definitionSeq = collision && activeHintMap.has(collision.collisionSeq)
      ? collision.collisionSeq
      : activeHintMap.has(candidate.rootSeq) ? candidate.rootSeq : null;
    if (definitionSeq !== null) {
      let hint: string | null;
      try {
        hint = await activeHintMap.get(definitionSeq)!(readingFor(candidate, definitionSeq));
      } catch (error) {
        const issue: AnalyzerSupportCompileIssue = {
          kind: 'hint-runtime-error',
          definitionSeq,
          route: candidate.route,
          surface: candidate.surface,
          reading: candidate.reading,
          message: error instanceof Error ? error.message : String(error)
        };
        issueOutput.set(JSON.stringify(issue), issue);
        continue;
      }
      if (hint !== null) {
        const value: AnalyzerSupportHintSource = {
          definitionSeq,
          route: candidate.route,
          surface: candidate.surface,
          reading: candidate.reading,
          hint
        };
        const key = hintKey(value);
        const prior = hintOutput.get(key);
        if (prior && prior.hint !== hint) {
          throw new AnalyzerSupportEncodingError(`Hint output depends on unkeyed state for ${key}`);
        }
        hintOutput.set(key, value);
      }
    }
  }
  const splits = [...splitOutput.values()];
  const generatedLocators = await splitGeneratedLocators(sql, splits);
  return {
    splits: splits.map(split => ({
      ...split,
      parts: split.parts.map(part => {
        if (typeof part === 'string') return part;
        const generated = generatedLocators.get(part.seq);
        return generated ? { ...part, generated } : part;
      })
    })),
    hints: [...hintOutput.values()],
    issues: [...issueOutput.values()]
  };
}

/**
 * Resolve every analyzer-only database/cache dependency into a pinned source.
 * The returned object is deliberately plain data and can be encoded without a
 * live core runtime.
 */
export async function loadAnalyzerSupportSource(sql: postgres.Sql): Promise<AnalyzerSupportSource> {
  return withConnectionOverride(sql, async () => {
    // Counter/no-conjugation/archive caches are connection-owned in the legacy
    // runtime. A release build must never reuse values from an earlier DB.
    resetAllCaches();
    await initSuffixes({ blocking: true, reset: true });
    const [morphology, counters] = await Promise.all([
      compileMorphology({ sql }),
      counterSources()
    ]);
    const collisions = await loadCollisionSources(sql, morphology.artifact);
    const generated = await loadAnalyzerGeneratedSource(sql, morphology.artifact);
    const activeSplitMap = new Map<number, AsyncSplitFunction>(splitMap);
    for (const [seq, split] of upstream260118SplitMap) activeSplitMap.set(seq, split);
    const activeSegsplitMap = new Map<number, AsyncSplitFunction>(segsplitMap);
    const activeHintMap = new Map<number, HintFunction>(hintMap);
    for (const [seq, hint] of upstream260118HintMap) activeHintMap.set(seq, hint);
    const roots = new Set<number>([
      ...activeSplitMap.keys(), ...activeSegsplitMap.keys(), ...activeHintMap.keys(),
      ...collisions
        .filter(value =>
          activeSplitMap.has(value.collisionSeq)
          || activeSegsplitMap.has(value.collisionSeq)
          || activeHintMap.has(value.collisionSeq))
        .map(value => value.rootSeq)
    ]);
    const directForms = await loadDirectForms(sql, [...roots]);
    const candidates = new Map<string, AnnotationCandidate>();
    for (const row of directForms) {
      const value = directCandidate(row);
      candidates.set(annotationCandidateKey(value), value);
    }
    for (const value of enumerateMorphologyCandidates(morphology.artifact, roots)) {
      candidates.set(annotationCandidateKey(value), value);
    }
    const annotations = await annotationSources(
      sql,
      [...candidates.values()],
      collisions,
      activeSplitMap,
      activeSegsplitMap,
      activeHintMap
    );
    const suffix = await suffixSources();
    const suffixes = await hydrateSuffixConjugations(sql, suffix.suffixes);
    return {
      suffixes,
      suffixClasses: suffix.suffixClasses,
      counters,
      splits: annotations.splits,
      hints: annotations.hints,
      collisions,
      generated,
      issues: annotations.issues
    };
  });
}

// Kept local to the compiler so the browser reader remains Postgres-free.
export type AnalyzerSupportSql = postgres.Sql;
