import { crc32 } from './crc32.js';

/** Pack section reserved for analyzer-only lookup data. */
export const ANALYZER_SUPPORT_SECTION_ID = 4;
export const ANALYZER_SUPPORT_FORMAT_VERSION = 2;
export const ANALYZER_SUPPORT_HEADER_BYTES = 224;

const MAGIC = 'IANSUP01';
const NONE = 0xffff_ffff;
const LITTLE_ENDIAN = true;
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

const COUNT_NAMES = [
  'suffixKeys', 'suffixValues', 'suffixForms', 'suffixConjugations', 'suffixClasses',
  'counterKeys', 'counterVariants', 'digitOptions', 'listMembers', 'numberMembers',
  'splits', 'splitParts', 'hints', 'collisions', 'strings', 'stringBytes'
] as const;

const OFFSET_NAMES = [
  'suffixKeysOffset', 'suffixValuesOffset', 'suffixFormsOffset', 'suffixConjugationsOffset',
  'suffixClassesOffset', 'counterKeysOffset', 'counterVariantsOffset', 'digitOptionsOffset',
  'listMembersOffset', 'numberMembersOffset', 'splitsOffset', 'splitPartsOffset',
  'hintsOffset', 'collisionsOffset', 'stringOffsetsOffset', 'stringDataOffset'
] as const;

type CountName = typeof COUNT_NAMES[number];
type OffsetName = typeof OFFSET_NAMES[number];

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

export interface AnalyzerSupportStats extends Record<CountName, number> {
  readonly byteLength: number;
  readonly generatedRules: number;
  readonly generatedAliases: number;
}

export interface AnalyzerSupportSuffixForm {
  readonly seq: number;
  readonly text: string;
  readonly bestKanji: string | null;
  readonly commonTags: string;
  readonly ord: number;
  readonly common: number | null;
  readonly conjugatable: boolean;
  readonly nokanji: boolean;
  readonly conjugations: ':root' | readonly AnalyzerSupportConjugation[] | null;
}

export interface AnalyzerSupportConjugation {
  readonly seq: number;
  readonly from: number;
  readonly via: number | null;
  readonly property: {
    readonly pos: string;
    readonly type: number;
    readonly negative: boolean | null;
    readonly formal: boolean | null;
  };
}

export interface AnalyzerSupportSuffixValue {
  readonly keyword: string;
  readonly form: AnalyzerSupportSuffixForm | null;
}

export interface AnalyzerSupportSuffixMatch {
  readonly start: number;
  readonly end: number;
  readonly text: string;
  readonly values: readonly AnalyzerSupportSuffixValue[];
}

export interface AnalyzerSupportCounterVariant {
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

export interface AnalyzerSupportCounterMatch {
  readonly start: number;
  readonly end: number;
  readonly text: string;
  readonly values: readonly AnalyzerSupportCounterVariant[];
}

export type AnalyzerSupportSplitPart =
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
      readonly generated?: readonly AnalyzerSupportSplitConjugation[] | null;
    };

export interface AnalyzerSupportSplitConjugation {
  readonly from: number;
  readonly via: boolean;
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
}

export interface AnalyzerSupportSplit {
  readonly definitionSeq: number;
  readonly route: AnalyzerSupportRoute;
  readonly surface: string;
  readonly kind: AnalyzerSupportSplitKind;
  readonly parts: readonly AnalyzerSupportSplitPart[];
  readonly score: number;
  readonly primary: number;
  readonly connector: string;
  readonly root: readonly number[];
}

export interface AnalyzerSupportHint {
  readonly definitionSeq: number;
  readonly route: AnalyzerSupportRoute;
  readonly surface: string;
  readonly reading: string;
  readonly hint: string;
}

export interface AnalyzerSupportCollision {
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

export type AnalyzerSupportFormatErrorCode =
  | 'invalid-header'
  | 'unsupported-version'
  | 'corrupt-payload'
  | 'out-of-range';

export class AnalyzerSupportFormatError extends Error {
  readonly code: AnalyzerSupportFormatErrorCode;

  constructor(code: AnalyzerSupportFormatErrorCode, message: string) {
    super(message);
    this.name = 'AnalyzerSupportFormatError';
    this.code = code;
  }
}

interface Header extends Record<CountName | OffsetName, number> {
  byteLength: number;
  generatedRules: number;
  generatedAliases: number;
  generatedRuleAliasesOffset: number;
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
  return Math.ceil(value / ALIGNMENT) * ALIGNMENT;
}

function hasMagic(bytes: Uint8Array): boolean {
  if (bytes.byteLength < MAGIC.length) return false;
  for (let index = 0; index < MAGIC.length; index++) {
    if (bytes[index] !== MAGIC.charCodeAt(index)) return false;
  }
  return true;
}

function route(code: number): AnalyzerSupportRoute {
  if (code === 0) return 'kana';
  if (code === 1) return 'kanji';
  throw new AnalyzerSupportFormatError('corrupt-payload', `Invalid route ${code}`);
}

function tri(code: number): boolean | null {
  if (code === 0) return false;
  if (code === 1) return true;
  if (code === 2) return null;
  throw new AnalyzerSupportFormatError('corrupt-payload', `Invalid tri-state ${code}`);
}

const COUNTER_CLASSES: readonly AnalyzerSupportCounterClass[] = [
  'CounterText', 'NumberText', 'CounterHalfhour', 'CounterTsu', 'CounterHifumi',
  'CounterDaysKun', 'CounterDaysOn', 'CounterMonths', 'CounterPeople', 'CounterWari', 'CounterAge'
];

export class AnalyzerSupportReader {
  readonly stats: AnalyzerSupportStats;

  private readonly bytes: Uint8Array;
  private readonly view: DataView;
  private readonly header: Header;
  private readonly strings: Array<string | undefined>;
  private readonly suffixKeysByLength = new Map<number, Set<string>>();
  private readonly suffixLengths: number[];
  private readonly counterKeysByLength = new Map<number, Set<string>>();
  private readonly counterLengths: number[];

  constructor(input: ArrayBuffer | Uint8Array) {
    this.bytes = asBytes(input);
    this.view = new DataView(this.bytes.buffer, this.bytes.byteOffset, this.bytes.byteLength);
    this.header = this.readHeader();
    this.validateLayout();
    this.strings = new Array(this.header.strings);
    for (let index = 0; index < this.header.suffixKeys; index++) {
      const key = this.string(this.u32(this.header.suffixKeysOffset + index * SUFFIX_KEY_BYTES));
      let keys = this.suffixKeysByLength.get(key.length);
      if (!keys) {
        keys = new Set();
        this.suffixKeysByLength.set(key.length, keys);
      }
      keys.add(key);
    }
    this.suffixLengths = [...this.suffixKeysByLength.keys()].sort((left, right) => right - left);
    for (let index = 0; index < this.header.counterKeys; index++) {
      const key = this.string(this.u32(this.header.counterKeysOffset + index * COUNTER_KEY_BYTES));
      let keys = this.counterKeysByLength.get(key.length);
      if (!keys) {
        keys = new Set();
        this.counterKeysByLength.set(key.length, keys);
      }
      keys.add(key);
    }
    this.counterLengths = [...this.counterKeysByLength.keys()].sort((left, right) => right - left);
    this.stats = {
      byteLength: this.header.byteLength,
      suffixKeys: this.header.suffixKeys,
      suffixValues: this.header.suffixValues,
      suffixForms: this.header.suffixForms,
      suffixConjugations: this.header.suffixConjugations,
      suffixClasses: this.header.suffixClasses,
      counterKeys: this.header.counterKeys,
      counterVariants: this.header.counterVariants,
      digitOptions: this.header.digitOptions,
      listMembers: this.header.listMembers,
      numberMembers: this.header.numberMembers,
      splits: this.header.splits,
      splitParts: this.header.splitParts,
      hints: this.header.hints,
      collisions: this.header.collisions,
      generatedRules: this.header.generatedRules,
      generatedAliases: this.header.generatedAliases,
      strings: this.header.strings,
      stringBytes: this.header.stringBytes
    };
  }

  /** Translate morphology rule IDs to the stable semantic aliases used by section 5. */
  generatedAliases(ruleIds: readonly number[]): readonly [number] | readonly [number, number] {
    if (ruleIds.length !== 1 && ruleIds.length !== 2) {
      throw new AnalyzerSupportFormatError(
        'out-of-range',
        `Generated lookup requires one or two rule IDs, got ${ruleIds.length}`
      );
    }
    const aliases = ruleIds.map(ruleId => {
      this.assertIndex(ruleId, this.header.generatedRules, 'Generated rule');
      return this.u16(
        this.header.generatedRuleAliasesOffset + ruleId * GENERATED_RULE_ALIAS_BYTES
      );
    });
    return aliases.length === 1 ? [aliases[0]!] : [aliases[0]!, aliases[1]!];
  }

  suffix(text: string): AnalyzerSupportSuffixValue[] {
    const index = this.findStringKey(
      text, this.header.suffixKeysOffset, this.header.suffixKeys, SUFFIX_KEY_BYTES
    );
    if (index < 0) return [];
    const at = this.header.suffixKeysOffset + index * SUFFIX_KEY_BYTES;
    const first = this.u32(at + 4);
    const count = this.u16(at + 8);
    const values: AnalyzerSupportSuffixValue[] = [];
    for (let offset = 0; offset < count; offset++) {
      const valueAt = this.header.suffixValuesOffset + (first + offset) * SUFFIX_VALUE_BYTES;
      const form = this.u32(valueAt + 4);
      values.push({
        keyword: this.string(this.u32(valueAt)),
        form: form === NONE ? null : this.suffixForm(form)
      });
    }
    return values;
  }

  /**
   * Exact cache matches ending at one UTF-16 offset. Known key lengths avoid
   * probing every substring; longest matches come first like `getSuffixMap`.
   */
  suffixMatchesEndingAt(
    text: string,
    end: number,
    maxCodeUnits = 50
  ): AnalyzerSupportSuffixMatch[] {
    if (!Number.isSafeInteger(end) || end < 0 || end > text.length) {
      throw new RangeError(`Suffix end ${end} lies outside the input`);
    }
    if (!Number.isSafeInteger(maxCodeUnits) || maxCodeUnits < 0) {
      throw new RangeError('Suffix maximum length must be a non-negative integer');
    }
    const output: AnalyzerSupportSuffixMatch[] = [];
    for (const length of this.suffixLengths) {
      if (length > maxCodeUnits || length > end) continue;
      const start = end - length;
      const value = text.slice(start, end);
      if (!this.suffixKeysByLength.get(length)!.has(value)) continue;
      output.push({ start, end, text: value, values: this.suffix(value) });
    }
    return output;
  }

  suffixClass(seq: number): string | null {
    let low = 0;
    let high = this.header.suffixClasses;
    while (low < high) {
      const middle = (low + high) >>> 1;
      const at = this.header.suffixClassesOffset + middle * SUFFIX_CLASS_BYTES;
      const current = this.u32(at);
      if (current < seq) low = middle + 1;
      else high = middle;
    }
    if (low >= this.header.suffixClasses) return null;
    const at = this.header.suffixClassesOffset + low * SUFFIX_CLASS_BYTES;
    return this.u32(at) === seq ? this.string(this.u32(at + 4)) : null;
  }

  counters(text: string): AnalyzerSupportCounterVariant[] {
    const index = this.findStringKey(
      text, this.header.counterKeysOffset, this.header.counterKeys, COUNTER_KEY_BYTES
    );
    if (index < 0) return [];
    const at = this.header.counterKeysOffset + index * COUNTER_KEY_BYTES;
    const first = this.u32(at + 4);
    const count = this.u16(at + 8);
    const output: AnalyzerSupportCounterVariant[] = [];
    for (let offset = 0; offset < count; offset++) output.push(this.counter(first + offset));
    return output;
  }

  /** Exact compiler-known counter keys beginning at one UTF-16 offset. */
  counterMatchesStartingAt(
    text: string,
    start: number,
    maxCodeUnits = 50
  ): AnalyzerSupportCounterMatch[] {
    if (!Number.isSafeInteger(start) || start < 0 || start > text.length) {
      throw new RangeError(`Counter start ${start} lies outside the input`);
    }
    if (!Number.isSafeInteger(maxCodeUnits) || maxCodeUnits < 0) {
      throw new RangeError('Counter maximum length must be a non-negative integer');
    }
    const output: AnalyzerSupportCounterMatch[] = [];
    for (const length of this.counterLengths) {
      if (length > maxCodeUnits || start + length > text.length) continue;
      const end = start + length;
      const value = text.slice(start, end);
      if (!this.counterKeysByLength.get(length)!.has(value)) continue;
      output.push({ start, end, text: value, values: this.counters(value) });
    }
    return output;
  }

  split(
    definitionSeq: number,
    routeValue: AnalyzerSupportRoute,
    surface: string,
    kind: AnalyzerSupportSplitKind = 'split'
  ): AnalyzerSupportSplit | null {
    let low = 0;
    let high = this.header.splits;
    const wanted = this.splitKey(definitionSeq, routeValue, surface, kind);
    while (low < high) {
      const middle = (low + high) >>> 1;
      const current = this.splitKeyAt(middle);
      if (current < wanted) low = middle + 1;
      else high = middle;
    }
    if (low >= this.header.splits || this.splitKeyAt(low) !== wanted) return null;
    return this.readSplit(low);
  }

  hint(
    definitionSeq: number,
    routeValue: AnalyzerSupportRoute,
    surface: string,
    reading: string
  ): string | null {
    const wanted = this.hintKey(definitionSeq, routeValue, surface, reading);
    let low = 0;
    let high = this.header.hints;
    while (low < high) {
      const middle = (low + high) >>> 1;
      const current = this.hintKeyAt(middle);
      if (current < wanted) low = middle + 1;
      else high = middle;
    }
    if (low >= this.header.hints || this.hintKeyAt(low) !== wanted) return null;
    return this.string(this.u32(this.header.hintsOffset + low * HINT_BYTES + 12));
  }

  collision(
    rootSeq: number,
    routeValue: AnalyzerSupportRoute,
    surface: string,
    ruleIds: readonly [number] | readonly [number, number]
  ): AnalyzerSupportCollision | null {
    const wanted = this.collisionKey(rootSeq, routeValue, surface, ruleIds[0], ruleIds[1] ?? NONE);
    let low = 0;
    let high = this.header.collisions;
    while (low < high) {
      const middle = (low + high) >>> 1;
      const current = this.collisionKeyAt(middle);
      if (current < wanted) low = middle + 1;
      else high = middle;
    }
    if (low >= this.header.collisions || this.collisionKeyAt(low) !== wanted) return null;
    return this.readCollision(low);
  }

  private suffixForm(index: number): AnalyzerSupportSuffixForm {
    this.assertIndex(index, this.header.suffixForms, 'suffix form');
    const at = this.header.suffixFormsOffset + index * SUFFIX_FORM_BYTES;
    const first = this.u32(at + 16);
    const count = this.u16(at + 22);
    const flags = this.u8(at + 25);
    const conjugations: ':root' | AnalyzerSupportConjugation[] | null = (flags & 4) !== 0
      ? ':root'
      : count === 0
        ? null
        : Array.from({ length: count }, (_, offset) => {
            const conjugationAt = this.header.suffixConjugationsOffset
              + (first + offset) * SUFFIX_CONJUGATION_BYTES;
            const via = this.u32(conjugationAt + 8);
            const conjugationFlags = this.u8(conjugationAt + 18);
            if ((conjugationFlags & 0xf0) !== 0) {
              throw new AnalyzerSupportFormatError('corrupt-payload', 'Invalid suffix conjugation flags');
            }
            return {
              seq: this.u32(conjugationAt),
              from: this.u32(conjugationAt + 4),
              via: via === NONE ? null : via,
              property: {
                pos: this.string(this.u32(conjugationAt + 12)),
                type: this.u16(conjugationAt + 16),
                negative: tri(conjugationFlags & 3),
                formal: tri((conjugationFlags >>> 2) & 3)
              }
            };
          });
    const best = this.u32(at + 8);
    const common = this.u8(at + 24);
    return {
      seq: this.u32(at),
      text: this.string(this.u32(at + 4)),
      bestKanji: best === NONE ? null : this.string(best),
      commonTags: this.string(this.u32(at + 12)),
      ord: this.u16(at + 20),
      common: common === 0xff ? null : common,
      conjugatable: (flags & 1) !== 0,
      nokanji: (flags & 2) !== 0,
      conjugations
    };
  }

  private counter(index: number): AnalyzerSupportCounterVariant {
    this.assertIndex(index, this.header.counterVariants, 'counter variant');
    const at = this.header.counterVariantsOffset + index * COUNTER_VARIANT_BYTES;
    const suffix = this.u32(at + 8);
    const sourceSeq = this.u32(at + 12);
    const flags = this.u8(at + 52);
    const common = this.u8(at + 53);
    const className = COUNTER_CLASSES[this.u8(at + 50)];
    if (!className) throw new AnalyzerSupportFormatError('corrupt-payload', 'Invalid counter class');
    return {
      className,
      text: this.string(this.u32(at)),
      kana: this.string(this.u32(at + 4)),
      suffix: suffix === NONE ? null : this.string(suffix),
      source: sourceSeq === 0 ? null : {
        seq: sourceSeq,
        route: route(this.u8(at + 51)),
        text: this.string(this.u32(at + 16)),
        ord: this.u16(at + 54)
      },
      ordinal: (flags & 1) !== 0,
      foreign: (flags & 2) !== 0,
      common: common === 0xff ? null : common,
      suffixDescriptions: this.stringList(this.u32(at + 20), this.u16(at + 24)),
      digitOptions: this.digitOptions(this.u32(at + 28), this.u16(at + 32)),
      digitSet: this.numberList(this.u32(at + 36), this.u16(at + 40)),
      allowed: this.numberList(this.u32(at + 44), this.u16(at + 48))
    };
  }

  private digitOptions(first: number, count: number): Array<readonly [number | ':off', ...string[]]> {
    const output: Array<readonly [number | ':off', ...string[]]> = [];
    for (let offset = 0; offset < count; offset++) {
      const at = this.header.digitOptionsOffset + (first + offset) * DIGIT_OPTION_BYTES;
      const digit = this.view.getInt16(at, LITTLE_ENDIAN);
      output.push([
        digit === -1 ? ':off' : digit,
        ...this.stringList(this.u32(at + 4), this.u16(at + 2))
      ]);
    }
    return output;
  }

  private readSplit(index: number): AnalyzerSupportSplit {
    const at = this.header.splitsOffset + index * SPLIT_BYTES;
    const partFirst = this.u32(at + 8);
    const partCount = this.u16(at + 24);
    const parts: AnalyzerSupportSplitPart[] = [];
    for (let offset = 0; offset < partCount; offset++) {
      const partAt = this.header.splitPartsOffset + (partFirst + offset) * SPLIT_PART_BYTES;
      const partKind = this.u8(partAt);
      if (partKind === 1) parts.push(':score');
      else if (partKind === 2) parts.push(':pscore');
      else if (partKind === 0) {
        const best = this.u32(partAt + 12);
        const common = this.u8(partAt + 3);
        const flags = this.u8(partAt + 2);
        parts.push({
          seq: this.u32(partAt + 4),
          route: route(this.u8(partAt + 1)),
          text: this.string(this.u32(partAt + 8)),
          best: best === NONE ? null : this.string(best),
          commonTags: this.string(this.u32(partAt + 16)),
          ord: this.u16(partAt + 20),
          common: common === 0xff ? null : common,
          conjugatable: (flags & 1) !== 0,
          nokanji: (flags & 2) !== 0
        });
      } else throw new AnalyzerSupportFormatError('corrupt-payload', `Invalid split part ${partKind}`);
    }
    const connector = this.u32(at + 16);
    return {
      definitionSeq: this.u32(at),
      surface: this.string(this.u32(at + 4)),
      route: route(this.u8(at + 29)),
      kind: this.u8(at + 30) === 0 ? 'split' : 'segsplit',
      parts,
      score: this.view.getInt32(at + 12, LITTLE_ENDIAN),
      connector: connector === NONE ? ' ' : this.string(connector),
      primary: this.u8(at + 28),
      root: this.numberList(this.u32(at + 20), this.u16(at + 26))
    };
  }

  private readCollision(index: number): AnalyzerSupportCollision {
    const at = this.header.collisionsOffset + index * COLLISION_BYTES;
    const second = this.u32(at + 16);
    const flags = this.u16(at + 30);
    return {
      rootSeq: this.u32(at),
      collisionSeq: this.u32(at + 4),
      viaSeq: this.u32(at + 32) === NONE ? null : this.u32(at + 32),
      surface: this.string(this.u32(at + 8)),
      route: (flags & 1) === 0 ? 'kana' : 'kanji',
      ruleIds: second === NONE
        ? [this.u32(at + 12)]
        : [this.u32(at + 12), second],
      nKanji: this.u16(at + 20),
      nKana: this.u16(at + 22),
      pos: this.stringList(this.u32(at + 24), this.u16(at + 28)),
      primaryNokanji: (flags & (1 << 1)) !== 0,
      archived: (flags & (1 << 2)) !== 0,
      preferKana: (flags & (1 << 3)) !== 0,
      preferKanaOnOrdinalZero: (flags & (1 << 4)) !== 0,
      skipWord: (flags & (1 << 5)) !== 0,
      finalParticle: (flags & (1 << 6)) !== 0,
      semiFinalParticle: (flags & (1 << 7)) !== 0,
      nonFinalParticle: (flags & (1 << 8)) !== 0,
      copula: (flags & (1 << 9)) !== 0,
      noKanjiBreakPenalty: (flags & (1 << 10)) !== 0
    };
  }

  private stringList(first: number, count: number): string[] {
    return Array.from({ length: count }, (_, offset) =>
      this.string(this.u32(this.header.listMembersOffset + (first + offset) * 4)));
  }

  private numberList(first: number, count: number): number[] {
    return Array.from({ length: count }, (_, offset) =>
      this.u32(this.header.numberMembersOffset + (first + offset) * 4));
  }

  private string(id: number): string {
    this.assertIndex(id, this.header.strings, 'string');
    let value = this.strings[id];
    if (value !== undefined) return value;
    const start = this.u32(this.header.stringOffsetsOffset + id * 4);
    const end = this.u32(this.header.stringOffsetsOffset + (id + 1) * 4);
    try {
      value = UTF8_DECODER.decode(this.bytes.subarray(
        this.header.stringDataOffset + start,
        this.header.stringDataOffset + end
      ));
    } catch {
      throw new AnalyzerSupportFormatError('corrupt-payload', `String ${id} is not UTF-8`);
    }
    this.strings[id] = value;
    return value;
  }

  private findStringKey(text: string, table: number, count: number, stride: number): number {
    let low = 0;
    let high = count;
    while (low < high) {
      const middle = (low + high) >>> 1;
      const current = this.string(this.u32(table + middle * stride));
      if (current < text) low = middle + 1;
      else high = middle;
    }
    return low < count && this.string(this.u32(table + low * stride)) === text ? low : -1;
  }

  private splitKeyAt(index: number): string {
    const at = this.header.splitsOffset + index * SPLIT_BYTES;
    return this.splitKey(
      this.u32(at), route(this.u8(at + 29)), this.string(this.u32(at + 4)),
      this.u8(at + 30) === 0 ? 'split' : 'segsplit'
    );
  }

  private splitKey(seq: number, routeValue: AnalyzerSupportRoute, surface: string, kind: AnalyzerSupportSplitKind): string {
    return `${seq.toString().padStart(10, '0')}\u0000${routeValue === 'kana' ? 0 : 1}\u0000${surface}\u0000${kind}`;
  }

  private hintKeyAt(index: number): string {
    const at = this.header.hintsOffset + index * HINT_BYTES;
    return this.hintKey(
      this.u32(at), route(this.u8(at + 16)), this.string(this.u32(at + 4)), this.string(this.u32(at + 8))
    );
  }

  private hintKey(seq: number, routeValue: AnalyzerSupportRoute, surface: string, reading: string): string {
    return `${seq.toString().padStart(10, '0')}\u0000${routeValue === 'kana' ? 0 : 1}\u0000${surface}\u0000${reading}`;
  }

  private collisionKeyAt(index: number): string {
    const at = this.header.collisionsOffset + index * COLLISION_BYTES;
    const flags = this.u16(at + 30);
    return this.collisionKey(
      this.u32(at), (flags & 1) === 0 ? 'kana' : 'kanji', this.string(this.u32(at + 8)),
      this.u32(at + 12), this.u32(at + 16)
    );
  }

  private collisionKey(rootSeq: number, routeValue: AnalyzerSupportRoute, surface: string, first: number, second: number): string {
    return `${rootSeq.toString().padStart(10, '0')}\u0000${first.toString().padStart(10, '0')}\u0000${second.toString().padStart(10, '0')}\u0000${routeValue === 'kana' ? 0 : 1}\u0000${surface}`;
  }

  private readHeader(): Header {
    if (this.bytes.byteLength < ANALYZER_SUPPORT_HEADER_BYTES || !hasMagic(this.bytes)) {
      throw new AnalyzerSupportFormatError('invalid-header', 'Invalid analyzer-support magic or truncated header');
    }
    const version = this.u16(8);
    if (version !== ANALYZER_SUPPORT_FORMAT_VERSION) {
      throw new AnalyzerSupportFormatError('unsupported-version', `Unsupported analyzer-support version ${version}`);
    }
    if (this.u16(10) !== ANALYZER_SUPPORT_HEADER_BYTES) {
      throw new AnalyzerSupportFormatError('invalid-header', 'Unexpected analyzer-support header size');
    }
    const byteLength = this.u32(12);
    if (byteLength !== this.bytes.byteLength) {
      throw new AnalyzerSupportFormatError('invalid-header', 'Analyzer-support byte length mismatch');
    }
    const headerCopy = this.bytes.slice(0, ANALYZER_SUPPORT_HEADER_BYTES);
    new DataView(headerCopy.buffer).setUint32(16, 0, LITTLE_ENDIAN);
    if (crc32(headerCopy) !== this.u32(16)) {
      throw new AnalyzerSupportFormatError('invalid-header', 'Analyzer-support header checksum mismatch');
    }
    if (crc32(this.bytes.subarray(ANALYZER_SUPPORT_HEADER_BYTES)) !== this.u32(20)) {
      throw new AnalyzerSupportFormatError('corrupt-payload', 'Analyzer-support payload checksum mismatch');
    }
    const header = { byteLength } as Header;
    COUNT_NAMES.forEach((name, index) => { header[name] = this.u32(24 + index * 4); });
    OFFSET_NAMES.forEach((name, index) => { header[name] = this.u32(88 + index * 4); });
    header.generatedRules = this.u32(152);
    header.generatedAliases = this.u32(156);
    header.generatedRuleAliasesOffset = this.u32(160);
    return header;
  }

  private validateLayout(): void {
    const h = this.header;
    let expected = ANALYZER_SUPPORT_HEADER_BYTES;
    const table = (actual: number, count: number, stride: number, label: string): void => {
      const end = actual + count * stride;
      if (actual !== expected || !Number.isSafeInteger(end) || end > h.byteLength) {
        throw new AnalyzerSupportFormatError('invalid-header', `Non-canonical ${label} layout`);
      }
      expected = end;
    };
    table(h.suffixKeysOffset, h.suffixKeys, SUFFIX_KEY_BYTES, 'suffix keys');
    table(h.suffixValuesOffset, h.suffixValues, SUFFIX_VALUE_BYTES, 'suffix values');
    table(h.suffixFormsOffset, h.suffixForms, SUFFIX_FORM_BYTES, 'suffix forms');
    table(
      h.suffixConjugationsOffset,
      h.suffixConjugations,
      SUFFIX_CONJUGATION_BYTES,
      'suffix conjugations'
    );
    table(h.suffixClassesOffset, h.suffixClasses, SUFFIX_CLASS_BYTES, 'suffix classes');
    table(h.counterKeysOffset, h.counterKeys, COUNTER_KEY_BYTES, 'counter keys');
    table(h.counterVariantsOffset, h.counterVariants, COUNTER_VARIANT_BYTES, 'counter variants');
    table(h.digitOptionsOffset, h.digitOptions, DIGIT_OPTION_BYTES, 'digit options');
    table(h.listMembersOffset, h.listMembers, 4, 'list members');
    table(h.numberMembersOffset, h.numberMembers, 4, 'number members');
    table(h.splitsOffset, h.splits, SPLIT_BYTES, 'splits');
    table(h.splitPartsOffset, h.splitParts, SPLIT_PART_BYTES, 'split parts');
    table(h.hintsOffset, h.hints, HINT_BYTES, 'hints');
    table(h.collisionsOffset, h.collisions, COLLISION_BYTES, 'collisions');
    table(h.stringOffsetsOffset, h.strings + 1, 4, 'string offsets');
    table(h.stringDataOffset, h.stringBytes, 1, 'string data');
    expected = align(expected);
    table(
      h.generatedRuleAliasesOffset,
      h.generatedRules,
      GENERATED_RULE_ALIAS_BYTES,
      'generated rule aliases'
    );
    for (let index = 0; index < h.generatedRules; index++) {
      if (this.u16(h.generatedRuleAliasesOffset + index * GENERATED_RULE_ALIAS_BYTES)
        >= h.generatedAliases) {
        throw new AnalyzerSupportFormatError('corrupt-payload', 'Generated rule alias is out of range');
      }
    }
    if (align(expected) !== h.byteLength) {
      throw new AnalyzerSupportFormatError('invalid-header', 'Analyzer-support trailing length is non-canonical');
    }
    for (let index = expected; index < h.byteLength; index++) {
      if (this.u8(index) !== 0) throw new AnalyzerSupportFormatError('corrupt-payload', 'Non-zero trailing padding');
    }
    let previous = 0;
    for (let index = 0; index <= h.strings; index++) {
      const current = this.u32(h.stringOffsetsOffset + index * 4);
      if (current < previous || current > h.stringBytes) {
        throw new AnalyzerSupportFormatError('invalid-header', 'Invalid string directory');
      }
      previous = current;
    }
    if (previous !== h.stringBytes) throw new AnalyzerSupportFormatError('invalid-header', 'String pool is not covered');
  }

  private assertIndex(index: number, count: number, label: string): void {
    if (!Number.isInteger(index) || index < 0 || index >= count) {
      throw new AnalyzerSupportFormatError('out-of-range', `${label} index ${index} is out of range`);
    }
  }

  private u8(offset: number): number { return this.view.getUint8(offset); }
  private u16(offset: number): number { return this.view.getUint16(offset, LITTLE_ENDIAN); }
  private u32(offset: number): number { return this.view.getUint32(offset, LITTLE_ENDIAN); }
}

export function openAnalyzerSupport(input: ArrayBuffer | Uint8Array): AnalyzerSupportReader {
  return new AnalyzerSupportReader(input);
}
