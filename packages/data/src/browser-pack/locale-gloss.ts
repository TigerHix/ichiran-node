import {
  BinaryStoreEncodingError,
  BinaryWriter,
  buildIndexedGzipStore,
  type IndexedGzipStoreStats
} from './indexed-gzip-store.js';

export const LOCALE_GLOSS_MAGIC = 'ICHIGLOS';
export const LOCALE_GLOSS_FORMAT_VERSION = 1;
export const LOCALE_GLOSS_HEADER_BYTES = 128;
export const LOCALE_GLOSS_LEXICON_SHA256_OFFSET = 60;
export const LOCALE_GLOSS_LOCALE_LENGTH_OFFSET = 92;
export const LOCALE_GLOSS_LOCALE_OFFSET = 93;
export const LOCALE_GLOSS_LOCALE_MAX_BYTES = 31;

const HEX_SHA256 = /^[0-9a-f]{64}$/;
const LOCALE_TAG = /^[A-Za-z]{2,8}(?:-[A-Za-z0-9]{1,8})*$/;
const UTF8_ENCODER = new TextEncoder();

export interface LocaleGlossTextSource {
  readonly ord: number;
  readonly text: string;
}

export interface LocaleGlossGroupSource {
  /** Empty means this source cannot safely align the group to a base sense. */
  readonly targets: readonly number[];
  readonly glosses: readonly LocaleGlossTextSource[];
  readonly info: readonly LocaleGlossTextSource[];
}

export interface LocaleGlossEntrySource {
  readonly seq: number;
  readonly groups: readonly LocaleGlossGroupSource[];
}

export interface LocaleGlossStoreBuild {
  readonly bytes: Uint8Array;
  readonly locale: string;
  readonly lexiconSha256: string;
  readonly stats: IndexedGzipStoreStats & {
    readonly translatedEntryCount: number;
    readonly groupCount: number;
    readonly targetCount: number;
    readonly glossCount: number;
    readonly infoCount: number;
  };
}

export class LocaleGlossStoreEncodingError extends BinaryStoreEncodingError {
  constructor(message: string) {
    super(message);
    this.name = 'LocaleGlossStoreEncodingError';
  }
}

function sha256Bytes(value: string): Uint8Array {
  if (!HEX_SHA256.test(value)) {
    throw new LocaleGlossStoreEncodingError('Lexicon digest must be a lowercase SHA-256');
  }
  const bytes = new Uint8Array(32);
  for (let index = 0; index < bytes.length; index++) {
    bytes[index] = Number.parseInt(value.slice(index * 2, index * 2 + 2), 16);
  }
  return bytes;
}

function localeBytes(locale: string): Uint8Array {
  if (!LOCALE_TAG.test(locale)) {
    throw new LocaleGlossStoreEncodingError(`Invalid locale tag ${JSON.stringify(locale)}`);
  }
  const bytes = UTF8_ENCODER.encode(locale);
  if (bytes.byteLength > LOCALE_GLOSS_LOCALE_MAX_BYTES) {
    throw new LocaleGlossStoreEncodingError('Locale tag exceeds 31 UTF-8 bytes');
  }
  return bytes;
}

function compareTargets(left: readonly number[], right: readonly number[]): number {
  const shared = Math.min(left.length, right.length);
  for (let index = 0; index < shared; index++) {
    const difference = left[index]! - right[index]!;
    if (difference !== 0) return difference;
  }
  return left.length - right.length;
}

function validateTexts(
  seq: number,
  groupIndex: number,
  label: string,
  texts: readonly LocaleGlossTextSource[]
): void {
  let previousOrdinal = -1;
  for (const value of texts) {
    if (value.ord <= previousOrdinal) {
      throw new LocaleGlossStoreEncodingError(
        `Entry ${seq} group ${groupIndex} ${label} are not ordered`
      );
    }
    if (value.text.length === 0) {
      throw new LocaleGlossStoreEncodingError(
        `Entry ${seq} group ${groupIndex} has an empty ${label} string`
      );
    }
    previousOrdinal = value.ord;
  }
}

function encodeEntry(entry: LocaleGlossEntrySource): Uint8Array {
  const writer = new BinaryWriter();
  writer.uint(entry.seq, 'Entry sequence');
  writer.uint(entry.groups.length, 'Gloss group count');
  let previousTargets: readonly number[] | null = null;
  const claimedTargets = new Set<number>();
  for (let groupIndex = 0; groupIndex < entry.groups.length; groupIndex++) {
    const group = entry.groups[groupIndex]!;
    if (group.glosses.length === 0 && group.info.length === 0) {
      throw new LocaleGlossStoreEncodingError(
        `Entry ${entry.seq} group ${groupIndex} has no localized text`
      );
    }
    if (group.targets.length === 0 && entry.groups.length !== 1) {
      throw new LocaleGlossStoreEncodingError(
        `Entry ${entry.seq} mixes an entry-wide gloss group with aligned groups`
      );
    }
    let previousTarget = -1;
    for (const target of group.targets) {
      if (target <= previousTarget || claimedTargets.has(target)) {
        throw new LocaleGlossStoreEncodingError(
          `Entry ${entry.seq} group ${groupIndex} has duplicate or unordered targets`
        );
      }
      previousTarget = target;
      claimedTargets.add(target);
    }
    if (previousTargets !== null && compareTargets(previousTargets, group.targets) >= 0) {
      throw new LocaleGlossStoreEncodingError(`Entry ${entry.seq} gloss groups are not ordered`);
    }
    previousTargets = group.targets;
    validateTexts(entry.seq, groupIndex, 'glosses', group.glosses);
    validateTexts(entry.seq, groupIndex, 'info strings', group.info);

    writer.uint(group.targets.length, 'Target sense count');
    for (const target of group.targets) writer.uint(target, 'Target sense ordinal');
    writer.uint(group.glosses.length, 'Gloss count');
    for (const gloss of group.glosses) {
      writer.uint(gloss.ord, 'Gloss ordinal');
      writer.text(gloss.text);
    }
    writer.uint(group.info.length, 'Info count');
    for (const info of group.info) {
      writer.uint(info.ord, 'Info ordinal');
      writer.text(info.text);
    }
  }
  return writer.finish();
}

export function buildLocaleGlossStore(options: {
  readonly locale: string;
  readonly lexiconSha256: string;
  readonly entries: readonly LocaleGlossEntrySource[];
  readonly targetBlockBytes?: number;
}): LocaleGlossStoreBuild {
  const digest = sha256Bytes(options.lexiconSha256);
  const encodedLocale = localeBytes(options.locale);
  if (options.entries.length === 0) {
    throw new LocaleGlossStoreEncodingError('Locale gloss store requires at least one entry');
  }
  const entries = [...options.entries].sort((left, right) => left.seq - right.seq);
  for (let index = 1; index < entries.length; index++) {
    if (entries[index - 1]!.seq === entries[index]!.seq) {
      throw new LocaleGlossStoreEncodingError(`Duplicate root sequence ${entries[index]!.seq}`);
    }
  }
  const store = buildIndexedGzipStore({
    magic: LOCALE_GLOSS_MAGIC,
    formatVersion: LOCALE_GLOSS_FORMAT_VERSION,
    headerBytes: LOCALE_GLOSS_HEADER_BYTES,
    records: entries.map(encodeEntry),
    ...(options.targetBlockBytes === undefined ? {} : {
      targetBlockBytes: options.targetBlockBytes
    }),
    writeHeaderExtension(bytes) {
      bytes.set(digest, LOCALE_GLOSS_LEXICON_SHA256_OFFSET);
      bytes[LOCALE_GLOSS_LOCALE_LENGTH_OFFSET] = encodedLocale.byteLength;
      bytes.set(encodedLocale, LOCALE_GLOSS_LOCALE_OFFSET);
    }
  });
  return {
    bytes: store.bytes,
    locale: options.locale,
    lexiconSha256: options.lexiconSha256,
    stats: {
      ...store.stats,
      translatedEntryCount: entries.reduce(
        (count, entry) => count + Number(entry.groups.length > 0), 0
      ),
      groupCount: entries.reduce((count, entry) => count + entry.groups.length, 0),
      targetCount: entries.reduce((count, entry) => count + entry.groups.reduce(
        (inner, group) => inner + group.targets.length, 0
      ), 0),
      glossCount: entries.reduce((count, entry) => count + entry.groups.reduce(
        (inner, group) => inner + group.glosses.length, 0
      ), 0),
      infoCount: entries.reduce((count, entry) => count + entry.groups.reduce(
        (inner, group) => inner + group.info.length, 0
      ), 0)
    }
  };
}
