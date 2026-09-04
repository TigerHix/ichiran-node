import {
  BinaryStoreEncodingError,
  BinaryWriter,
  buildIndexedGzipStore,
  compareBinaryText,
  type IndexedGzipStoreStats
} from './indexed-gzip-store.js';

export const LEXICON_MAGIC = 'ICHILEXI';
export const LEXICON_FORMAT_VERSION = 1;
export const LEXICON_HEADER_BYTES = 96;

export const LEXICON_PROPERTY_TAGS = [
  'dial', 'field', 'misc', 'pos', 'stagk', 'stagr'
] as const;
const PROPERTY_TAG_IDS = new Map<string, number>(
  LEXICON_PROPERTY_TAGS.map((tag, index) => [tag, index])
);

export type LexiconPropertyTag = typeof LEXICON_PROPERTY_TAGS[number];

export interface LexiconPropertySource {
  readonly tag: LexiconPropertyTag;
  readonly ord: number;
  readonly text: string;
}

export interface LexiconSenseSource {
  readonly ord: number;
  readonly properties: readonly LexiconPropertySource[];
}

export interface LexiconFormSource {
  readonly route: 'kanji' | 'kana';
  readonly text: string;
  readonly ord: number;
  readonly common: number | null;
  readonly commonTags: string;
  readonly conjugatable: boolean;
  readonly nokanji: boolean;
  readonly best: string | null;
}

export interface LexiconEntrySource {
  readonly seq: number;
  readonly forms: readonly LexiconFormSource[];
  readonly senses: readonly LexiconSenseSource[];
}

export interface LexiconStoreBuild {
  readonly bytes: Uint8Array;
  readonly stats: IndexedGzipStoreStats & {
    readonly formCount: number;
    readonly senseCount: number;
    readonly propertyCount: number;
  };
}

export class LexiconStoreEncodingError extends BinaryStoreEncodingError {
  constructor(message: string) {
    super(message);
    this.name = 'LexiconStoreEncodingError';
  }
}

function encodeEntry(entry: LexiconEntrySource): Uint8Array {
  const writer = new BinaryWriter();
  writer.uint(entry.seq, 'Entry sequence');
  writer.uint(entry.forms.length, 'Form count');
  let previousForm: LexiconFormSource | null = null;
  for (const form of entry.forms) {
    if (form.ord < 0 || form.common !== null && form.common < 0) {
      throw new LexiconStoreEncodingError(
        `Entry ${entry.seq} has an invalid form ordinal/common rank`
      );
    }
    if (previousForm) {
      const routeOrder = (previousForm.route === 'kanji' ? 0 : 1)
        - (form.route === 'kanji' ? 0 : 1);
      if (
        routeOrder > 0
        || (routeOrder === 0 && previousForm.ord > form.ord)
        || (routeOrder === 0 && previousForm.ord === form.ord
          && compareBinaryText(previousForm.text, form.text) >= 0)
      ) {
        throw new LexiconStoreEncodingError(`Entry ${entry.seq} forms are not canonically ordered`);
      }
    }
    previousForm = form;
    writer.byte(
      (form.route === 'kana' ? 1 : 0)
      | (form.conjugatable ? 1 << 1 : 0)
      | (form.nokanji ? 1 << 2 : 0)
      | (form.best !== null ? 1 << 3 : 0),
      'Form flags'
    );
    writer.uint(form.ord, 'Form ordinal');
    writer.uint(form.common === null ? 0 : form.common + 1, 'Form common rank');
    writer.text(form.text);
    writer.text(form.commonTags);
    if (form.best !== null) writer.text(form.best);
  }

  writer.uint(entry.senses.length, 'Sense count');
  let previousSenseOrdinal = -1;
  for (const sense of entry.senses) {
    if (sense.ord <= previousSenseOrdinal) {
      throw new LexiconStoreEncodingError(`Entry ${entry.seq} senses are not ordered`);
    }
    previousSenseOrdinal = sense.ord;
    writer.uint(sense.ord, 'Sense ordinal');
    writer.uint(sense.properties.length, 'Property count');
    let previousProperty: LexiconPropertySource | null = null;
    for (const property of sense.properties) {
      const tagId = PROPERTY_TAG_IDS.get(property.tag);
      if (tagId === undefined) {
        throw new LexiconStoreEncodingError(`Unknown sense-property tag ${property.tag}`);
      }
      if (previousProperty) {
        const tagOrder = compareBinaryText(previousProperty.tag, property.tag);
        if (tagOrder > 0 || (tagOrder === 0 && previousProperty.ord > property.ord)) {
          throw new LexiconStoreEncodingError(
            `Entry ${entry.seq} properties are not canonically ordered`
          );
        }
      }
      previousProperty = property;
      writer.byte(tagId, 'Property tag ID');
      writer.uint(property.ord, 'Property ordinal');
      writer.text(property.text);
    }
  }
  return writer.finish();
}

export function buildLexiconStore(
  sourceEntries: readonly LexiconEntrySource[],
  options: { readonly targetBlockBytes?: number } = {}
): LexiconStoreBuild {
  if (sourceEntries.length === 0) {
    throw new LexiconStoreEncodingError('Lexicon store requires at least one root entry');
  }
  const entries = sourceEntries.map(entry => ({
    ...entry,
    forms: [...entry.forms].sort((left, right) =>
      (left.route === 'kanji' ? 0 : 1) - (right.route === 'kanji' ? 0 : 1)
      || left.ord - right.ord
      || compareBinaryText(left.text, right.text)
    ),
    senses: entry.senses.map(sense => ({
      ...sense,
      properties: [...sense.properties]
    }))
  })).sort((left, right) => left.seq - right.seq);
  for (let index = 1; index < entries.length; index++) {
    if (entries[index - 1]!.seq === entries[index]!.seq) {
      throw new LexiconStoreEncodingError(`Duplicate root sequence ${entries[index]!.seq}`);
    }
  }

  const store = buildIndexedGzipStore({
    magic: LEXICON_MAGIC,
    formatVersion: LEXICON_FORMAT_VERSION,
    headerBytes: LEXICON_HEADER_BYTES,
    records: entries.map(encodeEntry),
    ...(options.targetBlockBytes === undefined ? {} : {
      targetBlockBytes: options.targetBlockBytes
    })
  });
  return {
    bytes: store.bytes,
    stats: {
      ...store.stats,
      formCount: entries.reduce((sum, entry) => sum + entry.forms.length, 0),
      senseCount: entries.reduce((sum, entry) => sum + entry.senses.length, 0),
      propertyCount: entries.reduce(
        (sum, entry) => sum + entry.senses.reduce(
          (inner, sense) => inner + sense.properties.length, 0
        ), 0
      )
    }
  };
}
