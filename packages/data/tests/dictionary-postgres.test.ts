import { createHash } from 'node:crypto';
import { gunzipSync, gzipSync } from 'node:zlib';
import { expect, test } from 'bun:test';
import postgres from 'postgres';
import {
  DictionaryReader,
  LexiconStoreReader,
  LocaleGlossStoreReader,
  localizeEntry,
  memoryDictionarySource
} from '../../core/src/dictionary.js';
import { loadDictionaryEntries } from '../src/browser-pack/dictionary-oracle.js';
import { buildLexiconStore } from '../src/browser-pack/lexicon.js';
import { buildLocaleGlossStore } from '../src/browser-pack/locale-gloss.js';

const RUN_POSTGRES_TEST = process.env.RUN_DICTIONARY_POSTGRES === 'true';

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

const decodeGzip = async (bytes: Uint8Array): Promise<Uint8Array> =>
  new Uint8Array(gunzipSync(bytes));

test.skipIf(!RUN_POSTGRES_TEST)(
  'split dictionary stores exhaustively equal every root form and sense in ichiran_test',
  async () => {
    const sql = postgres({
      host: process.env.DICTIONARY_DATABASE_HOST ?? '/var/run/postgresql',
      database: process.env.DICTIONARY_DATABASE_NAME ?? 'ichiran_test',
      user: process.env.DICTIONARY_DATABASE_USER ?? 'tiger',
      password: process.env.DICTIONARY_DATABASE_PASSWORD ?? '',
      max: 5
    });
    try {
      const source = await loadDictionaryEntries(sql);
      const lexicon = buildLexiconStore(source.lexicon);
      const lexiconSha256 = sha256(lexicon.bytes);
      const english = buildLocaleGlossStore({
        locale: 'en', lexiconSha256, entries: source.english
      });
      const lexiconReader = await LexiconStoreReader.open(
        memoryDictionarySource(lexicon.bytes), decodeGzip
      );
      const englishReader = await LocaleGlossStoreReader.open(
        memoryDictionarySource(english.bytes),
        decodeGzip,
        { locale: 'en', lexiconSha256, entryCount: source.lexicon.length }
      );
      const dictionary = new DictionaryReader(lexiconReader, englishReader, englishReader);

      expect(source.lexicon).toHaveLength(213_732);
      expect(lexicon.stats.entryCount).toBe(213_732);
      expect(lexicon.stats.formCount).toBe(480_480);
      expect(english.stats.entryCount).toBe(source.lexicon.length);
      for (let entryIndex = 0; entryIndex < source.lexicon.length; entryIndex++) {
        const expected = localizeEntry(
          source.lexicon[entryIndex]!,
          source.english[entryIndex]!,
          source.english[entryIndex]!
        );
        expect(await dictionary.entry(entryIndex)).toEqual(expected);
      }
      console.log(JSON.stringify({
        lexicon: lexicon.stats,
        english: english.stats,
        transport: {
          lexiconGzip9Bytes: gzipSync(lexicon.bytes, { level: 9 }).byteLength,
          englishGzip9Bytes: gzipSync(english.bytes, { level: 9 }).byteLength
        }
      }, null, 2));
    } finally {
      await sql.end();
    }
  },
  300_000
);
