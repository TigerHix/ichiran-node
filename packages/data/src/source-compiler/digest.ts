import { createHash } from 'node:crypto';
import type { CanonicalEntry } from './model.js';

function semanticValue(entry: CanonicalEntry): unknown {
  return {
    seq: entry.seq,
    kanji: entry.kanji.map(({ sourceOrder: _sourceOrder, ...form }) => form),
    kana: entry.kana.map(({ sourceOrder: _sourceOrder, ...form }) => form),
    senses: entry.senses.map(sense => ({
      ...sense,
      properties: sense.properties.map(({ sourceOrder: _sourceOrder, ...property }) => property)
    })),
    restrictions: entry.restrictions,
    primaryNoKanji: entry.primaryNoKanji
  };
}

export function canonicalEntryJson(entry: CanonicalEntry): string {
  return JSON.stringify(semanticValue(entry));
}

export function canonicalEntryDigest(entry: CanonicalEntry): string {
  return createHash('sha256').update(canonicalEntryJson(entry)).digest('hex');
}

export interface CanonicalDigest {
  readonly entries: number;
  readonly sha256: string;
}

export async function canonicalEntriesDigest(entries: AsyncIterable<CanonicalEntry> | Iterable<CanonicalEntry>): Promise<CanonicalDigest> {
  const hash = createHash('sha256');
  let count = 0;
  for await (const entry of entries) {
    const bytes = Buffer.from(canonicalEntryJson(entry));
    const length = Buffer.allocUnsafe(4);
    length.writeUInt32BE(bytes.length);
    hash.update(length);
    hash.update(bytes);
    count++;
  }
  return { entries: count, sha256: hash.digest('hex') };
}
