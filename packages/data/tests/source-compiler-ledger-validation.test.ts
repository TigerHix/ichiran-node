import { describe, expect, test } from 'bun:test';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  loadQualifiedErrata
} from '../src/source-compiler/chronological-errata.js';
import {
  applyCanonicalCompatibility,
  loadSourceCompatibility,
  type SourceCompatibilityLedger
} from '../src/source-compiler/compatibility.js';
import type { CanonicalEntry } from '../src/source-compiler/model.js';
import { loadKanjidicHintReadings } from '../src/source-compiler/kanjidic-hints.js';
import { canonicalMorphologySource } from '../src/source-compiler/morphology-input.js';

const ERRATA = fileURLToPath(new URL('../../../data/source-compiler-errata.json', import.meta.url));
const COMPATIBILITY = fileURLToPath(new URL('../../../data/source-compiler-compatibility.json', import.meta.url));

async function withJson<T>(value: unknown, run: (path: string) => Promise<T>): Promise<T> {
  const directory = await mkdtemp(join(tmpdir(), 'ichiran-ledger-validation-'));
  const path = join(directory, 'ledger.json');
  try {
    await writeFile(path, JSON.stringify(value));
    return await run(path);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
}

function compatibilityRow(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    id: 'canonical-test',
    kind: 'canonical-sense-property',
    seq: 1,
    senseOrdinal: 0,
    tag: 'misc',
    text: 'uk',
    provenance: { source: 'test' },
    preservedBehavior: 'Preserve the test behavior.',
    ...overrides
  };
}

const AUTHORITY = {
  upstreamRepository: 'https://example.test/upstream.git',
  upstreamCommit: '0123456789abcdef0123456789abcdef01234567',
  upstreamPath: 'dict-errata.lisp',
  upstreamSha256: '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
  migrationPortPath: 'packages/data/src/data/errata.ts'
};

function errataRow(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    event: 0,
    phase: 'addErrata',
    operation: 'setPrimaryNokanji',
    arguments: [1, false],
    sourceLine: 1,
    preservedBehavior: 'Preserve the test behavior.',
    ...overrides
  };
}

describe('strict semantic ledger boundaries', () => {
  test('parses the pinned ledgers into unique stable row identities', async () => {
    const compatibility = await loadSourceCompatibility(COMPATIBILITY);
    const errata = await loadQualifiedErrata(ERRATA);
    expect(new Set(compatibility.rows.map(row => row.id)).size).toBe(25);
    expect(new Set(errata.rows.map(row => row.id)).size).toBe(601);
    expect(errata.rows[0]?.id).toMatch(/^addErrata:1228:conjugateDa:[0-9a-f]{16}$/u);
  });

  test('rejects compatibility kind and field typos instead of dropping them', async () => {
    await expect(withJson({
      formatVersion: 1,
      rows: [compatibilityRow({ kind: 'canonical-sense-proprety' })]
    }, loadSourceCompatibility)).rejects.toThrow('kind is unsupported');
    await expect(withJson({
      formatVersion: 1,
      rows: [compatibilityRow({ senseOridnal: 0 })]
    }, loadSourceCompatibility)).rejects.toThrow('unknown field senseOridnal');
  });

  test('rejects duplicate compatibility ids and semantic identities', async () => {
    await expect(withJson({
      formatVersion: 1,
      rows: [compatibilityRow(), compatibilityRow()]
    }, loadSourceCompatibility)).rejects.toThrow('Duplicate compatibility row id');
    await expect(withJson({
      formatVersion: 1,
      rows: [compatibilityRow(), compatibilityRow({ id: 'same-behavior-new-name' })]
    }, loadSourceCompatibility)).rejects.toThrow('Duplicate compatibility semantic identity');
  });

  test('rejects a stale canonical compatibility row', () => {
    const entry: CanonicalEntry = {
      seq: 1,
      source: { sourceId: 'test', ordinal: 0 },
      kanji: [],
      kana: [],
      senses: [{
        ordinal: 0,
        glosses: [],
        properties: [{
          tag: 'misc', text: 'uk', ordinal: 0, sourceOrder: { event: 0, ordinal: 0 }
        }]
      }],
      restrictions: [],
      primaryNoKanji: true
    };
    const ledger: SourceCompatibilityLedger = {
      formatVersion: 1,
      rows: [compatibilityRow() as unknown as SourceCompatibilityLedger['rows'][number]]
    };
    expect(() => applyCanonicalCompatibility([entry], ledger, 1))
      .toThrow('canonical property already exists');
  });

  test('rejects a stale conjugation-position compatibility row', () => {
    const entry: CanonicalEntry = {
      seq: 1,
      source: { sourceId: 'test', ordinal: 0 },
      kanji: [],
      kana: [{
        text: 'たべる', ordinal: 0, sourceOrder: { event: 0, ordinal: 0 },
        common: null, priorityTags: [], conjugatable: true, noKanji: false, best: null
      }],
      senses: [{
        ordinal: 0,
        glosses: [],
        properties: [{ tag: 'pos', text: 'v1', ordinal: 0, sourceOrder: { event: 0, ordinal: 0 } }]
      }],
      restrictions: [],
      primaryNoKanji: true
    };
    expect(() => canonicalMorphologySource([entry], [{
      id: 'stale-position-witness', seq: entry.seq, pos: 'v1'
    }])).toThrow('stale-position-witness is stale');
  });

  test('rejects a stale Kanjidic compatibility reading', async () => {
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-kanjidic-stale-'));
    const path = join(directory, 'kanjidic2.xml');
    try {
      await writeFile(path, `<?xml version="1.0"?><kanjidic2><character><literal>楊</literal><reading_meaning><rmgroup><reading r_type="ja_kun">かわ</reading></rmgroup></reading_meaning></character></kanjidic2>`);
      await expect(loadKanjidicHintReadings(path, [{
        literal: '楊', reading: 'かわ', type: 'ja_kun', prefix: false, suffix: false
      }])).rejects.toThrow('already exists');
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  test('rejects unknown errata operations, option typos, and invalid ranges', async () => {
    await expect(withJson({
      formatVersion: 1, authority: AUTHORITY, rows: [errataRow({ operation: 'setPrimryNokanji' })]
    }, loadQualifiedErrata)).rejects.toThrow('operation is unsupported');
    await expect(withJson({
      formatVersion: 1,
      authority: AUTHORITY,
      rows: [errataRow({ operation: 'addReading', arguments: [1, 'かな', { conjugate: false }] })]
    }, loadQualifiedErrata)).rejects.toThrow('unknown field conjugate');
    await expect(withJson({
      formatVersion: 1, authority: AUTHORITY, rows: [errataRow({ arguments: [0, false] })]
    }, loadQualifiedErrata)).rejects.toThrow('integer from 1');
  });

  test('rejects duplicate chronological identities even when events differ', async () => {
    await expect(withJson({
      formatVersion: 1,
      authority: AUTHORITY,
      rows: [errataRow(), errataRow({ event: 1 })]
    }, loadQualifiedErrata)).rejects.toThrow('Duplicate qualified errata identity');
  });
});
