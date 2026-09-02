import { describe, expect, test } from 'bun:test';
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  rmSync,
  writeFileSync
} from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import {
  GeneratedProjectionSpoolWriter,
  writeGeneratedOccurrenceSpool
} from '../src/source-compiler/generated-projection-spool.js';
import { writeBoundedSurfaceIndexTsv } from '../src/source-compiler/surface-index-spool.js';
import type { CanonicalEntry, CanonicalForm } from '../src/source-compiler/model.js';

function form(text: string, ordinal: number): CanonicalForm {
  return {
    text,
    ordinal,
    sourceOrder: { event: 0, ordinal },
    common: null,
    priorityTags: [],
    conjugatable: false,
    noKanji: false,
    best: null
  };
}

function entry(): CanonicalEntry {
  return {
    seq: 1,
    source: { sourceId: 'surface-index-spool-test', ordinal: 0 },
    kanji: [form('\uE000', 0), form('𠀀', 1)],
    kana: [form('あ', 0), form('漢', 1)],
    senses: [],
    restrictions: [],
    primaryNoKanji: false
  };
}

function temporaryDirectory(): string {
  return mkdtempSync(join(tmpdir(), 'ichiran-surface-spool-'));
}

describe('bounded surface-index TSV', () => {
  test('merges direct and morphology flags in strict UTF-8 byte order', async () => {
    const directory = temporaryDirectory();
    const occurrences = join(directory, 'occurrences.bin');
    const paths = join(directory, 'paths.bin');
    const destination = join(directory, 'surface.tsv');
    try {
      const writer = new GeneratedProjectionSpoolWriter(paths, occurrences);
      writer.writePath({
        ordinal: 0, rootSeq: 9, firstAlias: 0, secondAlias: null,
        targetSeq: 2, viaTargetSeq: null
      });
      writer.writePath({
        ordinal: 1, rootSeq: 10, firstAlias: 1, secondAlias: null,
        targetSeq: 3, viaTargetSeq: null
      });
      for (const row of [
        {
          pathOrdinal: 0, precedence: 0, firstRule: 0, secondRule: null,
          route: 'kana', kind: 'emission', installed: false,
          surface: '食べた', physicalCounterpart: '語'
        },
        {
          pathOrdinal: 0, precedence: 0, firstRule: 0, secondRule: null,
          route: 'kana', kind: 'emission', installed: true,
          surface: 'あ', physicalCounterpart: null
        },
        {
          pathOrdinal: 0, precedence: 0, firstRule: 0, secondRule: null,
          route: 'kanji', kind: 'emission', installed: true,
          surface: '𠀀', physicalCounterpart: null
        },
        {
          pathOrdinal: 0, precedence: 0, firstRule: 0, secondRule: null,
          route: 'kanji', kind: 'patch', installed: true,
          surface: '食べた', physicalCounterpart: null
        }
      ]) writer.writeOccurrence(row);
      writer.close();
      const summary = await writeBoundedSurfaceIndexTsv({
        entries: [entry()],
        physicalTargets: [
          {
            seq: 1, kanji: ['\uE000', '𠀀'], kana: ['あ'],
            secondaryForms: [],
            conjugatable: false, origin: 'lexical'
          },
          {
            seq: 2, kanji: ['食べた'], kana: [],
            secondaryForms: [],
            conjugatable: false, origin: 'generated'
          },
          {
            seq: 3, kanji: [], kana: ['語'],
            secondaryForms: [],
            conjugatable: false, origin: 'generated'
          }
        ],
        occurrencesPath: occurrences,
        temporaryDirectory: directory,
        destination,
        maxChunkRows: 2
      });
      const expected = 'あ\t1\t1\t0\t0\n'
        + '漢\t0\t0\t0\t0\n'
        + '食べた\t0\t0\t0\t1\n'
        + '\uE000\t0\t0\t1\t0\n'
        + '𠀀\t0\t0\t1\t1\n';
      expect(readFileSync(destination, 'utf8')).toBe(expected);
      expect(summary).toEqual({
        inputRows: 13,
        surfaces: 5,
        direct: 3,
        morphology: 3,
        overlap: 2,
        chunks: 7,
        bytes: Buffer.byteLength(expected)
      });
      expect(readdirSync(directory).sort()).toEqual(['occurrences.bin', 'paths.bin', 'surface.tsv']);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });

  test('rejects invalid TSV surfaces and preserves an existing destination', async () => {
    const directory = temporaryDirectory();
    try {
      const invalid = join(directory, 'invalid.bin');
      writeGeneratedOccurrenceSpool(invalid, [{
        pathOrdinal: 0, precedence: 0, firstRule: 0, secondRule: null,
        route: 'kana', kind: 'emission', installed: true,
        surface: 'bad\nrow', physicalCounterpart: null
      }]);
      const destination = join(directory, 'surface.tsv');
      const emptyPaths = join(directory, 'paths.bin');
      const sidecar = join(directory, 'empty-occurrences.bin');
      new GeneratedProjectionSpoolWriter(emptyPaths, sidecar).close();
      await expect(writeBoundedSurfaceIndexTsv({
        entries: [], physicalTargets: [], occurrencesPath: invalid,
        temporaryDirectory: directory, destination
      })).rejects.toThrow('invalid TSV surface');
      expect(existsSync(destination)).toBe(false);

      const valid = join(directory, 'valid.bin');
      writeGeneratedOccurrenceSpool(valid, [{
        pathOrdinal: 0, precedence: 0, firstRule: 0, secondRule: null,
        route: 'kana', kind: 'emission', installed: true,
        surface: 'あ', physicalCounterpart: null
      }]);
      writeFileSync(destination, 'keep');
      await expect(writeBoundedSurfaceIndexTsv({
        entries: [], physicalTargets: [], occurrencesPath: valid,
        temporaryDirectory: directory, destination
      })).rejects.toThrow();
      expect(readFileSync(destination, 'utf8')).toBe('keep');
      expect(readdirSync(directory).some(name => name.startsWith('surface-index-'))).toBe(false);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});
