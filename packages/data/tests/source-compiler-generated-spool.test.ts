import { describe, expect, test } from 'bun:test';
import {
  closeSync,
  mkdtempSync,
  openSync,
  readFileSync,
  rmSync,
  truncateSync,
  writeFileSync,
  writeSync
} from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import {
  GeneratedProjectionSpoolWriter,
  readGeneratedOccurrenceSpool,
  readGeneratedPathSpool
} from '../src/source-compiler/generated-projection-spool.js';

function temporarySpool(): {
  readonly directory: string;
  readonly paths: string;
  readonly occurrences: string;
} {
  const directory = mkdtempSync(join(tmpdir(), 'ichiran-generated-spool-'));
  return {
    directory,
    paths: join(directory, 'paths.bin'),
    occurrences: join(directory, 'occurrences.bin')
  };
}

describe('generated projection spool', () => {
  test('round-trips dense paths and UTF-8 occurrences', () => {
    const spool = temporarySpool();
    try {
      const writer = new GeneratedProjectionSpoolWriter(spool.paths, spool.occurrences);
      writer.writePath({
        ordinal: 0,
        rootSeq: 1_358_280,
        firstAlias: 17,
        secondAlias: null,
        targetSeq: 3_000_001,
        viaTargetSeq: null
      });
      writer.writeOccurrence({
        pathOrdinal: 0,
        precedence: 0,
        firstRule: 17,
        secondRule: null,
        route: 'kanji',
        kind: 'emission',
        installed: false,
        surface: '食べられる',
        physicalCounterpart: 'たべられる'
      });
      writer.writeOccurrence({
        pathOrdinal: 0,
        precedence: 0,
        firstRule: 17,
        secondRule: null,
        route: 'kana',
        kind: 'emission',
        installed: true,
        surface: 'たべられる',
        physicalCounterpart: '食べられる'
      });
      writer.writePath({
        ordinal: 1,
        rootSeq: 1_358_280,
        firstAlias: 17,
        secondAlias: 42,
        targetSeq: 3_000_002,
        viaTargetSeq: 3_000_001
      });
      writer.writeOccurrence({
        pathOrdinal: 1,
        precedence: 2_470_038,
        firstRule: 17,
        secondRule: 42,
        route: 'kana',
        kind: 'patch',
        installed: true,
        surface: '𠮟られました',
        physicalCounterpart: null
      });

      expect(writer.close()).toEqual({ paths: 2, occurrences: 3, installedOccurrences: 2 });
      expect([...readGeneratedPathSpool(spool.paths)]).toEqual([
        {
          ordinal: 0,
          rootSeq: 1_358_280,
          firstAlias: 17,
          secondAlias: null,
          targetSeq: 3_000_001,
          viaTargetSeq: null
        },
        {
          ordinal: 1,
          rootSeq: 1_358_280,
          firstAlias: 17,
          secondAlias: 42,
          targetSeq: 3_000_002,
          viaTargetSeq: 3_000_001
        }
      ]);
      expect([...readGeneratedOccurrenceSpool(spool.occurrences)]).toEqual([
        {
          pathOrdinal: 0,
          precedence: 0,
          firstRule: 17,
          secondRule: null,
          route: 'kanji',
          kind: 'emission',
          installed: false,
          surface: '食べられる',
          physicalCounterpart: 'たべられる'
        },
        {
          pathOrdinal: 0,
          precedence: 0,
          firstRule: 17,
          secondRule: null,
          route: 'kana',
          kind: 'emission',
          installed: true,
          surface: 'たべられる',
          physicalCounterpart: '食べられる'
        },
        {
          pathOrdinal: 1,
          precedence: 2_470_038,
          firstRule: 17,
          secondRule: 42,
          route: 'kana',
          kind: 'patch',
          installed: true,
          surface: '𠮟られました',
          physicalCounterpart: null
        }
      ]);
    } finally {
      rmSync(spool.directory, { recursive: true, force: true });
    }
  });

  test('rejects non-dense paths and references to unwritten paths', () => {
    const spool = temporarySpool();
    try {
      const writer = new GeneratedProjectionSpoolWriter(spool.paths, spool.occurrences);
      expect(() => writer.writePath({
        ordinal: 1,
        rootSeq: 1,
        firstAlias: 0,
        secondAlias: null,
        targetSeq: 2,
        viaTargetSeq: null
      })).toThrow('is not dense');
      expect(() => writer.writeOccurrence({
        pathOrdinal: 0,
        precedence: 0,
        firstRule: 0,
        secondRule: null,
        route: 'kana',
        kind: 'emission',
        installed: true,
        surface: 'ない',
        physicalCounterpart: null
      })).toThrow('unwritten path');
      writer.abort();
      expect(() => readFileSync(spool.paths)).toThrow();
      expect(() => readFileSync(spool.occurrences)).toThrow();
    } finally {
      rmSync(spool.directory, { recursive: true, force: true });
    }
  });

  test('keeps prefix fields stable when a surface crosses the read buffer boundary', () => {
    const spool = temporarySpool();
    try {
      const writer = new GeneratedProjectionSpoolWriter(spool.paths, spool.occurrences);
      writer.writePath({
        ordinal: 0, rootSeq: 1, firstAlias: 0, secondAlias: null,
        targetSeq: 2, viaTargetSeq: null
      });
      for (let index = 0; index < 240_000; index++) writer.writeOccurrence({
        pathOrdinal: 0,
        precedence: index,
        firstRule: 3,
        secondRule: null,
        route: 'kana',
        kind: 'emission',
        installed: true,
        surface: 'あ',
        physicalCounterpart: index % 2 === 0 ? '亜' : null
      });
      writer.close();
      let rows = 0;
      for (const row of readGeneratedOccurrenceSpool(spool.occurrences)) {
        if (row.pathOrdinal !== 0 || row.precedence !== rows) {
          throw new Error(`Boundary row ${rows} decoded as ${row.pathOrdinal}/${row.precedence}`);
        }
        rows++;
      }
      expect(rows).toBe(240_000);
    } finally {
      rmSync(spool.directory, { recursive: true, force: true });
    }
  });

  test('rejects incompatible headers, truncation and trailing bytes', () => {
    const spool = temporarySpool();
    try {
      const writer = new GeneratedProjectionSpoolWriter(spool.paths, spool.occurrences);
      writer.writePath({
        ordinal: 0,
        rootSeq: 1,
        firstAlias: 0,
        secondAlias: null,
        targetSeq: 2,
        viaTargetSeq: null
      });
      writer.writeOccurrence({
        pathOrdinal: 0,
        precedence: 0,
        firstRule: 0,
        secondRule: null,
        route: 'kana',
        kind: 'emission',
        installed: true,
        surface: 'ない',
        physicalCounterpart: null
      });
      writer.close();

      const versionFd = openSync(spool.paths, 'r+');
      writeSync(versionFd, Buffer.from([3, 0]), 0, 2, 8);
      closeSync(versionFd);
      expect(() => [...readGeneratedPathSpool(spool.paths)]).toThrow('Unsupported');

      const occurrenceBytes = readFileSync(spool.occurrences);
      truncateSync(spool.occurrences, occurrenceBytes.byteLength - 1);
      expect(() => [...readGeneratedOccurrenceSpool(spool.occurrences)]).toThrow('Truncated');

      const trailing = join(spool.directory, 'trailing.bin');
      writeFileSync(trailing, Buffer.concat([occurrenceBytes, Buffer.from([0])]));
      expect(() => [...readGeneratedOccurrenceSpool(trailing)]).toThrow('trailing bytes');
    } finally {
      rmSync(spool.directory, { recursive: true, force: true });
    }
  });
});
