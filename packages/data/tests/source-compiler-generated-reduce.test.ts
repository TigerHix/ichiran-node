import { describe, expect, test } from 'bun:test';
import { mkdtempSync, readdirSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import {
  collectGeneratedLocatorsForTargets,
  collectGeneratedRulePathTargets,
  generatedLookupClasses,
  reduceGeneratedPhysicalMembers,
  reduceGeneratedOccurrenceSurfaces,
  reduceGeneratedSemanticPaths,
  type GeneratedOccurrenceSurface,
  type GeneratedPhysicalTargetMembers,
  type GeneratedSemanticPath
} from '../src/source-compiler/generated-projection-reduce.js';
import {
  GeneratedProjectionSpoolWriter,
  writeGeneratedOccurrenceSpool
} from '../src/source-compiler/generated-projection-spool.js';

function temporaryDirectory(): string {
  return mkdtempSync(join(tmpdir(), 'ichiran-generated-reduce-'));
}

describe('generated projection bounded reducers', () => {
  test('chunk-sorts occurrences and reduces target classes deterministically', () => {
    const directory = temporaryDirectory();
    const paths = join(directory, 'paths.bin');
    const occurrences = join(directory, 'occurrences.bin');
    try {
      const writer = new GeneratedProjectionSpoolWriter(paths, occurrences);
      writer.writePath({
        ordinal: 0, rootSeq: 100, firstAlias: 1, secondAlias: null,
        targetSeq: 1000, viaTargetSeq: null
      });
      writer.writePath({
        ordinal: 1, rootSeq: 200, firstAlias: 2, secondAlias: 4,
        targetSeq: 2000, viaTargetSeq: 1900
      });
      writer.writePath({
        ordinal: 2, rootSeq: 300, firstAlias: 3, secondAlias: null,
        targetSeq: 1000, viaTargetSeq: null
      });
      writer.writeOccurrence({
        pathOrdinal: 1, precedence: 9, firstRule: 20, secondRule: 40,
        route: 'kana', kind: 'emission', installed: true,
        surface: 'あ', physicalCounterpart: null
      });
      writer.writeOccurrence({
        pathOrdinal: 0, precedence: 2, firstRule: 10, secondRule: null,
        route: 'kana', kind: 'emission', installed: true,
        surface: 'い', physicalCounterpart: null
      });
      writer.writeOccurrence({
        pathOrdinal: 2, precedence: 8, firstRule: 30, secondRule: null,
        route: 'kana', kind: 'emission', installed: true,
        surface: 'あ', physicalCounterpart: null
      });
      writer.writeOccurrence({
        pathOrdinal: 0, precedence: 2, firstRule: 10, secondRule: null,
        route: 'kana', kind: 'emission', installed: true,
        surface: 'あ', physicalCounterpart: null
      });
      writer.writeOccurrence({
        pathOrdinal: 0, precedence: 20, firstRule: 10, secondRule: null,
        route: 'kana', kind: 'patch', installed: true,
        surface: 'あ', physicalCounterpart: null
      });
      writer.close();

      const surfaces: GeneratedOccurrenceSurface[] = [];
      const summary = reduceGeneratedOccurrenceSurfaces({
        pathsPath: paths,
        occurrencesPath: occurrences,
        temporaryDirectory: directory,
        maxChunkRows: 2
      }, value => surfaces.push(value));
      expect(summary).toEqual({ rows: 5, chunks: 3, surfaces: 2, maxSurfaceRows: 4 });
      expect(surfaces.map(value => [value.route, value.surface])).toEqual([
        ['kana', 'あ'],
        ['kana', 'い']
      ]);
      expect(generatedLookupClasses(surfaces[0]!)).toEqual([
        {
          targetSeq: 1000,
          precedence: 20,
          locators: [
            { rootSeq: 100, firstAlias: 1, secondAlias: null },
            { rootSeq: 300, firstAlias: 3, secondAlias: null }
          ]
        },
        {
          targetSeq: 2000,
          precedence: 9,
          locators: [{ rootSeq: 200, firstAlias: 2, secondAlias: 4 }]
        }
      ]);
      expect(readdirSync(directory).sort()).toEqual(['occurrences.bin', 'paths.bin']);
      expect(() => reduceGeneratedOccurrenceSurfaces({
        pathsPath: paths,
        occurrencesPath: occurrences,
        temporaryDirectory: directory,
        maxChunkRows: 2
      }, () => {
        throw new Error('stop after first surface');
      })).toThrow('stop after first surface');
      expect(readdirSync(directory).sort()).toEqual(['occurrences.bin', 'paths.bin']);

      expect(collectGeneratedRulePathTargets(
        paths,
        occurrences,
        new Set([100, 200])
      )).toEqual([
        { rootSeq: 100, firstRule: 10, secondRule: null, targetSeq: 1000, viaTargetSeq: null },
        { rootSeq: 200, firstRule: 20, secondRule: 40, targetSeq: 2000, viaTargetSeq: 1900 }
      ]);
      const properties = [
        { pos: 'v1', type: 0, negative: null, formal: null },
        { pos: 'v1', type: 1, negative: false, formal: false },
        { pos: 'v1', type: 2, negative: false, formal: false },
        { pos: 'v5r', type: 3, negative: false, formal: false },
        { pos: 'v1', type: 4, negative: true, formal: false }
      ];
      expect(collectGeneratedLocatorsForTargets(
        paths,
        new Set([1000, 2000]),
        properties
      )).toEqual(new Map([
        [1000, [
          { from: 100, via: false, ...properties[1]! },
          { from: 300, via: false, ...properties[3]! }
        ]],
        [2000, [{ from: 200, via: true, ...properties[4]! }]]
      ]));

      const physical = new Map<number, GeneratedPhysicalTargetMembers>();
      expect(reduceGeneratedPhysicalMembers(paths, value => {
        physical.set(value.targetSeq, value);
      })).toEqual({
        paths: 3,
        targets: 2,
        members: 3,
        properties: 3,
        maxTargetPaths: 2
      });
      expect(physical.get(1000)).toEqual({
        targetSeq: 1000,
        paths: 2,
        members: [
          {
            rootSeq: 100,
            targetSeq: 1000,
            viaTargetSeq: null,
            memberOrd: 0,
            firstOrdinal: 0,
            properties: [{ alias: 1, propOrd: 0, firstOrdinal: 0 }]
          },
          {
            rootSeq: 300,
            targetSeq: 1000,
            viaTargetSeq: null,
            memberOrd: 1,
            firstOrdinal: 2,
            properties: [{ alias: 3, propOrd: 0, firstOrdinal: 2 }]
          }
        ]
      });

      const semantic: GeneratedSemanticPath[] = [];
      const semanticSummary = reduceGeneratedSemanticPaths(paths, value => semantic.push(value));
      expect(semantic.map(value => value.rootSeq)).toEqual([100, 200, 300]);
      expect(semanticSummary).toEqual({
        paths: 3,
        roots: 3,
        rootTargets: 3,
        sha256: 'b605df0a223ce767391b05687bff7905d29cec16d17117562ac35359d2cc7cba'
      });
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });

  test('rejects an occurrence with no path before sorting', () => {
    const directory = temporaryDirectory();
    const paths = join(directory, 'paths.bin');
    const occurrences = join(directory, 'occurrences.bin');
    try {
      const writer = new GeneratedProjectionSpoolWriter(paths, join(directory, 'valid.bin'));
      writer.writePath({
        ordinal: 0, rootSeq: 100, firstAlias: 1, secondAlias: null,
        targetSeq: 1000, viaTargetSeq: null
      });
      writer.close();
      writeGeneratedOccurrenceSpool(occurrences, [{
        pathOrdinal: 1,
        precedence: 0,
        firstRule: 0,
        secondRule: null,
        route: 'kana',
        kind: 'emission',
        installed: true,
        surface: 'ない',
        physicalCounterpart: null
      }]);
      expect(() => reduceGeneratedOccurrenceSurfaces({
        pathsPath: paths,
        occurrencesPath: occurrences,
        temporaryDirectory: directory
      }, () => {})).toThrow('missing path 1');
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});
