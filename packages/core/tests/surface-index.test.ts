import { describe, expect, test } from 'bun:test';
import { fileURLToPath } from 'node:url';
import {
  SurfaceIndex,
  SurfaceIndexFormatError,
  surfaceRoute,
  SURFACE_INDEX_HEADER_BYTES,
  SURFACE_INDEX_STATE_BYTES
} from '../src/compiler.js';

interface FixtureRow {
  surface: string;
  kanaDirect: boolean;
  kanaMorphology: boolean;
  kanjiDirect: boolean;
  kanjiMorphology: boolean;
}

interface BruteMatch {
  surface: string;
  route: 'kana' | 'kanji';
  direct: boolean;
  morphology: boolean;
  directRank: number | null;
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();
const compilerManifest = fileURLToPath(
  new URL('../../data/tools/surface-index/Cargo.toml', import.meta.url)
);

const fixtureRows: FixtureRow[] = [
  { surface: 'a', kanaDirect: false, kanaMorphology: false, kanjiDirect: true, kanjiMorphology: false },
  { surface: 'ab', kanaDirect: false, kanaMorphology: false, kanjiDirect: false, kanjiMorphology: true },
  { surface: 'b', kanaDirect: false, kanaMorphology: false, kanjiDirect: true, kanjiMorphology: true },
  { surface: 'あ', kanaDirect: true, kanaMorphology: false, kanjiDirect: false, kanjiMorphology: false },
  { surface: 'あい', kanaDirect: false, kanaMorphology: true, kanjiDirect: false, kanjiMorphology: false },
  { surface: 'かな', kanaDirect: true, kanaMorphology: true, kanjiDirect: true, kanjiMorphology: false },
  // Wrong-table-only rows are present in the SQL union but unreachable by current lookup.
  { surface: 'だけ', kanaDirect: false, kanaMorphology: false, kanjiDirect: true, kanjiMorphology: false },
  { surface: 'カナ', kanaDirect: true, kanaMorphology: false, kanjiDirect: false, kanjiMorphology: false },
  { surface: 'ー', kanaDirect: false, kanaMorphology: true, kanjiDirect: false, kanjiMorphology: false },
  { surface: '字', kanaDirect: true, kanaMorphology: false, kanjiDirect: false, kanjiMorphology: false },
  { surface: '漢', kanaDirect: false, kanaMorphology: true, kanjiDirect: true, kanjiMorphology: false },
  { surface: '漢じ', kanaDirect: false, kanaMorphology: false, kanjiDirect: false, kanjiMorphology: true }
];

function compareUtf8(left: string, right: string): number {
  const leftBytes = encoder.encode(left);
  const rightBytes = encoder.encode(right);
  const shared = Math.min(leftBytes.length, rightBytes.length);
  for (let index = 0; index < shared; index++) {
    if (leftBytes[index] !== rightBytes[index]) return leftBytes[index]! - rightBytes[index]!;
  }
  return leftBytes.length - rightBytes.length;
}

function fixtureTsv(rows = fixtureRows): Uint8Array {
  const text = [...rows]
    .sort((left, right) => compareUtf8(left.surface, right.surface))
    .map((row) => [
      row.surface,
      Number(row.kanaDirect),
      Number(row.kanaMorphology),
      Number(row.kanjiDirect),
      Number(row.kanjiMorphology)
    ].join('\t'))
    .join('\n') + '\n';
  return encoder.encode(text);
}

function compile(rows = fixtureRows): Uint8Array {
  const result = Bun.spawnSync([
    'cargo',
    'run',
    '--quiet',
    '--release',
    '--manifest-path',
    compilerManifest
  ], {
    stdin: fixtureTsv(rows),
    stdout: 'pipe',
    stderr: 'pipe'
  });
  if (result.exitCode !== 0) {
    throw new Error(`surface compiler failed: ${decoder.decode(result.stderr)}`);
  }
  return result.stdout;
}

function bruteFixture(): BruteMatch[] {
  const active = fixtureRows.flatMap((row): BruteMatch[] => {
    const route = surfaceRoute(row.surface);
    const direct = route === 'kana' ? row.kanaDirect : row.kanjiDirect;
    const morphology = route === 'kana' ? row.kanaMorphology : row.kanjiMorphology;
    return direct || morphology
      ? [{ surface: row.surface, route, direct, morphology, directRank: null }]
      : [];
  }).sort((left, right) => compareUtf8(left.surface, right.surface));

  let rank = 0;
  for (const match of active) {
    if (match.direct) match.directRank = rank++;
  }
  return active;
}

function expectFormatError(bytes: Uint8Array): void {
  expect(() => new SurfaceIndex(bytes)).toThrow(SurfaceIndexFormatError);
}

describe('route-aware surface index', () => {
  test('matches a brute-force model exhaustively, including direct rank and select', () => {
    const bytes = compile();
    const index = new SurfaceIndex(bytes);
    const brute = bruteFixture();
    const direct = brute.filter((match) => match.direct);
    const morphology = brute.filter((match) => match.morphology);

    expect(index.manifest.inputCount).toBe(fixtureRows.length);
    expect(index.manifest.acceptedCount).toBe(brute.length);
    expect(index.manifest.directCount).toBe(direct.length);
    expect(index.manifest.morphologyCount).toBe(brute.filter((match) => match.morphology).length);
    expect(index.manifest.overlapCount).toBe(
      brute.filter((match) => match.direct && match.morphology).length
    );

    for (const expected of brute) {
      const actual = index.lookup(expected.surface);
      expect(actual).toEqual({
        end: expected.surface.length,
        route: expected.route,
        direct: expected.direct,
        morphology: expected.morphology,
        directRank: expected.directRank
      });
    }
    for (const omitted of fixtureRows.filter((row) => !brute.some((item) => item.surface === row.surface))) {
      expect(index.lookup(omitted.surface)).toBeNull();
    }
    for (const absent of ['', 'aa', 'いう', '仮名', '漢字']) {
      expect(index.lookup(absent)).toBeNull();
    }

    expect(direct.map((match) => index.directSurface(match.directRank!)))
      .toEqual(direct.map((match) => match.surface));
    expect(() => index.directSurface(-1)).toThrow(SurfaceIndexFormatError);
    expect(() => index.directSurface(direct.length)).toThrow(SurfaceIndexFormatError);
  });

  test('scan endpoints, flags, ranks, and routes equal brute-force prefix lookup', () => {
    const index = new SurfaceIndex(compile());
    const brute = bruteFixture();
    const text = 'あいう漢じbカナ';

    for (let start = 0; start <= text.length; start++) {
      const expected = brute
        .filter((match) => text.startsWith(match.surface, start) && match.surface.length <= 50)
        .sort((left, right) => left.surface.length - right.surface.length)
        .map((match) => ({
          end: start + match.surface.length,
          route: match.route,
          direct: match.direct,
          morphology: match.morphology,
          directRank: match.directRank
        }));
      expect(index.scan(text, start)).toEqual(expected);
    }

    expect(index.scan('あいう', 0, 1).map((match) => match.end)).toEqual([1]);
    expect(() => index.scan(text, -1)).toThrow(SurfaceIndexFormatError);
    expect(() => index.scan(text, 0, 0)).toThrow(SurfaceIndexFormatError);
  });

  test('uses the analyzer kana ranges exactly', () => {
    for (const surface of ['あ', 'ゔゝゞー', 'ァヺヽヾー', 'かなカナ']) {
      expect(surfaceRoute(surface)).toBe('kana');
    }
    for (const surface of ['', '漢', 'かな。', 'ｶﾅ', '〇', '🙂']) {
      expect(surfaceRoute(surface)).toBe('kanji');
    }
  });

  test('compiler output is deterministic and rejects unsorted input', () => {
    expect(compile()).toEqual(compile());

    const unsorted = [fixtureRows[1]!, fixtureRows[0]!];
    const result = Bun.spawnSync([
      'cargo',
      'run',
      '--quiet',
      '--release',
      '--manifest-path',
      compilerManifest
    ], {
      stdin: encoder.encode(unsorted.map((row) => [
        row.surface,
        Number(row.kanaDirect),
        Number(row.kanaMorphology),
        Number(row.kanjiDirect),
        Number(row.kanjiMorphology)
      ].join('\t')).join('\n') + '\n'),
      stdout: 'pipe',
      stderr: 'pipe'
    });
    expect(result.exitCode).not.toBe(0);
    expect(decoder.decode(result.stderr)).toContain('not strictly UTF-8 bytewise sorted');
  });

  test('rejects header, state-sentinel, and edge corruption', () => {
    const encoded = compile();

    const badMagic = encoded.slice();
    badMagic[0] ^= 0xff;
    expectFormatError(badMagic);

    const badVersion = encoded.slice();
    new DataView(badVersion.buffer).setUint16(8, 2, true);
    expectFormatError(badVersion);

    const badSentinel = encoded.slice();
    const sentinelOffset = SURFACE_INDEX_HEADER_BYTES
      + new DataView(badSentinel.buffer).getUint32(16, true) * SURFACE_INDEX_STATE_BYTES;
    new DataView(badSentinel.buffer).setUint32(sentinelOffset, 0, true);
    expectFormatError(badSentinel);

    const badTarget = encoded.slice();
    const edgesOffset = new DataView(badTarget.buffer).getUint32(52, true);
    badTarget[edgesOffset + 1] = 0xff;
    badTarget[edgesOffset + 2] = 0xff;
    badTarget[edgesOffset + 3] = 0xff;
    expectFormatError(badTarget);
  });
});
