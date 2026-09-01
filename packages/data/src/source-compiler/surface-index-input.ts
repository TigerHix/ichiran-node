import { isRootPayloadKanaSurface } from '../browser-pack/root-payload.js';
import type { CanonicalEntry, CanonicalRoute } from './model.js';

export interface MorphologySurface {
  readonly route: CanonicalRoute;
  readonly surface: string;
}

export interface SurfaceIndexRow {
  readonly surface: string;
  readonly kanaDirect: boolean;
  readonly kanaMorphology: boolean;
  readonly kanjiDirect: boolean;
  readonly kanjiMorphology: boolean;
}

const UTF8 = new TextEncoder();

interface MutableSurfaceFlags {
  kanaDirect: boolean;
  kanaMorphology: boolean;
  kanjiDirect: boolean;
  kanjiMorphology: boolean;
}

function compareBytes(left: Uint8Array, right: Uint8Array): number {
  const shared = Math.min(left.length, right.length);
  for (let index = 0; index < shared; index++) {
    if (left[index] !== right[index]) return left[index]! - right[index]!;
  }
  return left.length - right.length;
}

export function canonicalSurfaceIndexRows(
  entries: readonly CanonicalEntry[],
  morphology: Iterable<MorphologySurface>
): SurfaceIndexRow[] {
  const rows = new Map<string, MutableSurfaceFlags>();
  const row = (surface: string): MutableSurfaceFlags => {
    const existing = rows.get(surface);
    if (existing) return existing;
    const created = {
      kanaDirect: false,
      kanaMorphology: false,
      kanjiDirect: false,
      kanjiMorphology: false
    };
    rows.set(surface, created);
    return created;
  };

  for (const entry of entries) {
    for (const form of entry.kanji) {
      if (!isRootPayloadKanaSurface(form.text)) row(form.text).kanjiDirect = true;
    }
    for (const form of entry.kana) {
      if (isRootPayloadKanaSurface(form.text)) row(form.text).kanaDirect = true;
    }
  }
  for (const value of morphology) {
    if (value.route === 'kana') row(value.surface).kanaMorphology = true;
    else row(value.surface).kanjiMorphology = true;
  }

  return [...rows].map(([surface, flags]) => ({ surface, ...flags }))
    .sort((left, right) => compareBytes(UTF8.encode(left.surface), UTF8.encode(right.surface)));
}

export function encodeSurfaceIndexTsv(rows: readonly SurfaceIndexRow[]): Uint8Array {
  const lines = rows.map(value => [
    value.surface,
    Number(value.kanaDirect),
    Number(value.kanaMorphology),
    Number(value.kanjiDirect),
    Number(value.kanjiMorphology)
  ].join('\t'));
  return UTF8.encode(lines.length === 0 ? '' : `${lines.join('\n')}\n`);
}
