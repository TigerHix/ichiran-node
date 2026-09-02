import {
  DO_NOT_CONJUGATE_SEQ,
  POS_WITH_CONJ_RULES
} from '../data/conj-rules.js';
import type {
  MorphologyManualPatchSource,
  MorphologySource
} from '../browser-pack/morphology-compiler.js';
import { isRootPayloadKanaSurface } from '../browser-pack/root-payload.js';
import { consumeCompatibilityRow } from './compatibility.js';
import { entryPartOfSpeech, type CanonicalEntry } from './model.js';

export interface ExtraConjugationPosition {
  readonly seq: number;
  readonly pos: string;
  readonly id?: string;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function semanticPosition(pos: string): string {
  return pos === 'cop-da' ? 'cop' : pos;
}

export function canonicalMorphologySource(
  entries: readonly CanonicalEntry[],
  extraPositions: readonly ExtraConjugationPosition[] = [],
  manualPatches: readonly MorphologyManualPatchSource[] = []
): MorphologySource {
  const allowed = new Set(POS_WITH_CONJ_RULES);
  const excluded = new Set(DO_NOT_CONJUGATE_SEQ);
  const manualRoots = new Set(manualPatches.map(patch => patch.rootSeq));
  const entriesBySeq = new Map(entries.map(entry => [entry.seq, entry]));
  const extraBySeq = new Map<number, string[]>();
  for (const value of extraPositions) {
    const entry = entriesBySeq.get(value.seq);
    if (!entry) throw new Error(`Conjugation-position compatibility ${value.id ?? value.seq} has no root`);
    const positions = extraBySeq.get(value.seq) ?? [];
    const pos = semanticPosition(value.pos);
    if (entryPartOfSpeech(entry).filter(value => allowed.has(value)).map(semanticPosition).includes(pos)) {
      throw new Error(`Conjugation-position compatibility ${value.id ?? `${value.seq}/${pos}`} is stale`);
    }
    if (positions.includes(pos)) {
      throw new Error(`Duplicate conjugation-position compatibility ${value.seq}/${pos}`);
    }
    if (excluded.has(value.seq)) {
      throw new Error(`Conjugation-position compatibility ${value.id ?? value.seq} names an excluded root`);
    }
    positions.push(pos);
    extraBySeq.set(value.seq, positions);
    consumeCompatibilityRow(value, 'conjugation-position');
  }

  const roots: MorphologySource['roots'][number][] = [];
  const rootForms: MorphologySource['rootForms'][number][] = [];
  for (const entry of entries) {
    if (excluded.has(entry.seq)) continue;
    const positions = [...new Set([
      ...entryPartOfSpeech(entry).filter(pos => allowed.has(pos)).map(semanticPosition),
      ...(extraBySeq.get(entry.seq) ?? [])
    ])].sort(compareText);
    if (positions.length === 0 && !manualRoots.has(entry.seq)) continue;

    for (const form of [...entry.kanji, ...entry.kana]) {
      rootForms.push({ seq: entry.seq, text: form.text });
    }
    for (const pos of positions) {
      for (const form of entry.kana) {
        if (!form.conjugatable || !isRootPayloadKanaSurface(form.text)) continue;
        roots.push({
          seq: entry.seq,
          pos,
          route: 'kana',
          text: form.text,
          ord: form.ordinal,
          common: form.common,
          counterpart: form.best
        });
      }
      for (const form of entry.kanji) {
        if (!form.conjugatable || isRootPayloadKanaSurface(form.text)) continue;
        roots.push({
          seq: entry.seq,
          pos,
          route: 'kanji',
          text: form.text,
          ord: form.ordinal,
          common: form.common,
          counterpart: form.best
        });
      }
    }
  }

  roots.sort((left, right) =>
    compareText(left.route, right.route)
    || compareText(left.pos, right.pos)
    || compareText(left.text, right.text)
    || left.seq - right.seq
    || left.ord - right.ord);
  rootForms.sort((left, right) => left.seq - right.seq || compareText(left.text, right.text));
  return { roots, rootForms, manualPatches };
}
