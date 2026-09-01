import type { MorphologySource } from '../browser-pack/morphology-compiler.js';
import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import {
  conjugationEmissionKey,
  conjugationSourceKey,
  emitPrimaryConjugations,
  emitSecondaryConjugations,
  type ConjugationEmission
} from './conjugation-emissions.js';
import type { CanonicalEntry } from './model.js';

export type EmissionPrecedence = ReadonlyMap<string, number>;

export interface ConfiguredConjugationSelection {
  readonly positions: readonly string[];
  readonly sourcesByPosition: ReadonlyMap<string, ReadonlySet<string>>;
}

/** Exact POS declarations selected by the compiler-owned morphology source. */
export function conjugationPositionsByRoot(
  source: Pick<MorphologySource, 'roots'>
): ReadonlyMap<number, readonly string[]> {
  const result = new Map<number, string[]>();
  for (const root of source.roots) {
    const values = result.get(root.seq) ?? [];
    if (!values.includes(root.pos)) values.push(root.pos);
    result.set(root.seq, values);
  }
  return result;
}

/** Emit one root using only positions selected by the source-owned root slice. */
export function emitConfiguredConjugations(
  entry: CanonicalEntry,
  selection: ConfiguredConjugationSelection
): ConjugationEmission[] {
  const primary = emitPrimaryConjugations(entry, {
    positions: selection.positions,
    sourcesByPosition: selection.sourcesByPosition
  });
  return [
    ...primary,
    ...primary.flatMap(emission => emitSecondaryConjugations(emission, {
      enforceSurfaceRoute: true
    }))
  ];
}

/** Exact root/POS/form selection encoded in the format-v1 morphology input. */
export function conjugationSelectionsFromMorphology(
  morphology: Pick<CompiledMorphologyArtifact, 'rootGroups' | 'rootKeys'>
): ReadonlyMap<number, ConfiguredConjugationSelection> {
  const groupSeqs = morphology.rootGroups.map(group => group.seq);
  const mutable = new Map<number, Map<string, Set<string>>>();
  for (const key of morphology.rootKeys) {
    for (const record of key.records) {
      const seq = groupSeqs[record.rootGroup];
      if (seq === undefined) throw new Error(`Morphology root key has unknown group ${record.rootGroup}`);
      const value = mutable.get(seq) ?? new Map<string, Set<string>>();
      const sources = value.get(key.pos) ?? new Set<string>();
      sources.add(conjugationSourceKey(key.route, key.sourceText));
      value.set(key.pos, sources);
      mutable.set(seq, value);
    }
  }
  return new Map([...mutable].map(([seq, sourcesByPosition]) => [seq, {
    positions: [...sourcesByPosition.keys()],
    sourcesByPosition
  }]));
}

/**
 * Validate and apply the central scheduler's dense precedence. The scheduler,
 * not target allocation, owns phase order (base primary, base secondary,
 * custom primary, custom secondary, then chronological lineages).
 */
export function orderConjugationEmissions(
  emissions: readonly ConjugationEmission[],
  precedence: EmissionPrecedence
): ConjugationEmission[] {
  if (precedence.size !== emissions.length) {
    throw new Error(`Emission precedence covers ${precedence.size}/${emissions.length} emissions`);
  }
  const seen = new Set<string>();
  const rows = emissions.map(emission => {
    const key = conjugationEmissionKey(emission);
    if (seen.has(key)) throw new Error(`Duplicate conjugation emission ${key}`);
    seen.add(key);
    const order = precedence.get(key);
    if (!Number.isSafeInteger(order) || order! < 0 || order! >= emissions.length) {
      throw new Error(`Emission has invalid dense precedence ${key}`);
    }
    return { emission, key, order: order! };
  });
  const orders = new Set(rows.map(row => row.order));
  if (orders.size !== rows.length) throw new Error('Emission precedence contains duplicate ordinals');
  for (let order = 0; order < rows.length; order++) {
    if (!orders.has(order)) throw new Error(`Emission precedence is missing ordinal ${order}`);
  }
  return rows.sort((left, right) => left.order - right.order)
    .map(row => row.emission);
}

export function denseEmissionPrecedence(
  ordered: readonly ConjugationEmission[]
): EmissionPrecedence {
  const result = new Map<string, number>();
  ordered.forEach((emission, order) => {
    const key = conjugationEmissionKey(emission);
    if (result.has(key)) throw new Error(`Duplicate conjugation emission ${key}`);
    result.set(key, order);
  });
  return result;
}
