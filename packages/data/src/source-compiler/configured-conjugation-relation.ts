import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import type { ConjugationSuppression } from './conjugation-errata.js';
import {
  forwardRelationKey,
  writeConjugationRelationKeys,
  type ConjugationRelationKey
} from './conjugation-relation-proof.js';
import {
  conjugationSelectionsFromMorphology,
  emitConfiguredConjugations
} from './conjugation-emission-order.js';
import type { ConjugationEmission, EmissionForm } from './conjugation-emissions.js';
import type { CanonicalEntry, ConjugationProperty } from './model.js';
import { sameConjugationProperty } from './conjugation-identity.js';
import {
  omitsConjugationReadingLineage,
  type ConjugationReadingLineageCompatibilityRow
} from './compatibility.js';

export interface ConfiguredRelationInput {
  readonly entries: readonly CanonicalEntry[];
  readonly positionsByRoot: ReadonlyMap<number, readonly string[]>;
  readonly suppressions: readonly ConjugationSuppression[];
  readonly lineageCompatibility: readonly ConjugationReadingLineageCompatibilityRow[];
  readonly morphology: CompiledMorphologyArtifact;
}

export interface ConfiguredRelationWriteResult {
  readonly roots: number;
  readonly rows: number;
  readonly emittedRows: number;
  readonly manualPatchRows: number;
  readonly appliedSuppressions: number;
  readonly ghostSuppressions: readonly ConjugationSuppression[];
  readonly appliedTombstones: number;
  readonly unmatchedTombstones: number;
}

interface RelationFilter {
  readonly route: 'kana' | 'kanji';
  readonly surface: string;
  readonly rootSeq: number;
  readonly first: ConjugationProperty;
  readonly second: ConjugationProperty | null;
}

function property(value: CompiledMorphologyArtifact['rules'][number]): ConjugationProperty {
  return {
    pos: value.pos,
    type: value.type,
    negative: value.negative,
    formal: value.formal
  };
}

function matches(
  filter: RelationFilter,
  emission: ConjugationEmission,
  form: EmissionForm
): boolean {
  return filter.rootSeq === emission.rootSeq
    && filter.route === form.route
    && filter.surface === form.surface
    && sameConjugationProperty(filter.first, emission.first)
    && (filter.second === null
      ? emission.second === null
      : emission.second !== null && sameConjugationProperty(filter.second, emission.second));
}

function tombstoneFilters(morphology: CompiledMorphologyArtifact): RelationFilter[] {
  return morphology.tombstones.map(value => {
    const first = morphology.rules[value.firstRule];
    const second = value.secondRule === null ? null : morphology.rules[value.secondRule];
    if (!first || (value.secondRule !== null && !second)) {
      throw new Error(`Tombstone for root ${value.rootSeq} references an unknown rule`);
    }
    return {
      route: value.route,
      surface: value.surface,
      rootSeq: value.rootSeq,
      first: property(first),
      second: second === null ? null : property(second)
    };
  });
}

function suppressionFilter(value: ConjugationSuppression): RelationFilter {
  return {
    route: value.route,
    surface: value.surface,
    rootSeq: value.rootSeq,
    first: value.first,
    second: value.second
  };
}

function patchKey(
  patch: CompiledMorphologyArtifact['patches'][number],
  morphology: CompiledMorphologyArtifact
): ConjugationRelationKey {
  const first = morphology.rules[patch.firstRule];
  const second = patch.secondRule === null ? null : morphology.rules[patch.secondRule];
  if (!first || (patch.secondRule !== null && !second)) {
    throw new Error(`Manual patch for root ${patch.rootSeq} references an unknown rule`);
  }
  return {
    rootSeq: patch.rootSeq,
    route: patch.route,
    surface: patch.surface,
    sourceText: patch.sourceText,
    sourceForm: patch.sourceForm,
    sourceReading: patch.sourceReading,
    first: property(first),
    second: second === null ? null : property(second),
    intermediate: patch.intermediate,
    sourceOrdinal: patch.ord,
    sourceCommon: patch.common
  };
}

/**
 * Write the exact configured forward relation used by the pack: only selected
 * MorphologySource positions, chronological suppressions, compiled tombstones,
 * and every compiled manual patch. The result is directly accepted by
 * `scripts/source-compiler-conjugation-proof.ts --forward` for exhaustive
 * comparison with the packed reverse relation.
 */
export async function writeConfiguredConjugationRelation(
  input: ConfiguredRelationInput,
  destination: string
): Promise<ConfiguredRelationWriteResult> {
  const entriesBySeq = new Map(input.entries.map(entry => [entry.seq, entry]));
  if (entriesBySeq.size !== input.entries.length) throw new Error('Canonical entries contain duplicate ids');
  for (const patch of input.morphology.patches) {
    if (!entriesBySeq.has(patch.rootSeq)) {
      throw new Error(`Manual patch references missing root ${patch.rootSeq}`);
    }
  }
  const suppressionFilters = input.suppressions.map(suppressionFilter);
  const tombstones = tombstoneFilters(input.morphology);
  const selections = conjugationSelectionsFromMorphology(input.morphology);
  const suppressionHits = new Uint32Array(suppressionFilters.length);
  const tombstoneHits = new Uint32Array(tombstones.length);
  let emittedRows = 0;

  function* relation(): Generator<ConjugationRelationKey> {
    for (const entry of input.entries) {
      const selection = selections.get(entry.seq);
      if (!selection) continue;
      const expectedPositions = input.positionsByRoot.get(entry.seq) ?? [];
      if (selection.positions.some(pos => !expectedPositions.includes(pos))
        || expectedPositions.some(pos => !selection.positions.includes(pos))) {
        throw new Error(`Configured position map disagrees with morphology for root ${entry.seq}`);
      }
      for (const emission of emitConfiguredConjugations(entry, selection)) {
        for (const generated of emission.forms) {
          let suppressed = false;
          suppressionFilters.forEach((filter, index) => {
            if (!matches(filter, emission, generated)) return;
            suppressionHits[index]++;
            suppressed = true;
          });
          tombstones.forEach((filter, index) => {
            if (!matches(filter, emission, generated)) return;
            tombstoneHits[index]++;
            suppressed = true;
          });
          if (input.lineageCompatibility.some(row => omitsConjugationReadingLineage(row, {
            rootSeq: emission.rootSeq,
            route: generated.route,
            sourceText: generated.sourceText,
            firstRule: generated.firstRule,
            secondRule: generated.secondRule
          }))) suppressed = true;
          if (suppressed) continue;
          emittedRows++;
          yield forwardRelationKey(entry, emission, generated);
        }
      }
    }
    for (const patch of input.morphology.patches) yield patchKey(patch, input.morphology);
  }

  const written = await writeConjugationRelationKeys(relation(), destination);
  const ghostSuppressions = input.suppressions.filter((_, index) => suppressionHits[index] === 0);
  return {
    roots: input.entries.length,
    rows: written.rows,
    emittedRows,
    manualPatchRows: input.morphology.patches.length,
    appliedSuppressions: suppressionHits.filter(value => value > 0).length,
    ghostSuppressions,
    appliedTombstones: tombstoneHits.filter(value => value > 0).length,
    unmatchedTombstones: tombstoneHits.filter(value => value === 0).length
  };
}
