import { deriveBestReadings } from './best-readings.js';
import {
  compileQualifiedCustomData,
  type CustomCompilation,
  type CustomSourcePaths
} from './chronological-custom.js';
import {
  applyQualifiedErrata,
  loadQualifiedErrata,
  type AppliedErrata
} from './chronological-errata.js';
import {
  applyCanonicalCompatibility,
  loadSourceCompatibility,
  type SourceCompatibilityLedger
} from './compatibility.js';
import { loadJmdictEntries } from './jmdict.js';
import type { CanonicalEntry } from './model.js';

export interface CanonicalRootPaths extends CustomSourcePaths {
  readonly jmdict: string;
  readonly errata: string;
  readonly compatibility: string;
}

export interface CanonicalRootCompilation {
  readonly entries: readonly CanonicalEntry[];
  readonly jmdictEntries: number;
  readonly custom: CustomCompilation;
  readonly errata: AppliedErrata;
  readonly compatibility: SourceCompatibilityLedger;
}

export const QUALIFIED_JMDICT_SOURCE_ID = 'edrdg-jmdict-e-2026-01-01';

/** Build the complete qualified root lexicon from pinned semantic source files. */
export async function compileCanonicalRoots(
  paths: CanonicalRootPaths
): Promise<CanonicalRootCompilation> {
  const jmdict: CanonicalEntry[] = [];
  for await (const entry of loadJmdictEntries(paths.jmdict, QUALIFIED_JMDICT_SOURCE_ID)) {
    jmdict.push(entry);
  }

  const custom = await compileQualifiedCustomData(jmdict, paths, jmdict.length);
  const combined = new Map(jmdict.map(entry => [entry.seq, entry]));
  for (const entry of custom.updatedEntries) combined.set(entry.seq, entry);
  for (const entry of custom.createdRoots) combined.set(entry.seq, entry);

  const errataLedger = await loadQualifiedErrata(paths.errata);
  const errata = applyQualifiedErrata(combined.values(), errataLedger, custom.nextEvent);
  const compatibility = await loadSourceCompatibility(paths.compatibility);
  const compatible = applyCanonicalCompatibility(
    errata.entries,
    compatibility,
    errata.nextEvent
  );

  return {
    entries: compatible.map(deriveBestReadings),
    jmdictEntries: jmdict.length,
    custom,
    errata,
    compatibility
  };
}
