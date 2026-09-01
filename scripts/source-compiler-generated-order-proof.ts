import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { mkdir, writeFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { openMorphology, MORPHOLOGY_SECTION_ID } from '../packages/core/src/morphology.js';
import { openPack } from '../packages/core/src/pack.js';
import { buildMorphology } from '../packages/data/src/browser-pack/morphology-compiler.js';
import { isRootPayloadKanaSurface } from '../packages/data/src/browser-pack/root-payload.js';
import type { AnalyzerSupportLookupOrderSource } from '../packages/data/src/browser-pack/analyzer-support.js';
import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import { conjugationPositionCompatibility } from '../packages/data/src/source-compiler/compatibility.js';
import { chronologicalMorphologySource } from '../packages/data/src/source-compiler/conjugation-errata.js';
import { reduceGeneratedOccurrenceSurfaces } from '../packages/data/src/source-compiler/generated-projection-reduce.js';
import {
  directGeneratedLookupClassPrecedence,
  generatedLookupClassKey
} from '../packages/data/src/source-compiler/generated-projection-stream.js';
import { decodeQualifiedGenerated } from './source-compiler-generated-order-baseline.js';

type Route = 'kana' | 'kanji';
type Locator = Omit<AnalyzerSupportLookupOrderSource, 'rank'>;

interface PhysicalClass {
  readonly id: string;
  readonly precedence: number | null;
  readonly locators: Map<string, Locator>;
}

function surfaceKey(route: Route, surface: string): string {
  return `${route}\u0000${surface}`;
}

function locatorKey(value: Pick<AnalyzerSupportLookupOrderSource,
  'rootSeq' | 'firstAlias' | 'secondAlias'>): string {
  return `${value.rootSeq}\u0000${value.firstAlias ?? -1}\u0000${value.secondAlias ?? -1}`;
}

function rawLocatorKey(value: {
  readonly rootSeq: number;
  readonly firstRule: number;
  readonly secondRule: number | null;
}): string {
  return `${value.rootSeq}\u0000${value.firstRule}\u0000${value.secondRule ?? -1}`;
}

function tombstoneKey(
  route: Route,
  surface: string,
  rootSeq: number,
  firstRule: number,
  secondRule: number | null
): string {
  return `${route}\u0000${surface}\u0000${rootSeq}\u0000${firstRule}\u0000${secondRule ?? -1}`;
}

function addLocator(
  classes: Map<string, PhysicalClass>,
  classId: string,
  locator: Locator,
  precedence: number | null
): void {
  const prior = classes.get(classId);
  if (prior && (prior.precedence === null) !== (precedence === null)) {
    throw new Error(`Physical class ${classId} has precedence ${prior.precedence}/${precedence}`);
  }
  const selectedPrecedence = prior?.precedence === null || precedence === null
    ? null : Math.max(prior?.precedence ?? precedence, precedence);
  const value = prior
    ? { ...prior, precedence: selectedPrecedence }
    : { id: classId, precedence: selectedPrecedence, locators: new Map<string, Locator>() };
  value.locators.set(locatorKey(locator), locator);
  classes.set(classId, value);
}

function normalizedClass(value: PhysicalClass): readonly string[] {
  return [...value.locators.keys()].sort();
}

function normalizedPartition(values: readonly PhysicalClass[]): string {
  return JSON.stringify(values.map(normalizedClass).map(value => JSON.stringify(value)).sort());
}

function setDifference(left: ReadonlySet<string>, right: ReadonlySet<string>): string[] {
  return [...left].filter(value => !right.has(value)).sort();
}

function digestRows(values: readonly string[]): string {
  const hash = createHash('sha256');
  for (const value of values) hash.update(value + '\n');
  return hash.digest('hex');
}

const [hotArg, pathsArg, occurrencesArg, temporaryArg, reportArg, differencesArg] =
  process.argv.slice(2);
if (!hotArg || !pathsArg || !occurrencesArg || !temporaryArg || !reportArg || !differencesArg) {
  throw new Error(
    'Usage: bun scripts/source-compiler-generated-order-proof.ts '
      + '<hot.bin.gz> <generated-paths.bin> <generated-occurrences.bin> '
      + '<temporary-directory> <report.json> <differences.ndjson>'
  );
}
const hotPath = resolve(hotArg);
const pathsPath = resolve(pathsArg);
const occurrencesPath = resolve(occurrencesArg);
const temporaryDirectory = resolve(temporaryArg);
const reportPath = resolve(reportArg);
const differencesPath = resolve(differencesArg);
await mkdir(temporaryDirectory, { recursive: true });

const repository = resolve(import.meta.dir, '..');
const data = join(repository, 'data');
const roots = await compileCanonicalRoots({
  jmdict: join(repository, 'packages/data/JMdict_e.gz'),
  extra: join(data, 'sources/extra.xml'),
  municipality: join(data, 'sources/jichitai.csv'),
  ward: join(data, 'sources/gyoseiku.csv'),
  errata: join(data, 'source-compiler-errata.json'),
  compatibility: join(data, 'source-compiler-compatibility.json')
});
const morphologySource = chronologicalMorphologySource(
  roots.entries,
  roots.errata.conjugationRows,
  {
    dataPath: data,
    extraPositions: conjugationPositionCompatibility(roots.compatibility)
      .map(value => ({ seq: value.seq, pos: value.pos }))
  }
);
const morphology = buildMorphology(morphologySource, { dataPath: data }).artifact;
const packed = openPack(gunzipSync(readFileSync(hotPath)));
const packedMorphology = openMorphology(packed.getSection(MORPHOLOGY_SECTION_ID));
const customRootSeqs = new Set(roots.custom.createdRoots.map(entry => entry.seq));
const directPrecedence = directGeneratedLookupClassPrecedence({
  entries: roots.entries,
  customRootSeqs,
  firstErrataEvent: roots.custom.nextEvent
});
const direct = new Map<string, Map<number, Locator>>();
for (const entry of roots.entries) {
  for (const [route, forms] of [['kana', entry.kana], ['kanji', entry.kanji]] as const) {
    for (const form of forms) {
      if (isRootPayloadKanaSurface(form.text) !== (route === 'kana')) continue;
      const key = surfaceKey(route, form.text);
      const targets = direct.get(key) ?? new Map<number, Locator>();
      targets.set(entry.seq, { rootSeq: entry.seq, firstAlias: null, secondAlias: null });
      direct.set(key, targets);
    }
  }
}
const lexical = new Set(roots.entries.map(entry => entry.seq));
const tombstones = new Set(morphology.tombstones.map(value => tombstoneKey(
  value.route, value.surface, value.rootSeq, value.firstRule, value.secondRule
)));

const qualified = decodeQualifiedGenerated(hotPath);
const qualifiedRecords = new Map(qualified.records.map(value => [locatorKey(value), value]));
const globalRanks = new Map(qualified.lookupOrders.map(value => [locatorKey(value), value.rank]));
const exceptionRanks = new Map(qualified.lookupOrderExceptions.map(value => [
  surfaceKey(value.route, value.surface),
  new Map(value.orders.map(order => [locatorKey(order), order.rank]))
]));
const qualifiedCollisions = new Map(qualified.collisions.map(value => [
  `${surfaceKey(value.route, value.surface)}\u0000${rawLocatorKey(value)}`,
  value
]));
if (qualifiedCollisions.size !== qualified.collisions.length) {
  throw new Error('Qualified pack contains duplicate collision locators');
}

const normalizedHash = createHash('sha256');
const differenceHash = createHash('sha256');
const differenceRows: string[] = [];
const seenQualifiedExceptions = new Set<string>();
const seenQualifiedCollisions = new Set<string>();
let comparedSurfaces = 0;
let exactSurfaces = 0;
let sourceAmbiguousSurfaces = 0;
let qualifiedAmbiguousSurfaces = 0;
let sourcePhysicalClasses = 0;
let qualifiedPhysicalClasses = 0;
let sourceLookupLocators = 0;
let qualifiedLookupLocators = 0;
let groupingChanges = 0;
let orderingChanges = 0;
let winnerChanges = 0;
let sourceOnlyLocators = 0;
let qualifiedOnlyLocators = 0;
let qualifiedMissingRanks = 0;
let qualifiedRankConflicts = 0;
let qualifiedRankPartitionChanges = 0;
let reverseSourceOnlyLocators = 0;
let reversePackedOnlyLocators = 0;
let collisionStatusChanges = 0;
let groupingKana = 0;
let groupingKanji = 0;

const reduction = reduceGeneratedOccurrenceSurfaces({
  pathsPath,
  occurrencesPath,
  temporaryDirectory
}, grouped => {
  const active = grouped.occurrences.filter(value => !tombstones.has(tombstoneKey(
    grouped.route,
    grouped.surface,
    value.rootSeq,
    value.firstRule,
    value.secondRule
  )));
  if (active.length === 0) return;
  const key = surfaceKey(grouped.route, grouped.surface);
  const sourceClasses = new Map<string, PhysicalClass>();
  const qualifiedClasses = new Map<string, PhysicalClass>();
  for (const [targetSeq, locator] of direct.get(key) ?? []) {
    const precedence = directPrecedence.get(generatedLookupClassKey(
      targetSeq, grouped.route, grouped.surface
    ));
    if (precedence === undefined) throw new Error(`Direct class ${targetSeq} has no precedence`);
    addLocator(sourceClasses, `source:${targetSeq}`, locator, precedence);
    addLocator(qualifiedClasses, `direct:${targetSeq}`, locator, null);
  }

  const sourceRawLocators = new Set<string>();
  for (const occurrence of active) {
    const locator: Locator = {
      rootSeq: occurrence.rootSeq,
      firstAlias: occurrence.firstAlias,
      secondAlias: occurrence.secondAlias
    };
    const semanticKey = locatorKey(locator);
    const rawKey = rawLocatorKey(occurrence);
    sourceRawLocators.add(rawKey);
    const collisionKey = `${key}\u0000${rawKey}`;
    const sourceCollision = lexical.has(occurrence.targetSeq);
    const qualifiedCollision = qualifiedCollisions.get(collisionKey);
    if (sourceCollision !== (qualifiedCollision !== undefined)) collisionStatusChanges++;
    if (sourceCollision) {
      if (!(direct.get(key)?.has(occurrence.targetSeq) ?? false)) {
        throw new Error(`Source collision target ${occurrence.targetSeq} has no direct surface`);
      }
    } else {
      addLocator(
        sourceClasses,
        `source:${occurrence.targetSeq}`,
        locator,
        occurrence.precedence
      );
    }
    if (qualifiedCollision) {
      seenQualifiedCollisions.add(collisionKey);
      if (!(direct.get(key)?.has(qualifiedCollision.collisionSeq) ?? false)) {
        throw new Error(`Qualified collision target ${qualifiedCollision.collisionSeq} has no direct surface`);
      }
      continue;
    }
    const record = qualifiedRecords.get(semanticKey);
    const qualifiedClass = record?.physicalGroup === null || record?.physicalGroup === undefined
      ? `unique:${semanticKey}`
      : `group:${record.physicalGroup}`;
    addLocator(qualifiedClasses, qualifiedClass, locator, null);
  }

  const sourceLocatorSet = new Set([...sourceClasses.values()].flatMap(value =>
    [...value.locators.keys()]));
  const qualifiedLocatorSet = new Set([...qualifiedClasses.values()].flatMap(value =>
    [...value.locators.keys()]));
  const sourceOnly = setDifference(sourceLocatorSet, qualifiedLocatorSet);
  const qualifiedOnly = setDifference(qualifiedLocatorSet, sourceLocatorSet);
  sourceOnlyLocators += sourceOnly.length;
  qualifiedOnlyLocators += qualifiedOnly.length;

  const sourceAmbiguous = sourceClasses.size >= 2;
  const qualifiedAmbiguous = qualifiedClasses.size >= 2;
  if (!sourceAmbiguous && !qualifiedAmbiguous) return;
  comparedSurfaces++;
  if (sourceAmbiguous) sourceAmbiguousSurfaces++;
  if (qualifiedAmbiguous) qualifiedAmbiguousSurfaces++;
  sourcePhysicalClasses += sourceClasses.size;
  qualifiedPhysicalClasses += qualifiedClasses.size;
  sourceLookupLocators += sourceLocatorSet.size;
  qualifiedLookupLocators += qualifiedLocatorSet.size;

  const sourceOrdered = [...sourceClasses.values()].sort((left, right) =>
    right.precedence! - left.precedence! || left.id.localeCompare(right.id));
  const localRanks = exceptionRanks.get(key);
  if (localRanks) seenQualifiedExceptions.add(key);
  const rankFor = (locator: string): number | undefined =>
    localRanks ? localRanks.get(locator) : globalRanks.get(locator);
  const qualifiedRanked = qualifiedAmbiguous ? [...qualifiedClasses.values()].map(value => {
    const ranks = new Set([...value.locators.keys()].flatMap(locator => {
      const rank = rankFor(locator);
      if (rank === undefined) {
        qualifiedMissingRanks++;
        return [];
      }
      return [rank];
    }));
    if (ranks.size > 1) qualifiedRankConflicts++;
    return { value, ranks };
  }) : [];
  const qualifiedOrdered = qualifiedAmbiguous
    ? qualifiedRanked
      .filter(value => value.ranks.size === 1)
      .sort((left, right) => left.ranks.values().next().value! - right.ranks.values().next().value!)
      .map(value => value.value)
    : [...qualifiedClasses.values()];
  if (qualifiedAmbiguous
    && normalizedPartition(qualifiedOrdered) !== normalizedPartition([...qualifiedClasses.values()])) {
    qualifiedRankPartitionChanges++;
  }

  const sourceNormalized = sourceOrdered.map(normalizedClass);
  const qualifiedNormalized = qualifiedOrdered.map(normalizedClass);
  const groupingChanged = normalizedPartition(sourceOrdered)
    !== normalizedPartition([...qualifiedClasses.values()]);
  const orderingChanged = !groupingChanged
    && JSON.stringify(sourceNormalized) !== JSON.stringify(qualifiedNormalized);
  const sourceWinner = sourceNormalized[0] ?? [];
  const qualifiedWinner = qualifiedNormalized[0] ?? [];
  const winnerChanged = JSON.stringify(sourceWinner) !== JSON.stringify(qualifiedWinner);

  let reverseSourceOnly: string[] = [];
  let reversePackedOnly: string[] = [];
  if (groupingChanged) {
    groupingChanges++;
    if (grouped.route === 'kana') groupingKana++;
    else groupingKanji++;
    const packedRawLocators = new Set(packedMorphology.lookup(
      grouped.surface, grouped.route
    ).map(candidate => rawLocatorKey({
      rootSeq: candidate.rootSeq,
      firstRule: candidate.ruleIds[0],
      secondRule: candidate.ruleIds[1] ?? null
    })));
    reverseSourceOnly = setDifference(sourceRawLocators, packedRawLocators);
    reversePackedOnly = setDifference(packedRawLocators, sourceRawLocators);
    reverseSourceOnlyLocators += reverseSourceOnly.length;
    reversePackedOnlyLocators += reversePackedOnly.length;
  }
  if (orderingChanged) orderingChanges++;
  if (winnerChanged) winnerChanges++;

  const normalized = JSON.stringify({
    route: grouped.route,
    surface: grouped.surface,
    source: sourceNormalized,
    qualified: qualifiedNormalized
  });
  normalizedHash.update(normalized + '\n');
  const rankIncomplete = qualifiedAmbiguous
    && qualifiedRanked.some(value => value.ranks.size !== 1);
  if (!groupingChanged && !orderingChanged && !winnerChanged
    && sourceOnly.length === 0 && qualifiedOnly.length === 0
    && !rankIncomplete && reverseSourceOnly.length === 0 && reversePackedOnly.length === 0) {
    exactSurfaces++;
    return;
  }
  const row = JSON.stringify({
    route: grouped.route,
    surface: grouped.surface,
    source: sourceNormalized,
    qualified: qualifiedNormalized,
    sourceClasses: sourceOrdered.map(value => ({
      targetSeq: Number(value.id.slice('source:'.length)),
      precedence: value.precedence,
      locators: normalizedClass(value)
    })),
    qualifiedClasses: [...qualifiedClasses.values()].map(value => ({
      physicalIdentity: value.id,
      rank: qualifiedRanked.find(ranked => ranked.value === value)?.ranks.values().next().value ?? null,
      locators: normalizedClass(value)
    })).sort((left, right) => left.physicalIdentity.localeCompare(right.physicalIdentity)),
    sourceOnlyLocators: sourceOnly,
    qualifiedOnlyLocators: qualifiedOnly,
    reverseSourceOnlyLocators: reverseSourceOnly,
    reversePackedOnlyLocators: reversePackedOnly,
    groupingChanged,
    orderingChanged,
    winnerChanged,
    qualifiedAmbiguous,
    qualifiedRankIncomplete: rankIncomplete
  });
  differenceHash.update(row + '\n');
  differenceRows.push(row);
});

const unseenExceptions = qualified.lookupOrderExceptions.filter(value =>
  !seenQualifiedExceptions.has(surfaceKey(value.route, value.surface)));
const unseenCollisions = qualified.collisions.filter(value => !seenQualifiedCollisions.has(
  `${surfaceKey(value.route, value.surface)}\u0000${rawLocatorKey(value)}`
));
const collisionSemanticRows = qualified.collisions.map(value => JSON.stringify([
  value.route,
  value.surface,
  value.rootSeq,
  value.firstAlias,
  value.secondAlias,
  value.collisionSeq
])).sort();
const report = {
  source: {
    ambiguousSurfaces: sourceAmbiguousSurfaces,
    physicalClasses: sourcePhysicalClasses,
    lookupLocators: sourceLookupLocators,
    occurrenceRows: reduction.rows,
    occurrenceSurfaces: reduction.surfaces,
    maxOccurrenceSurfaceRows: reduction.maxSurfaceRows
  },
  qualified: {
    ambiguousSurfaces: qualifiedAmbiguousSurfaces,
    physicalClasses: qualifiedPhysicalClasses,
    lookupLocators: qualifiedLookupLocators,
    generatedRecords: qualified.records.length,
    declaredPhysicalGroups: qualified.physicalGroups,
    globalRankLocators: qualified.lookupOrders.length,
    exceptionSurfaces: qualified.lookupOrderExceptions.length,
    exceptionLocators: qualified.lookupOrderExceptions.reduce((sum, value) =>
      sum + value.orders.length, 0),
    unseenExceptionSurfaces: unseenExceptions.length,
    collisions: qualified.collisions.length,
    collisionSemanticSha256: digestRows(collisionSemanticRows),
    unseenCollisions: unseenCollisions.length
  },
  comparison: {
    comparedSurfaces,
    exactSurfaces,
    changedSurfaces: differenceRows.length,
    groupingChanges,
    groupingKana,
    groupingKanji,
    orderingChanges,
    winnerChanges,
    sourceOnlyLocators,
    qualifiedOnlyLocators,
    collisionStatusChanges,
    qualifiedMissingRanks,
    qualifiedRankConflicts,
    qualifiedRankPartitionChanges,
    reverseGroupingSurfaces: groupingChanges,
    reverseSourceOnlyLocators,
    reversePackedOnlyLocators,
    normalizedSha256: normalizedHash.digest('hex'),
    differenceSha256: differenceHash.digest('hex'),
    examples: differenceRows.slice(0, 20).map(value => JSON.parse(value))
  }
};
await writeFile(differencesPath, differenceRows.join('\n') + '\n', { flag: 'wx' });
await writeFile(reportPath, JSON.stringify(report, null, 2) + '\n', { flag: 'wx' });
console.log(JSON.stringify(report, null, 2));
