import { createHash } from 'node:crypto';
import type {
  AnalyzerSupportCollisionSource,
  AnalyzerSupportGeneratedMemberSource,
  AnalyzerSupportGeneratedRecordSource,
  AnalyzerSupportGeneratedSource,
  AnalyzerSupportLookupOrderSource
} from '../browser-pack/analyzer-support.js';
import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import { isRootPayloadKanaSurface } from '../browser-pack/root-payload.js';
import {
  compileLookupOrders,
  type LookupOrderRow
} from '../browser-pack/lookup-order-compression.js';
import {
  canonicalCollisionEntryFacts,
  sourceNativeCollisionKey
} from './analyzer-support-collisions.js';
import {
  generatedLookupClasses,
  reduceGeneratedOccurrenceSurfaces,
  reduceGeneratedPhysicalMembers,
  reduceGeneratedSemanticPaths,
  type GeneratedPhysicalTargetMembers
} from './generated-projection-reduce.js';
import {
  readGeneratedOccurrenceSpool,
  readGeneratedPathSpool
} from './generated-projection-spool.js';
import {
  directGeneratedLookupClassPrecedence,
  generatedLookupClassKey,
  type GeneratedProjectionStreamResult
} from './generated-projection-stream.js';
import type { CanonicalEntry, ConjugationProperty } from './model.js';

interface BoundedGeneratedInput {
  readonly projection: GeneratedProjectionStreamResult;
  readonly entries: readonly CanonicalEntry[];
  readonly morphology: CompiledMorphologyArtifact;
  readonly temporaryDirectory: string;
  readonly customRootSeqs: ReadonlySet<number>;
  readonly firstErrataEvent: number;
  readonly maxOccurrenceChunkRows?: number;
}

export interface BoundedGeneratedProjection {
  readonly generated: AnalyzerSupportGeneratedSource;
  readonly collisions: readonly AnalyzerSupportCollisionSource[];
  readonly occurrenceRows: number;
  readonly occurrenceSurfaces: number;
  readonly maxOccurrenceSurfaceRows: number;
  readonly semanticPathSha256: string;
}

interface AmbiguousSurface {
  readonly route: 'kana' | 'kanji';
  readonly surface: string;
  readonly classes: readonly LookupClass[];
}

interface LookupClass {
  readonly targetSeq: number;
  readonly precedence: number;
  readonly locators: readonly AnalyzerSupportLookupOrderSource[];
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function surfaceKey(route: 'kana' | 'kanji', surface: string): string {
  return `${route}\u0000${surface}`;
}

function locatorKey(value: Pick<AnalyzerSupportLookupOrderSource,
  'rootSeq' | 'firstAlias' | 'secondAlias'>): string {
  return `${value.rootSeq}\u0000${value.firstAlias ?? -1}\u0000${value.secondAlias ?? -1}`;
}

function tombstoneKey(
  route: 'kana' | 'kanji',
  surface: string,
  rootSeq: number,
  firstRule: number,
  secondRule: number | null
): string {
  return `${route}\u0000${surface}\u0000${rootSeq}\u0000${firstRule}\u0000${secondRule ?? -1}`;
}

function sameProperty(
  left: AnalyzerSupportGeneratedMemberSource['property'],
  right: AnalyzerSupportGeneratedMemberSource['property']
): boolean {
  return left.posId === right.posId && left.type === right.type
    && left.negative === right.negative && left.formal === right.formal;
}

function propertyMatches(semantic: ConjugationProperty, physical: ConjugationProperty): boolean {
  return semantic.pos === physical.pos && semantic.type === physical.type
    && (semantic.negative === null || semantic.negative === physical.negative)
    && (semantic.formal === null || semantic.formal === physical.formal);
}

function projectionDigest(records: readonly AnalyzerSupportGeneratedRecordSource[]): string {
  const hash = createHash('sha256');
  for (const record of records) {
    hash.update([record.rootSeq, record.firstAlias, record.secondAlias ?? -1,
      record.counts?.[0] ?? -1, record.counts?.[1] ?? -1,
      record.physicalGroup ?? 0].join('\t') + '\n');
    for (const member of record.members ?? []) {
      hash.update([member.property.posId, member.property.type,
        member.property.negative === null ? -1 : Number(member.property.negative),
        member.property.formal === null ? -1 : Number(member.property.formal),
        member.memberOrd, member.propOrd, member.viaMemberOrd ?? -1].join('\t') + '\n');
    }
  }
  return hash.digest('hex');
}

function buildGeneratedRecords(input: BoundedGeneratedInput): {
  readonly values: Omit<AnalyzerSupportGeneratedSource,
    'lookupOrders' | 'lookupOrderSourceRows' | 'lookupOrderSourceSha256'
    | 'lookupOrderSurfaces' | 'lookupOrderClasses' | 'lookupOrderEquivalenceClasses'
    | 'lookupOrderComponents' | 'lookupOrderCyclicComponents' | 'lookupOrderEdges'
    | 'lookupOrderMaxRank' | 'lookupOrderProjectionSha256' | 'lookupOrderExceptions'
    | 'lookupOrderExceptionClasses' | 'lookupOrderExceptionLocators'>;
  readonly semanticPathSha256: string;
} {
  const membersByTarget = new Map<number, GeneratedPhysicalTargetMembers>();
  const generatedTargets = new Set(input.projection.targets
    .filter(target => target.origin === 'generated').map(target => target.seq));
  const sharedTargets = new Set<number>();
  reduceGeneratedPhysicalMembers(input.projection.pathsPath, target => {
    membersByTarget.set(target.targetSeq, target);
    if (generatedTargets.has(target.targetSeq) && target.paths > 1) {
      sharedTargets.add(target.targetSeq);
    }
  });

  const patchForms = new Map<number, { readonly kana: Set<string>; readonly kanji: Set<string> }>();
  // Patch target ids live in the path table; the occurrence reducer below is
  // deliberately not reused here because this pass reads only fifty rows.
  const patchOrdinals = new Set<number>();
  for (const row of readGeneratedOccurrenceSpool(input.projection.occurrencesPath)) {
    if (row.installed && row.kind === 'patch') patchOrdinals.add(row.pathOrdinal);
  }
  const matchedPatchOrdinals = new Set<number>();
  if (patchOrdinals.size > 0) {
    const targetByOrdinal = new Map<number, number>();
    for (const row of readGeneratedPathSpool(input.projection.pathsPath)) {
      if (patchOrdinals.has(row.ordinal)) targetByOrdinal.set(row.ordinal, row.targetSeq);
    }
    for (const row of readGeneratedOccurrenceSpool(input.projection.occurrencesPath)) {
      if (!row.installed) continue;
      if (row.kind !== 'patch') {
        if (patchOrdinals.has(row.pathOrdinal)) matchedPatchOrdinals.add(row.pathOrdinal);
        continue;
      }
      const targetSeq = targetByOrdinal.get(row.pathOrdinal);
      if (targetSeq === undefined) throw new Error(`Patch references missing path ${row.pathOrdinal}`);
      const forms = patchForms.get(targetSeq) ?? { kana: new Set<string>(), kanji: new Set<string>() };
      forms[row.route].add(row.surface);
      patchForms.set(targetSeq, forms);
    }
  }

  const entries = new Map(input.entries.map(entry => [entry.seq, entry]));
  const targets = new Map(input.projection.targets.map(target => [target.seq, target]));
  const positions = new Map(input.morphology.positions.map((pos, id) => [pos, id]));
  const groupIds = new Map<number, number>();
  const records: AnalyzerSupportGeneratedRecordSource[] = [];
  // The qualified query unions patch rows with rule-derived rows before
  // DISTINCT. A manual-only path contributes its one semantic row; only a
  // patch sharing an already rule-derived path contributes one extra match.
  let matchedPaths = matchedPatchOrdinals.size;
  let countExceptions = 0;
  let physicalMembers = 0;
  let propertyOverrides = 0;
  let maxMemberOrd = 0;
  let maxViaMemberOrd = 0;
  let maxPropOrd = 0;
  const semantic = reduceGeneratedSemanticPaths(input.projection.pathsPath, path => {
    const root = entries.get(path.rootSeq);
    const target = targets.get(path.targetSeq);
    const targetMembers = membersByTarget.get(path.targetSeq);
    if (!root || !target || !targetMembers) {
      throw new Error(`Generated semantic path ${path.ordinal} has incomplete target data`);
    }
    const finalAlias = path.secondAlias ?? path.firstAlias;
    const finalProperty = input.projection.aliasProperties[finalAlias];
    if (!finalProperty) throw new Error(`Generated semantic path has unknown alias ${finalAlias}`);
    const posId = positions.get(finalProperty.pos);
    if (posId === undefined) throw new Error(`Unknown morphology position ${finalProperty.pos}`);
    const defaultProperty = { posId, type: finalProperty.type,
      negative: finalProperty.negative, formal: finalProperty.formal };
    const member = targetMembers.members.find(value =>
      value.rootSeq === path.rootSeq && value.viaTargetSeq === path.viaTargetSeq);
    if (!member) throw new Error(`Generated semantic path ${path.ordinal} has no physical member`);
    const viaMember = path.viaTargetSeq === null ? null
      : membersByTarget.get(path.viaTargetSeq)?.members.find(value =>
        value.rootSeq === path.rootSeq && value.viaTargetSeq === null) ?? null;
    if (path.viaTargetSeq !== null && !viaMember) {
      throw new Error(`Generated semantic path ${path.ordinal} has no prefix member`);
    }
    const memberRows = member.properties.map(value => {
      const property = input.projection.aliasProperties[value.alias];
      if (!property) throw new Error(`Physical member has unknown alias ${value.alias}`);
      const propertyPosId = positions.get(property.pos);
      if (propertyPosId === undefined) throw new Error(`Unknown physical position ${property.pos}`);
      if (propertyMatches(finalProperty, property)) matchedPaths++;
      return {
        property: { posId: propertyPosId, type: property.type,
          negative: property.negative, formal: property.formal },
        memberOrd: member.memberOrd,
        propOrd: value.propOrd,
        viaMemberOrd: viaMember?.memberOrd ?? null
      };
    }).sort((left, right) => left.memberOrd - right.memberOrd
      || left.propOrd - right.propOrd || (left.viaMemberOrd ?? -1) - (right.viaMemberOrd ?? -1));
    if (memberRows.length === 0) throw new Error(`Generated semantic path ${path.ordinal} has no properties`);
    if (sharedTargets.has(path.targetSeq) && !groupIds.has(path.targetSeq)) {
      groupIds.set(path.targetSeq, groupIds.size + 1);
    }
    const physicalGroup = groupIds.get(path.targetSeq) ?? null;
    const viaMembers = path.viaTargetSeq === null
      ? 0 : membersByTarget.get(path.viaTargetSeq)?.members.length ?? 0;
    const needsMembers = physicalGroup !== null || memberRows.length !== 1
      || memberRows[0]!.memberOrd !== 0
      || (memberRows[0]!.viaMemberOrd !== null && viaMembers > 1)
      || !sameProperty(memberRows[0]!.property, defaultProperty);
    if (needsMembers) {
      physicalMembers += memberRows.length;
      for (const value of memberRows) {
        if (!sameProperty(value.property, defaultProperty)) propertyOverrides++;
        maxMemberOrd = Math.max(maxMemberOrd, value.memberOrd);
        maxViaMemberOrd = Math.max(maxViaMemberOrd, value.viaMemberOrd ?? 0);
        maxPropOrd = Math.max(maxPropOrd, value.propOrd);
      }
    }
    const extra = patchForms.get(path.targetSeq);
    const nKanji = new Set([...target.kanji, ...(extra?.kanji ?? [])]).size;
    const nKana = new Set([...target.kana, ...(extra?.kana ?? [])]).size;
    const counts = nKanji !== root.kanji.length || nKana !== root.kana.length
      ? [nKanji, nKana] as const : null;
    if (counts !== null) countExceptions++;
    if (counts !== null || needsMembers) records.push({
      rootSeq: path.rootSeq,
      firstAlias: path.firstAlias,
      secondAlias: path.secondAlias,
      counts,
      physicalGroup,
      members: needsMembers ? memberRows : null
    });
  }, targetSeq => generatedTargets.has(targetSeq));
  return {
    values: {
      ruleAliases: input.projection.ruleAliases,
      aliasCount: input.projection.aliasProperties.length,
      records,
      semanticPaths: semantic.paths,
      matchedPaths,
      countExceptions,
      physicalGroups: groupIds.size,
      physicalMembers,
      propertyOverrides,
      maxMemberOrd,
      maxViaMemberOrd,
      maxPropOrd,
      projectionSha256: projectionDigest(records)
    },
    semanticPathSha256: semantic.sha256
  };
}

function buildLookupAndCollisions(input: BoundedGeneratedInput): {
  readonly lookup: Pick<AnalyzerSupportGeneratedSource,
    'lookupOrders' | 'lookupOrderSourceRows' | 'lookupOrderSourceSha256'
    | 'lookupOrderSurfaces' | 'lookupOrderClasses' | 'lookupOrderEquivalenceClasses'
    | 'lookupOrderComponents' | 'lookupOrderCyclicComponents' | 'lookupOrderEdges'
    | 'lookupOrderMaxRank' | 'lookupOrderProjectionSha256' | 'lookupOrderExceptions'
    | 'lookupOrderExceptionClasses' | 'lookupOrderExceptionLocators'>;
  readonly collisions: readonly AnalyzerSupportCollisionSource[];
  readonly rows: number;
  readonly surfaces: number;
  readonly maxSurfaceRows: number;
} {
  const direct = new Map<string, Map<number, AnalyzerSupportLookupOrderSource>>();
  for (const entry of input.entries) {
    for (const [route, forms] of [['kana', entry.kana], ['kanji', entry.kanji]] as const) {
      for (const form of forms) {
        if (isRootPayloadKanaSurface(form.text) !== (route === 'kana')) continue;
        const key = surfaceKey(route, form.text);
        const values = direct.get(key) ?? new Map<number, AnalyzerSupportLookupOrderSource>();
        values.set(entry.seq, { rootSeq: entry.seq, firstAlias: null, secondAlias: null, rank: 0 });
        direct.set(key, values);
      }
    }
  }
  const directPrecedence = directGeneratedLookupClassPrecedence(input);
  const tombstones = new Set(input.morphology.tombstones.map(value => tombstoneKey(
    value.route, value.surface, value.rootSeq, value.firstRule, value.secondRule
  )));
  const lexical = new Map(input.entries.map(entry => [entry.seq, entry]));
  const ambiguous: AmbiguousSurface[] = [];
  const collisionValues = new Map<string, AnalyzerSupportCollisionSource>();
  const reduction = reduceGeneratedOccurrenceSurfaces({
    pathsPath: input.projection.pathsPath,
    occurrencesPath: input.projection.occurrencesPath,
    temporaryDirectory: input.temporaryDirectory,
    ...(input.maxOccurrenceChunkRows === undefined
      ? {} : { maxChunkRows: input.maxOccurrenceChunkRows })
  }, surface => {
    const active = surface.occurrences.filter(value => !tombstones.has(tombstoneKey(
      surface.route, surface.surface, value.rootSeq, value.firstRule, value.secondRule
    )));
    if (active.length === 0) return;
    for (const occurrence of active) {
      const target = lexical.get(occurrence.targetSeq);
      if (!target) continue;
      const value: AnalyzerSupportCollisionSource = {
        rootSeq: occurrence.rootSeq,
        collisionSeq: occurrence.targetSeq,
        viaSeq: occurrence.viaTargetSeq,
        route: occurrence.route,
        surface: occurrence.surface,
        ruleIds: occurrence.secondRule === null
          ? [occurrence.firstRule] : [occurrence.firstRule, occurrence.secondRule],
        ...canonicalCollisionEntryFacts(target)
      };
      const key = sourceNativeCollisionKey(value);
      const prior = collisionValues.get(key);
      if (prior && JSON.stringify(prior) !== JSON.stringify(value)) {
        throw new Error(`Conflicting source-native collision ${key}`);
      }
      collisionValues.set(key, value);
    }
    const generatedOccurrences = active.filter(value => !lexical.has(value.targetSeq));
    const classes = new Map<number, LookupClass>(
      generatedLookupClasses({ ...surface, occurrences: generatedOccurrences }).map(value => [value.targetSeq, {
        ...value,
        locators: value.locators.map(locator => ({ ...locator, rank: 0 }))
      }])
    );
    for (const [targetSeq, locator] of direct.get(surfaceKey(surface.route, surface.surface)) ?? []) {
      const prior = classes.get(targetSeq);
      if (prior) {
        const locators = new Map(prior.locators.map(value => [locatorKey(value), value]));
        locators.set(locatorKey(locator), locator);
        classes.set(targetSeq, { ...prior, locators: [...locators.values()] });
      } else {
        const precedence = directPrecedence.get(generatedLookupClassKey(
          targetSeq, surface.route, surface.surface
        ));
        if (precedence === undefined) throw new Error('Direct lookup class has no source precedence');
        classes.set(targetSeq, { targetSeq, precedence, locators: [locator] });
      }
    }
    if (classes.size < 2) return;
    const ordered = [...classes.values()].sort((left, right) =>
      right.precedence - left.precedence || left.targetSeq - right.targetSeq);
    if (new Set(ordered.map(value => value.precedence)).size !== ordered.length) {
      throw new Error(`Lookup precedence is not strict for ${surface.route}/${surface.surface}`);
    }
    ambiguous.push({ route: surface.route, surface: surface.surface, classes: ordered });
  });
  const physicalClasses = ambiguous.reduce((total, value) => total + value.classes.length, 0);
  const rows: LookupOrderRow[] = [];
  for (const surface of ambiguous) surface.classes.forEach((group, rank) => {
    for (const locator of group.locators) rows.push({
      rootSeq: locator.rootSeq,
      firstAlias: locator.firstAlias,
      secondAlias: locator.secondAlias,
      route: surface.route,
      surface: surface.surface,
      rank,
      physicalClasses,
      locatedClasses: physicalClasses,
      ambiguousSurfaces: ambiguous.length,
      loadedPatches: input.morphology.patches.length
    });
  });
  const compiled = rows.length === 0 ? {
    values: [] as AnalyzerSupportGeneratedSource['lookupOrders'],
    sourceRows: 0, sourceSha256: '', surfaces: 0, physicalClasses: 0,
    equivalenceClasses: 0, components: 0, cyclicComponents: 0, edges: 0,
    maxRank: 0, sha256: '',
    exceptions: [] as AnalyzerSupportGeneratedSource['lookupOrderExceptions'],
    exceptionClasses: 0, exceptionLocators: 0
  } : compileLookupOrders(
    rows, input.projection.aliasProperties.length, input.morphology.patches.length
  );
  return {
    lookup: {
      lookupOrders: compiled.values,
      lookupOrderSourceRows: compiled.sourceRows,
      lookupOrderSourceSha256: compiled.sourceSha256,
      lookupOrderSurfaces: compiled.surfaces,
      lookupOrderClasses: compiled.physicalClasses,
      lookupOrderEquivalenceClasses: compiled.equivalenceClasses,
      lookupOrderComponents: compiled.components,
      lookupOrderCyclicComponents: compiled.cyclicComponents,
      lookupOrderEdges: compiled.edges,
      lookupOrderMaxRank: compiled.maxRank,
      lookupOrderProjectionSha256: compiled.sha256,
      lookupOrderExceptions: compiled.exceptions,
      lookupOrderExceptionClasses: compiled.exceptionClasses,
      lookupOrderExceptionLocators: compiled.exceptionLocators
    },
    collisions: [...collisionValues.values()].sort((left, right) =>
      compareText(sourceNativeCollisionKey(left), sourceNativeCollisionKey(right))),
    rows: reduction.rows,
    surfaces: reduction.surfaces,
    maxSurfaceRows: reduction.maxSurfaceRows
  };
}

/** Reduce the two exact M6 spools into pack-owned generated and collision facts. */
export function compileBoundedGeneratedProjection(
  input: BoundedGeneratedInput
): BoundedGeneratedProjection {
  const records = buildGeneratedRecords(input);
  const surface = buildLookupAndCollisions(input);
  return {
    generated: { ...records.values, ...surface.lookup },
    collisions: surface.collisions,
    occurrenceRows: surface.rows,
    occurrenceSurfaces: surface.surfaces,
    maxOccurrenceSurfaceRows: surface.maxSurfaceRows,
    semanticPathSha256: records.semanticPathSha256
  };
}
