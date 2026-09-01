import { createHash } from 'node:crypto';
import type {
  AnalyzerSupportGeneratedMemberSource,
  AnalyzerSupportGeneratedRecordSource
} from '../browser-pack/analyzer-support.js';
import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import {
  conjugationEmissionKey,
  type ConjugationEmission
} from './conjugation-emissions.js';
import type {
  PhysicalConjugationResult,
  PhysicalTarget
} from './conjugation-emissions-physical.js';
import type { CanonicalEntry, ConjugationProperty } from './model.js';
import {
  orderConjugationEmissions,
  type EmissionPrecedence
} from './conjugation-emission-order.js';

export interface GeneratedLookupOccurrence {
  readonly rootSeq: number;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
  readonly targetSeq: number;
  readonly route: 'kana' | 'kanji';
  readonly surface: string;
  readonly kind: 'emission' | 'patch';
  readonly precedence: number;
}

export interface GeneratedRecordProjection {
  readonly ruleAliases: readonly number[];
  readonly aliasCount: number;
  readonly records: readonly AnalyzerSupportGeneratedRecordSource[];
  readonly semanticPaths: number;
  readonly matchedPaths: number;
  readonly countExceptions: number;
  readonly physicalGroups: number;
  readonly physicalMembers: number;
  readonly propertyOverrides: number;
  readonly maxMemberOrd: number;
  readonly maxViaMemberOrd: number;
  readonly maxPropOrd: number;
  readonly projectionSha256: string;
  readonly occurrences: readonly GeneratedLookupOccurrence[];
}

interface SemanticPath {
  readonly emission: ConjugationEmission;
  readonly target: PhysicalTarget;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
}

interface MutableTargetForms {
  readonly kanji: Set<string>;
  readonly kana: Set<string>;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function semanticPropertyKey(value: ConjugationProperty): string {
  return JSON.stringify([value.pos, value.type, value.negative, value.formal]);
}

function recordKey(value: {
  readonly rootSeq: number;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
}): string {
  return `${value.rootSeq.toString().padStart(10, '0')}\u0000${value.firstAlias
    .toString().padStart(5, '0')}\u0000${(value.secondAlias ?? -1).toString().padStart(5, '0')}`;
}

function targetRecordKey(value: SemanticPath): string {
  return `${recordKey({
    rootSeq: value.emission.rootSeq,
    firstAlias: value.firstAlias,
    secondAlias: value.secondAlias
  })}\u0000${value.targetSeq.toString().padStart(10, '0')}`;
}

function memberIdentity(rootSeq: number, targetSeq: number, viaTargetSeq: number | null): string {
  return JSON.stringify([rootSeq, targetSeq, viaTargetSeq]);
}

function sameProperty(
  left: AnalyzerSupportGeneratedMemberSource['property'],
  right: AnalyzerSupportGeneratedMemberSource['property']
): boolean {
  return left.posId === right.posId
    && left.type === right.type
    && left.negative === right.negative
    && left.formal === right.formal;
}

function propertyMatches(
  semantic: ConjugationProperty,
  physical: ConjugationProperty
): boolean {
  return semantic.pos === physical.pos
    && semantic.type === physical.type
    && (semantic.negative === null || semantic.negative === physical.negative)
    && (semantic.formal === null || semantic.formal === physical.formal);
}

function projectRuleAliases(morphology: CompiledMorphologyArtifact): {
  readonly aliases: readonly number[];
  readonly aliasByProperty: ReadonlyMap<string, number>;
  readonly properties: readonly ConjugationProperty[];
} {
  const keys = [...new Set(morphology.rules.map(semanticPropertyKey))].sort(compareText);
  const aliasByProperty = new Map(keys.map((key, alias) => [key, alias]));
  const aliases = morphology.rules.map(rule => {
    const alias = aliasByProperty.get(semanticPropertyKey(rule));
    if (alias === undefined) throw new Error('Morphology rule has no semantic alias');
    return alias;
  });
  const propertyByAlias = new Map<number, ConjugationProperty>();
  for (const [ruleId, alias] of aliases.entries()) {
    const rule = morphology.rules[ruleId]!;
    propertyByAlias.set(alias, {
      pos: rule.pos,
      type: rule.type,
      negative: rule.negative,
      formal: rule.formal
    });
  }
  return {
    aliases,
    aliasByProperty,
    properties: keys.map((_, alias) => propertyByAlias.get(alias)!)
  };
}

function targetForms(targets: readonly PhysicalTarget[]): Map<number, MutableTargetForms> {
  return new Map(targets.map(target => [target.seq, {
    kanji: new Set(target.kanji),
    kana: new Set(target.kana)
  }]));
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

/**
 * Projects semantic emissions and their centrally allocated physical targets
 * into the generated-entry facts consumed by analyzer annotations. PostgreSQL
 * identities never enter this boundary; target sequence values are transient
 * joins and are absent from the returned records and digest.
 */
export function compileGeneratedRecords(
  entries: readonly CanonicalEntry[],
  emissions: readonly ConjugationEmission[],
  physical: PhysicalConjugationResult,
  morphology: CompiledMorphologyArtifact,
  precedence: EmissionPrecedence
): GeneratedRecordProjection {
  const entriesBySeq = new Map(entries.map(entry => [entry.seq, entry]));
  if (entriesBySeq.size !== entries.length) throw new Error('Canonical entries contain duplicate sequence ids');
  const targetsBySeq = new Map(physical.targets.map(target => [target.seq, target]));
  if (targetsBySeq.size !== physical.targets.length) throw new Error('Physical targets contain duplicate sequence ids');
  const bindings = new Map(physical.bindings.map(binding => [binding.emissionKey, binding]));
  if (bindings.size !== physical.bindings.length) throw new Error('Physical bindings contain duplicate emissions');

  const aliasProjection = projectRuleAliases(morphology);
  const orderedEmissions = orderConjugationEmissions(emissions, precedence);
  const paths: SemanticPath[] = orderedEmissions.map(emission => {
    const binding = bindings.get(conjugationEmissionKey(emission));
    if (!binding) throw new Error(`Emission has no physical binding for root ${emission.rootSeq}`);
    const target = targetsBySeq.get(binding.targetSeq);
    if (!target) throw new Error(`Binding references missing physical target ${binding.targetSeq}`);
    const firstAlias = aliasProjection.aliasByProperty.get(semanticPropertyKey(emission.first));
    const secondAlias = emission.second === null
      ? null : aliasProjection.aliasByProperty.get(semanticPropertyKey(emission.second));
    if (firstAlias === undefined || (emission.second !== null && secondAlias === undefined)) {
      throw new Error(`Emission for root ${emission.rootSeq} has no morphology alias`);
    }
    return {
      emission,
      target,
      targetSeq: target.seq,
      viaTargetSeq: binding.viaTargetSeq,
      firstAlias,
      secondAlias: secondAlias ?? null
    };
  });

  const pathsByRecord = new Map<string, SemanticPath[]>();
  const targetByRecord = new Map<string, number>();
  for (const path of paths) {
    const key = recordKey({
      rootSeq: path.emission.rootSeq,
      firstAlias: path.firstAlias,
      secondAlias: path.secondAlias
    });
    const priorTarget = targetByRecord.get(key);
    if (priorTarget !== undefined && priorTarget !== path.targetSeq) {
      throw new Error(`Generated semantic record ${JSON.stringify(key)} maps to multiple targets`);
    }
    targetByRecord.set(key, path.targetSeq);
    const values = pathsByRecord.get(key) ?? [];
    values.push(path);
    pathsByRecord.set(key, values);
  }

  const effectiveForms = targetForms(physical.targets);
  const occurrences: GeneratedLookupOccurrence[] = [];
  for (const [emissionOrdinal, path] of paths.entries()) {
    for (const form of path.emission.forms) {
      occurrences.push({
        rootSeq: path.emission.rootSeq,
        firstAlias: path.firstAlias,
        secondAlias: path.secondAlias,
        targetSeq: path.targetSeq,
        route: form.route,
        surface: form.surface,
        kind: 'emission',
        precedence: emissionOrdinal
      });
    }
  }

  const patchKeys = new Set<string>();
  for (const patch of morphology.patches) {
    const firstAlias = aliasProjection.aliases[patch.firstRule];
    const secondAlias = patch.secondRule === null ? null : aliasProjection.aliases[patch.secondRule];
    if (firstAlias === undefined || (patch.secondRule !== null && secondAlias === undefined)) {
      throw new Error(`Manual patch for root ${patch.rootSeq} references an unknown rule`);
    }
    const key = recordKey({ rootSeq: patch.rootSeq, firstAlias, secondAlias: secondAlias ?? null });
    const targetSeq = targetByRecord.get(key);
    if (targetSeq === undefined) {
      throw new Error(`Manual patch for root ${patch.rootSeq} has no emitted semantic target`);
    }
    const forms = effectiveForms.get(targetSeq);
    if (!forms) throw new Error(`Manual patch references missing target ${targetSeq}`);
    forms[patch.route].add(patch.surface);
    const occurrenceKey = JSON.stringify([
      patch.route, patch.surface, patch.rootSeq, firstAlias, secondAlias ?? null, targetSeq
    ]);
    if (patchKeys.has(occurrenceKey)) continue;
    patchKeys.add(occurrenceKey);
    occurrences.push({
      rootSeq: patch.rootSeq,
      firstAlias,
      secondAlias: secondAlias ?? null,
      targetSeq,
      route: patch.route,
      surface: patch.surface,
      kind: 'patch',
      precedence: orderedEmissions.length + patchKeys.size - 1
    });
  }

  const propertyValues = new Map<string, ConjugationProperty>();
  for (const path of paths) {
    propertyValues.set(semanticPropertyKey(path.emission.first), path.emission.first);
    if (path.emission.second !== null) {
      propertyValues.set(semanticPropertyKey(path.emission.second), path.emission.second);
    }
  }
  for (const property of physical.properties) {
    if (!propertyValues.has(property.propertyKey)) {
      throw new Error(`Physical member has unknown property ${property.propertyKey}`);
    }
  }
  const membersByIdentity = new Map(physical.members.map(member => [member.key, member]));
  const memberCounts = new Map<number, number>();
  for (const member of physical.members) {
    memberCounts.set(member.targetSeq, (memberCounts.get(member.targetSeq) ?? 0) + 1);
  }
  const propertyKeysByMember = new Map<string, string[]>();
  for (const membership of physical.properties) {
    const values = propertyKeysByMember.get(membership.memberKey) ?? [];
    values.push(membership.propertyKey);
    propertyKeysByMember.set(membership.memberKey, values);
  }
  for (const values of propertyKeysByMember.values()) values.sort(compareText);

  const semanticRecordTargets = new Map<number, string[]>();
  for (const [key, targetSeq] of targetByRecord) {
    const values = semanticRecordTargets.get(targetSeq) ?? [];
    values.push(key);
    semanticRecordTargets.set(targetSeq, values);
  }
  const groupedTargets = [...semanticRecordTargets]
    .filter(([, keys]) => keys.length > 1)
    .map(([targetSeq, keys]) => ({ targetSeq, firstKey: [...keys].sort(compareText)[0]! }))
    .sort((left, right) => compareText(left.firstKey, right.firstKey));
  const groupIds = new Map(groupedTargets.map((value, index) => [value.targetSeq, index + 1]));

  const positions = new Map(morphology.positions.map((pos, id) => [pos, id]));
  let physicalMembers = 0;
  let propertyOverrides = 0;
  let maxMemberOrd = 0;
  let maxViaMemberOrd = 0;
  let maxPropOrd = 0;
  const allRecords: AnalyzerSupportGeneratedRecordSource[] = [];
  const matchedPathKeys = new Set<string>();

  for (const [key, values] of [...pathsByRecord].sort((left, right) => compareText(left[0], right[0]))) {
    const path = values[0]!;
    const root = entriesBySeq.get(path.emission.rootSeq);
    const forms = effectiveForms.get(path.targetSeq);
    if (!root || !forms) throw new Error(`Generated record ${JSON.stringify(key)} has incomplete counts`);
    const finalProperty = path.emission.second ?? path.emission.first;
    const defaultPosId = positions.get(finalProperty.pos);
    if (defaultPosId === undefined) throw new Error(`Unknown morphology position ${finalProperty.pos}`);
    const defaultProperty = {
      posId: defaultPosId,
      type: finalProperty.type,
      negative: finalProperty.negative,
      formal: finalProperty.formal
    };

    const memberRows = new Map<string, AnalyzerSupportGeneratedMemberSource>();
    let viaTargetMemberCount = 0;
    for (const value of values) {
      const finalMember = membersByIdentity.get(memberIdentity(
        value.emission.rootSeq,
        value.targetSeq,
        value.viaTargetSeq
      ));
      if (!finalMember) throw new Error(`Generated record ${JSON.stringify(key)} has no physical member`);
      const viaMember = value.viaTargetSeq === null ? null : membersByIdentity.get(memberIdentity(
        value.emission.rootSeq,
        value.viaTargetSeq,
        null
      ));
      if (value.viaTargetSeq !== null && !viaMember) {
        throw new Error(`Generated record ${JSON.stringify(key)} has no prefix member`);
      }
      viaTargetMemberCount = Math.max(
        viaTargetMemberCount,
        value.viaTargetSeq === null ? 0 : memberCounts.get(value.viaTargetSeq) ?? 0
      );
      const propertyKeys = propertyKeysByMember.get(finalMember.key) ?? [];
      for (const [propOrd, propertyKey] of propertyKeys.entries()) {
        const property = propertyValues.get(propertyKey)!;
        const posId = positions.get(property.pos);
        if (posId === undefined) throw new Error(`Unknown physical position ${property.pos}`);
        if (propertyMatches(finalProperty, property)) {
          matchedPathKeys.add(`${targetRecordKey(value)}\u0000${finalMember.key}\u0000${propertyKey}`);
        }
        const projected = {
          property: {
            posId,
            type: property.type,
            negative: property.negative,
            formal: property.formal
          },
          memberOrd: finalMember.ordinalOnTarget,
          propOrd,
          viaMemberOrd: viaMember?.ordinalOnTarget ?? null
        };
        memberRows.set(JSON.stringify(projected), projected);
      }
    }
    const members = [...memberRows.values()].sort((left, right) =>
      left.memberOrd - right.memberOrd
      || left.propOrd - right.propOrd
      || (left.viaMemberOrd ?? -1) - (right.viaMemberOrd ?? -1));
    if (members.length === 0) throw new Error(`Generated record ${JSON.stringify(key)} has no properties`);
    const physicalGroup = groupIds.get(path.targetSeq) ?? null;
    const needsMembers = physicalGroup !== null
      || members.length !== 1
      || members[0]!.memberOrd !== 0
      || (members[0]!.viaMemberOrd !== null && viaTargetMemberCount > 1)
      || !sameProperty(members[0]!.property, defaultProperty);
    if (needsMembers) {
      physicalMembers += members.length;
      for (const member of members) {
        if (!sameProperty(member.property, defaultProperty)) propertyOverrides++;
        maxMemberOrd = Math.max(maxMemberOrd, member.memberOrd);
        maxViaMemberOrd = Math.max(maxViaMemberOrd, member.viaMemberOrd ?? 0);
        maxPropOrd = Math.max(maxPropOrd, member.propOrd);
      }
    }
    const counts = [forms.kanji.size, forms.kana.size] as const;
    const countException = counts[0] !== root.kanji.length || counts[1] !== root.kana.length;
    allRecords.push({
      rootSeq: path.emission.rootSeq,
      firstAlias: path.firstAlias,
      secondAlias: path.secondAlias,
      counts: countException ? counts : null,
      physicalGroup,
      members: needsMembers ? members : null
    });
  }

  const records = allRecords.filter(record => record.counts !== null || record.members !== null);
  return {
    ruleAliases: aliasProjection.aliases,
    aliasCount: aliasProjection.properties.length,
    records,
    semanticPaths: pathsByRecord.size,
    matchedPaths: matchedPathKeys.size + patchKeys.size,
    countExceptions: records.filter(record => record.counts !== null).length,
    physicalGroups: groupIds.size,
    physicalMembers,
    propertyOverrides,
    maxMemberOrd,
    maxViaMemberOrd,
    maxPropOrd,
    projectionSha256: projectionDigest(records),
    occurrences
  };
}
