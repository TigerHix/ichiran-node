import type {
  AnalyzerSupportRoute,
  AnalyzerSupportSplitConjugationSource,
  AnalyzerSupportSplitPartSource
} from '../browser-pack/analyzer-support.js';
import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import { isRootPayloadKanaSurface } from '../browser-pack/root-payload.js';
import {
  enumerateAnnotationCandidates
} from './analyzer-support-annotations.js';
import type { SplitPartResolver } from './analyzer-support-annotation-model.js';
import {
  conjugationEmissionKey,
  type ConjugationEmission
} from './conjugation-emissions.js';
import {
  compiledMorphologyRuleKey,
  emissionRuleKey
} from './conjugation-identity.js';
import type {
  PhysicalConjugationResult,
  PhysicalTarget
} from './conjugation-emissions-physical.js';
import {
  SEGMENT_SPLIT_DECLARATIONS,
  SPLIT_DECLARATIONS
} from './analyzer-support-split-declarations.js';
import type { CanonicalEntry, CanonicalForm } from './model.js';
import {
  collectGeneratedLocatorsForTargets,
  collectGeneratedRulePathTargets
} from './generated-projection-reduce.js';
import type { GeneratedProjectionStreamResult } from './generated-projection-stream.js';

interface ResolverInput {
  readonly entries: readonly CanonicalEntry[];
  readonly morphology: CompiledMorphologyArtifact;
  readonly emissions: readonly ConjugationEmission[];
  readonly physical: PhysicalConjugationResult;
}

interface PreparedResolverInput {
  readonly entries: readonly CanonicalEntry[];
  readonly morphology: CompiledMorphologyArtifact;
  readonly targets: ReadonlyMap<number, PhysicalTarget>;
  readonly locators: ReadonlyMap<number, readonly AnalyzerSupportSplitConjugationSource[]>;
  readonly targetsByPath: ReadonlyMap<string, number>;
}

type StructuredSplitPart = Exclude<AnalyzerSupportSplitPartSource, string>;

interface DirectPart {
  readonly value: StructuredSplitPart;
  readonly sourceOrder: CanonicalForm['sourceOrder'] | null;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function compareDirectPart(left: DirectPart, right: DirectPart): number {
  if (left.sourceOrder === null) return right.sourceOrder === null ? 0 : 1;
  if (right.sourceOrder === null) return -1;
  return left.sourceOrder.event - right.sourceOrder.event
    || left.sourceOrder.ordinal - right.sourceOrder.ordinal
    || left.value.seq - right.value.seq
    || left.value.ord - right.value.ord;
}

function routeFor(text: string): AnalyzerSupportRoute {
  return isRootPayloadKanaSurface(text) ? 'kana' : 'kanji';
}

function sourceKey(seq: number, route: AnalyzerSupportRoute, text: string): string {
  return `${seq}\u0000${route}\u0000${text}`;
}

function ancestorKey(seq: number, route: AnalyzerSupportRoute, text: string): string {
  return sourceKey(seq, route, text);
}

function pathKey(
  rootSeq: number,
  firstRule: number,
  secondRule: number | null
): string {
  return JSON.stringify([rootSeq, firstRule, secondRule]);
}

function commonTags(form: CanonicalForm): string {
  return form.priorityTags.map(tag => `[${tag}]`).join('');
}

function propertyValue(key: string): {
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
} {
  const parsed: unknown = JSON.parse(key);
  if (!Array.isArray(parsed) || parsed.length !== 4
    || typeof parsed[0] !== 'string' || typeof parsed[1] !== 'number'
    || (parsed[2] !== null && typeof parsed[2] !== 'boolean')
    || (parsed[3] !== null && typeof parsed[3] !== 'boolean')) {
    throw new Error(`Invalid physical property ${key}`);
  }
  return {
    pos: parsed[0],
    type: parsed[1],
    negative: parsed[2],
    formal: parsed[3]
  };
}

function compareNullableBoolean(left: boolean | null, right: boolean | null): number {
  return (left === null ? -1 : Number(left)) - (right === null ? -1 : Number(right));
}

function generatedLocators(
  physical: PhysicalConjugationResult
): ReadonlyMap<number, readonly AnalyzerSupportSplitConjugationSource[]> {
  const propertiesByMember = new Map<string, string[]>();
  for (const membership of physical.properties) {
    const values = propertiesByMember.get(membership.memberKey) ?? [];
    values.push(membership.propertyKey);
    propertiesByMember.set(membership.memberKey, values);
  }
  const output = new Map<number, AnalyzerSupportSplitConjugationSource[]>();
  const seen = new Map<number, Set<string>>();
  for (const member of physical.members) {
    for (const key of propertiesByMember.get(member.key) ?? []) {
      const property = propertyValue(key);
      const value: AnalyzerSupportSplitConjugationSource = {
        from: member.rootSeq,
        via: member.viaTargetSeq !== null,
        ...property
      };
      const valueKey = JSON.stringify([
        value.from,
        value.via,
        value.pos,
        value.type,
        value.negative,
        value.formal
      ]);
      const targetSeen = seen.get(member.targetSeq) ?? new Set<string>();
      if (targetSeen.has(valueKey)) continue;
      targetSeen.add(valueKey);
      seen.set(member.targetSeq, targetSeen);
      const values = output.get(member.targetSeq) ?? [];
      values.push(value);
      output.set(member.targetSeq, values);
    }
  }
  for (const values of output.values()) values.sort((left, right) =>
    left.from - right.from
    || Number(left.via) - Number(right.via)
    || compareText(left.pos, right.pos)
    || left.type - right.type
    || compareNullableBoolean(left.negative, right.negative)
    || compareNullableBoolean(left.formal, right.formal));
  return output;
}

function referencedRoots(): ReadonlySet<number> {
  const roots = new Set<number>();
  for (const declaration of [...SPLIT_DECLARATIONS, ...SEGMENT_SPLIT_DECLARATIONS]) {
    for (const part of declaration.parts) {
      if (part.type !== 'part') continue;
      if (typeof part.seqs === 'number') roots.add(part.seqs);
      else for (const seq of part.seqs) if (typeof seq === 'number') roots.add(seq);
    }
  }
  return roots;
}

function directPart(
  entry: CanonicalEntry,
  route: AnalyzerSupportRoute,
  form: CanonicalForm,
  locators: readonly AnalyzerSupportSplitConjugationSource[] | undefined
): StructuredSplitPart {
  return {
    seq: entry.seq,
    route,
    text: form.text,
    best: form.best,
    ord: form.ordinal,
    common: form.common,
    commonTags: commonTags(form),
    conjugatable: form.conjugatable,
    nokanji: form.noKanji,
    generated: locators ?? null
  };
}

function generatedPart(
  target: PhysicalTarget,
  route: AnalyzerSupportRoute,
  text: string,
  sourceOrdinal: number,
  locators: readonly AnalyzerSupportSplitConjugationSource[] | undefined
): StructuredSplitPart {
  const forms = target[route];
  const targetOrdinal = forms.indexOf(text);
  return {
    seq: target.seq,
    route,
    text,
    best: null,
    // Manual morphology compatibility can name a surface that resolves to an
    // existing physical target without installing another target text row.
    // PostgreSQL exposed the originating text ordinal in that case.
    ord: targetOrdinal === -1 ? sourceOrdinal : targetOrdinal,
    common: null,
    commonTags: '',
    conjugatable: target.conjugatable,
    nokanji: false,
    generated: locators ?? null
  };
}

function createPreparedSplitPartResolver(input: PreparedResolverInput): SplitPartResolver {
  const entries = new Map(input.entries.map(entry => [entry.seq, entry]));
  const direct = new Map<string, DirectPart[]>();
  const addDirect = (
    part: StructuredSplitPart,
    sourceOrder: CanonicalForm['sourceOrder'] | null
  ): void => {
    const key = sourceKey(part.seq, part.route, part.text);
    const values = direct.get(key) ?? [];
    values.push({ value: part, sourceOrder });
    direct.set(key, values);
  };
  for (const entry of input.entries) {
    for (const form of entry.kanji) {
      addDirect(
        directPart(entry, 'kanji', form, input.locators.get(entry.seq)),
        form.sourceOrder
      );
    }
    for (const form of entry.kana) {
      addDirect(
        directPart(entry, 'kana', form, input.locators.get(entry.seq)),
        form.sourceOrder
      );
    }
  }

  const candidates = enumerateAnnotationCandidates(input.morphology, referencedRoots());
  const conjugated = new Map<string, AnalyzerSupportSplitPartSource[]>();
  const generatedSources = new Map<string, StructuredSplitPart>();
  for (const candidate of candidates) {
    if (candidate.ruleIds === null) continue;
    const targetSeq = input.targetsByPath.get(pathKey(
      candidate.rootSeq,
      candidate.ruleIds[0],
      candidate.ruleIds[1] ?? null
    ));
    if (targetSeq === undefined) continue;
    const target = input.targets.get(targetSeq);
    if (!target) throw new Error(`Split resolver path references missing target ${targetSeq}`);
    const lexical = entries.get(targetSeq);
    const lexicalForm = lexical?.[candidate.route].find(form => form.text === candidate.surface);
    const part = lexical && lexicalForm
      ? directPart(lexical, candidate.route, lexicalForm, input.locators.get(targetSeq))
      : generatedPart(
        target,
        candidate.route,
        candidate.surface,
        candidate.ord,
        input.locators.get(targetSeq)
      );
    const key = ancestorKey(candidate.rootSeq, candidate.route, candidate.surface);
    const values = conjugated.get(key) ?? [];
    if (!values.some(value => typeof value !== 'string' && value.seq === targetSeq)) {
      values.push(part);
    }
    conjugated.set(key, values);
    if (target.origin === 'generated') {
      generatedSources.set(sourceKey(targetSeq, candidate.route, candidate.surface), part);
    }
  }
  for (const part of generatedSources.values()) {
    addDirect(part, null);
  }

  return {
    find(text, seqs, includeConjugated) {
      const route = routeFor(text);
      const directCandidates: DirectPart[] = [];
      for (const seq of seqs) {
        directCandidates.push(...(direct.get(sourceKey(seq, route, text)) ?? []));
      }
      // The qualified query exposed text-row insertion order, not declaration seq order.
      const found = directCandidates.sort(compareDirectPart)[0];
      if (found) return found.value;
      if (!includeConjugated) return null;
      for (const seq of seqs) {
        const found = conjugated.get(ancestorKey(seq, route, text))?.[0];
        if (found) return found;
      }
      return null;
    }
  };
}

/**
 * Build the exact word resolver needed by split declarations. It indexes only
 * declaration-referenced roots and exposes no general dictionary query API.
 */
export function createSourceNativeSplitPartResolver(input: ResolverInput): SplitPartResolver {
  const targets = new Map(input.physical.targets.map(target => [target.seq, target]));
  const locators = generatedLocators(input.physical);
  const ruleIds = new Map(input.morphology.rules.map((rule, id) => [
    compiledMorphologyRuleKey(rule), id
  ]));
  const bindings = new Map(input.physical.bindings.map(binding => [binding.emissionKey, binding]));
  const targetsByPath = new Map<string, number>();
  for (const emission of input.emissions) {
    const binding = bindings.get(conjugationEmissionKey(emission));
    if (!binding) throw new Error(`Split resolver emission has no physical binding ${emission.rootSeq}`);
    for (const form of emission.forms) {
      const first = ruleIds.get(emissionRuleKey(form.firstRule));
      const second = form.secondRule === null
        ? null : ruleIds.get(emissionRuleKey(form.secondRule));
      if (first === undefined || (form.secondRule !== null && second === undefined)) {
        throw new Error(`Split resolver emission has no morphology rule ${emission.rootSeq}`);
      }
      const key = pathKey(emission.rootSeq, first, second ?? null);
      const prior = targetsByPath.get(key);
      if (prior !== undefined && prior !== binding.targetSeq) {
        throw new Error(`Split resolver path has multiple targets ${key}`);
      }
      targetsByPath.set(key, binding.targetSeq);
    }
  }
  return createPreparedSplitPartResolver({
    entries: input.entries,
    morphology: input.morphology,
    targets,
    locators,
    targetsByPath
  });
}

/** Build the split resolver from the bounded generated projection spools. */
export function createBoundedSourceNativeSplitPartResolver(input: {
  readonly entries: readonly CanonicalEntry[];
  readonly morphology: CompiledMorphologyArtifact;
  readonly projection: GeneratedProjectionStreamResult;
}): SplitPartResolver {
  const roots = referencedRoots();
  const paths = collectGeneratedRulePathTargets(
    input.projection.pathsPath,
    input.projection.occurrencesPath,
    roots
  );
  const targetsByPath = new Map(paths.map(value => [
    pathKey(value.rootSeq, value.firstRule, value.secondRule), value.targetSeq
  ]));
  // Explicit lexical part seqs can themselves be conjugation targets whose
  // lineage root is not named by a split declaration.
  const selectedTargets = new Set([
    ...roots,
    ...paths.map(value => value.targetSeq)
  ]);
  const targets = new Map<number, PhysicalTarget>();
  for (const target of input.projection.targets) {
    if (selectedTargets.has(target.seq)) targets.set(target.seq, target);
  }
  const locators = collectGeneratedLocatorsForTargets(
    input.projection.pathsPath,
    selectedTargets,
    input.projection.aliasProperties
  );
  return createPreparedSplitPartResolver({
    entries: input.entries,
    morphology: input.morphology,
    targets,
    locators,
    targetsByPath
  });
}
