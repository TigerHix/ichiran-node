import {
  constructConjugation,
  SECONDARY_CONJUGATION_TYPES_FROM
} from '../data/conj-rules.js';
import type { CanonicalEntry } from './model.js';
import type { PhysicalTargetOrderCompatibilityRow } from './compatibility.js';
import {
  conjugationEmissionKey,
  type ConjugationEmission,
  type EmissionRule
} from './conjugation-emissions.js';
import {
  conjugationPropertyKey,
  emissionRuleKey
} from './conjugation-identity.js';

const SECONDARY_SOURCE_TYPES = new Set(SECONDARY_CONJUGATION_TYPES_FROM);

export interface PhysicalTarget {
  readonly seq: number;
  readonly kanji: readonly string[];
  readonly kana: readonly string[];
  /** Pre-chronology text rows and their direct reading/form pairing. */
  readonly secondaryForms: readonly PhysicalTargetSourceForm[];
  readonly conjugatable: boolean;
  readonly origin: 'lexical' | 'generated';
}

export interface PhysicalTargetSourceForm {
  readonly route: 'kana' | 'kanji';
  readonly text: string;
  readonly counterpart: string | null;
}

export interface PhysicalBinding {
  readonly emissionKey: string;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
}

export interface PhysicalMember {
  readonly key: string;
  readonly rootSeq: number;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
  readonly ordinalOnTarget: number;
}

export interface PhysicalPropertyMembership {
  readonly memberKey: string;
  readonly propertyKey: string;
}

export interface PhysicalConjugationResult {
  readonly targets: readonly PhysicalTarget[];
  readonly bindings: readonly PhysicalBinding[];
  readonly members: readonly PhysicalMember[];
  readonly properties: readonly PhysicalPropertyMembership[];
}

export interface StreamedPhysicalBinding {
  readonly ordinal: number;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
  /** First scheduled creation precedence for generated targets; null for lexical targets. */
  readonly targetCreationPrecedence: number | null;
}

interface MutablePhysicalMember {
  readonly parts: readonly [rootSeq: number, targetSeq: number, viaTargetSeq: number | null];
  readonly properties: Set<string>;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function unique(values: readonly string[]): string[] {
  return [...new Set(values)];
}

function constructPhysicalConjugation(word: string, rule: EmissionRule): string {
  return constructConjugation(word, {
    pos: 0,
    conj: rule.type,
    neg: rule.negative ?? false,
    fml: rule.formal ?? false,
    onum: rule.order,
    stem: rule.stem,
    okuri: rule.okuri,
    euphr: rule.euphr,
    euphk: rule.euphk
  });
}

function physicalFormKey(form: ConjugationEmission['physicalForms'][number]): string {
  return JSON.stringify([
    form.route,
    form.surface,
    form.sourceText,
    form.intermediate,
    emissionRuleKey(form.firstRule),
    form.secondRule === null ? null : emissionRuleKey(form.secondRule)
  ]);
}

function hasEvery(haystack: readonly string[], needles: readonly string[]): boolean {
  const values = new Set(haystack);
  return needles.every(value => values.has(value));
}

function routedTargetForms(forms: readonly ConjugationEmission['physicalForms'][number][]): {
  kanji: string[]; kana: string[];
} {
  const counterparts = forms.filter(form => form.physicalCounterpart !== null);
  return {
    kanji: unique([
      ...forms.filter(form => form.route === 'kanji').map(form => form.surface),
      ...counterparts.filter(form => form.route === 'kana').map(form => form.physicalCounterpart!)
    ]),
    kana: unique([
      ...forms.filter(form => form.route === 'kana').map(form => form.surface),
      ...counterparts.filter(form => form.route === 'kanji').map(form => form.physicalCounterpart!)
    ])
  };
}

function targetForms(emission: ConjugationEmission): {
  kanji: string[];
  kana: string[];
  secondaryForms: PhysicalTargetSourceForm[];
} {
  const physical = routedTargetForms(emission.physicalForms);
  const eligible = emission.physicalForms.filter(form => form.secondaryEligible);
  return {
    ...physical,
    secondaryForms: uniqueTargetSourceForms(eligible.map(form => ({
      route: form.route,
      text: form.surface,
      counterpart: form.physicalCounterpart
    })))
  };
}

function uniqueTargetSourceForms(
  forms: readonly PhysicalTargetSourceForm[]
): PhysicalTargetSourceForm[] {
  const seen = new Set<string>();
  return forms.filter(form => {
    const key = `${form.route}\u0000${form.text}\u0000${form.counterpart ?? ''}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

function compatibleTarget(
  target: PhysicalTarget,
  forms: { readonly kanji: readonly string[]; readonly kana: readonly string[] }
): boolean {
  return forms.kanji.length > 0
    ? hasEvery(target.kanji, forms.kanji) && hasEvery(target.kana, forms.kana)
    : target.kanji.length === 0 && hasEvery(target.kana, forms.kana);
}

function memberKey(rootSeq: number, targetSeq: number, viaTargetSeq: number | null): string {
  return JSON.stringify([rootSeq, targetSeq, viaTargetSeq]);
}

function targetIndexKeys(target: PhysicalTarget): string[] {
  return target.kanji.length > 0
    ? target.kanji.map(text => `kanji\u0000${text}`)
    : target.kana.map(text => `kana\u0000${text}`);
}

function emissionIndexKey(forms: { readonly kanji: readonly string[]; readonly kana: readonly string[] }): string {
  const text = forms.kanji[0] ?? forms.kana[0];
  if (text === undefined) throw new Error('Conjugation emission has no forms');
  return `${forms.kanji.length > 0 ? 'kanji' : 'kana'}\u0000${text}`;
}

export function lexicalPhysicalTarget(entry: CanonicalEntry): PhysicalTarget {
  const secondaryForms = uniqueTargetSourceForms([
    ...entry.kanji.filter(form => form.conjugatable).map(form => ({
      route: 'kanji' as const, text: form.text, counterpart: form.best
    })),
    ...entry.kana.filter(form => form.conjugatable).map(form => ({
      route: 'kana' as const, text: form.text, counterpart: form.best
    }))
  ]);
  return {
    seq: entry.seq,
    kanji: entry.kanji.map(form => form.text),
    kana: entry.kana.map(form => form.text),
    secondaryForms,
    conjugatable: [...entry.kanji, ...entry.kana].some(form => form.conjugatable),
    origin: 'lexical'
  };
}

/** The single owner of compatible-target search and generated target identity. */
class PhysicalTargetPool {
  readonly #targets: PhysicalTarget[];
  readonly #targetsBySeq = new Map<number, PhysicalTarget>();
  readonly #targetIndex = new Map<string, number[]>();
  readonly #creatorBySeq = new Map<number, number>();
  readonly #targetOrderCompatibility: readonly PhysicalTargetOrderCompatibilityRow[];
  #nextSeq: number;

  constructor(
    lexicalTargets: readonly PhysicalTarget[],
    firstGeneratedSeq: number,
    targetOrderCompatibility: readonly PhysicalTargetOrderCompatibilityRow[]
  ) {
    this.#targets = [...lexicalTargets].sort((left, right) => left.seq - right.seq);
    this.#nextSeq = firstGeneratedSeq;
    this.#targetOrderCompatibility = targetOrderCompatibility;
    for (const target of this.#targets) {
      if (this.#targetsBySeq.has(target.seq)) throw new Error(`Duplicate physical target ${target.seq}`);
      if (target.seq >= firstGeneratedSeq) {
        throw new Error(`Generated sequence ${firstGeneratedSeq} overlaps lexical target ${target.seq}`);
      }
      this.#add(target);
    }
  }

  #add(target: PhysicalTarget, creatorSeq?: number, beforeSeq?: number): void {
    this.#targetsBySeq.set(target.seq, target);
    if (creatorSeq !== undefined) this.#creatorBySeq.set(target.seq, creatorSeq);
    for (const key of targetIndexKeys(target)) {
      const values = this.#targetIndex.get(key) ?? [];
      const before = beforeSeq === undefined ? -1 : values.indexOf(beforeSeq);
      if (before === -1) values.push(target.seq);
      else values.splice(before, 0, target.seq);
      this.#targetIndex.set(key, values);
    }
  }

  allocate(
    emission: ConjugationEmission,
    viaTargetSeq: number | null
  ): { readonly target: PhysicalTarget; readonly created: boolean } {
    const forms = targetForms(emission);
    let reviewedPredecessor: number | undefined;
    for (const seq of this.#targetIndex.get(emissionIndexKey(forms)) ?? []) {
      if (seq === emission.rootSeq || seq === viaTargetSeq) continue;
      const candidate = this.#targetsBySeq.get(seq)!;
      if (!compatibleTarget(candidate, forms)) continue;
      const creatorSeq = this.#creatorBySeq.get(seq);
      if (emission.stage === 'primary' && creatorSeq !== undefined
        && this.#targetOrderCompatibility.some(row =>
          row.seq === emission.rootSeq
          && row.competingCreatorSeq === creatorSeq
          && row.property.pos === emission.first.pos
          && row.property.type === emission.first.type
          && row.property.negative === emission.first.negative
          && row.property.formal === emission.first.formal)) {
        reviewedPredecessor ??= seq;
        continue;
      }
      return { target: candidate, created: false };
    }
    const conjugatable = emission.stage === 'primary'
      && SECONDARY_SOURCE_TYPES.has(emission.first.type);
    const target: PhysicalTarget = {
      seq: this.#nextSeq++,
      kanji: forms.kanji,
      kana: forms.kana,
      secondaryForms: conjugatable ? forms.secondaryForms : [],
      conjugatable,
      origin: 'generated'
    };
    this.#targets.push(target);
    // A reviewed target-order row represents a qualified target that existed
    // before the skipped competitor. Preserve that one local relation so later
    // compatible emissions reuse the replacement as the qualified producer did.
    this.#add(target, emission.rootSeq, reviewedPredecessor);
    return { target, created: true };
  }

  finish(): readonly PhysicalTarget[] {
    return this.#targets;
  }

  target(seq: number): PhysicalTarget {
    const target = this.#targetsBySeq.get(seq);
    if (!target) throw new Error(`Unknown physical target ${seq}`);
    return target;
  }

  appendChronologicalForm(
    seq: number,
    route: 'kana' | 'kanji',
    baseText: string,
    text: string
  ): boolean {
    const target = this.target(seq);
    const forms = route === 'kana' ? target.kana : target.kanji;
    if (forms.includes(text)) return false;
    if (forms[0] !== baseText) {
      throw new Error(`Target ${seq} ${route} base changed from ${baseText} to ${forms[0] ?? '<empty>'}`);
    }
    (forms as string[]).push(text);
    return true;
  }
}

function expandSecondaryPhysicalForms(
  emission: ConjugationEmission,
  source: PhysicalTarget
): ConjugationEmission {
  if (emission.stage !== 'secondary') return emission;
  const representative = emission.physicalForms[0];
  if (!representative || representative.secondRule === null) {
    throw new Error(`Secondary emission ${emission.rootSeq} has no physical rule form`);
  }
  const rootSources = new Set(emission.physicalForms.flatMap(form =>
    form.intermediate === null ? [] : [form.intermediate]));
  const selectedSources = source.origin === 'lexical'
    ? source.secondaryForms
    : source.secondaryForms.filter(form =>
      rootSources.has(form.text)
      || (form.counterpart !== null && rootSources.has(form.counterpart)));
  // The historical loader selected target text rows through the current
  // root's direct reading/form pairs, then rejected a group with no kana row.
  if (!selectedSources.some(form => form.route === 'kana')) {
    if (emission.forms.length === 0) {
      throw new Error(`Rejected secondary emission ${emission.rootSeq} has no installed form`);
    }
    return emission.physicalForms.length === emission.forms.length
      ? emission : { ...emission, physicalForms: emission.forms };
  }
  const sources = selectedSources.filter(form => !rootSources.has(form.text));
  if (sources.length === 0) return emission;
  const rules = new Map<string, EmissionRule>();
  for (const form of emission.physicalForms) {
    if (form.secondRule === null) {
      throw new Error(`Secondary emission ${emission.rootSeq} has a primary physical form`);
    }
    rules.set(emissionRuleKey(form.secondRule), form.secondRule);
  }
  const intermediates = new Set(sources
    .map(form => form.text)
    .map(text => text.normalize('NFKC').trim()));
  const forms = [...emission.physicalForms];
  const keys = new Set(forms.map(physicalFormKey));
  const orderedSources = sources.map((form, ordinal) => ({ ...form, ordinal }));
  for (const rule of rules.values()) {
    for (const value of orderedSources) {
      const surface = constructPhysicalConjugation(value.text, rule);
      if (intermediates.has(surface.normalize('NFKC').trim())) continue;
      const form = {
        route: value.route,
        surface,
        sourceText: value.text,
        sourceEvent: representative.sourceEvent,
        sourceOrdinal: value.ordinal,
        secondaryEligible: false,
        physicalCounterpart: value.counterpart === null
          ? null : constructPhysicalConjugation(value.counterpart, rule),
        intermediate: value.text,
        firstRule: representative.firstRule,
        secondRule: rule
      };
      const key = physicalFormKey(form);
      if (keys.has(key)) continue;
      keys.add(key);
      forms.push(form);
    }
  }
  return forms.length === emission.physicalForms.length
    ? emission : { ...emission, physicalForms: forms };
}

/** Central deterministic owner for generated physical identity and lineage. */
export class PhysicalTargetAllocator {
  readonly #pool: PhysicalTargetPool;
  readonly #bindings: PhysicalBinding[] = [];
  readonly #targetByEmission = new Map<string, number>();
  readonly #members = new Map<string, MutablePhysicalMember>();

  constructor(
    lexicalTargets: readonly PhysicalTarget[],
    firstGeneratedSeq: number,
    targetOrderCompatibility: readonly PhysicalTargetOrderCompatibilityRow[] = []
  ) {
    this.#pool = new PhysicalTargetPool(
      lexicalTargets, firstGeneratedSeq, targetOrderCompatibility
    );
  }

  add(emission: ConjugationEmission): PhysicalBinding {
    const key = conjugationEmissionKey(emission);
    if (this.#targetByEmission.has(key)) throw new Error(`Duplicate conjugation emission ${key}`);
    const viaTargetSeq = emission.via === null ? null : this.#targetByEmission.get(emission.via);
    if (emission.via !== null && viaTargetSeq === undefined) {
      throw new Error(`Secondary emission has no assigned primary lineage: ${emission.via}`);
    }

    const physicalEmission = viaTargetSeq === undefined || viaTargetSeq === null
      ? emission : expandSecondaryPhysicalForms(emission, this.#pool.target(viaTargetSeq));
    const { target } = this.#pool.allocate(physicalEmission, viaTargetSeq ?? null);

    this.#targetByEmission.set(key, target.seq);
    const binding = { emissionKey: key, targetSeq: target.seq, viaTargetSeq: viaTargetSeq ?? null };
    this.#bindings.push(binding);
    const physicalMemberKey = memberKey(emission.rootSeq, target.seq, viaTargetSeq ?? null);
    const member = this.#members.get(physicalMemberKey) ?? {
      parts: [emission.rootSeq, target.seq, viaTargetSeq ?? null] as const,
      properties: new Set<string>()
    };
    member.properties.add(conjugationPropertyKey(emission.second ?? emission.first));
    this.#members.set(physicalMemberKey, member);
    return binding;
  }

  finish(): PhysicalConjugationResult {
    const memberRows = [...this.#members].sort((left, right) =>
      left[1].parts[1] - right[1].parts[1] || compareText(left[0], right[0]));
    const nextOrdinal = new Map<number, number>();
    const members: PhysicalMember[] = memberRows.map(([key, member]) => {
      const [rootSeq, targetSeq, viaTargetSeq] = member.parts;
      const ordinalOnTarget = nextOrdinal.get(targetSeq) ?? 0;
      nextOrdinal.set(targetSeq, ordinalOnTarget + 1);
      return { key, rootSeq, targetSeq, viaTargetSeq, ordinalOnTarget };
    });
    const properties = members.flatMap(member => [...this.#members.get(member.key)!.properties].sort()
      .map(value => ({ memberKey: member.key, propertyKey: value })));
    return {
      targets: this.#pool.finish(),
      bindings: this.#bindings,
      members,
      properties
    };
  }
}

/**
 * Bounded allocator for the release spool. It retains only primary lineage and
 * physical target state; bindings, members, properties and forms are written
 * by the caller as each scheduled emission is consumed.
 */
export class StreamingPhysicalTargetAllocator {
  readonly #pool: PhysicalTargetPool;
  readonly #primaryTargets = new Map<number, Map<number, number>>();
  readonly #generatedCreationPrecedence: number[] = [];
  readonly #firstGeneratedSeq: number;

  constructor(
    lexicalTargets: readonly PhysicalTarget[],
    firstGeneratedSeq: number,
    targetOrderCompatibility: readonly PhysicalTargetOrderCompatibilityRow[]
  ) {
    this.#pool = new PhysicalTargetPool(
      lexicalTargets, firstGeneratedSeq, targetOrderCompatibility
    );
    this.#firstGeneratedSeq = firstGeneratedSeq;
  }

  /** Expand a secondary CSR/physical matrix from the allocated primary target's full text set. */
  expandSecondary(
    emission: ConjugationEmission,
    rootSeq: number,
    firstAlias: number
  ): ConjugationEmission {
    if (emission.stage !== 'secondary') return emission;
    const targetSeq = this.#primaryTargets.get(rootSeq)?.get(firstAlias);
    if (targetSeq === undefined) {
      throw new Error(`Secondary emission ${rootSeq}/${firstAlias} has no primary target`);
    }
    return expandSecondaryPhysicalForms(emission, this.#pool.target(targetSeq));
  }

  add(value: {
    readonly ordinal: number;
    readonly firstAlias: number;
    readonly secondAlias: number | null;
    readonly creationPrecedence: number;
    readonly emission: ConjugationEmission;
  }): StreamedPhysicalBinding {
    const primary = this.#primaryTargets.get(value.emission.rootSeq) ?? new Map<number, number>();
    const viaTargetSeq = value.secondAlias === null ? null : primary.get(value.firstAlias);
    if (value.secondAlias !== null && viaTargetSeq === undefined) {
      throw new Error(
        `Secondary emission ${value.emission.rootSeq}/${value.firstAlias}/${value.secondAlias} has no primary lineage`
      );
    }
    const allocated = this.#pool.allocate(value.emission, viaTargetSeq ?? null);
    const target = allocated.target;
    let targetCreationPrecedence: number | null = null;
    if (target.origin === 'generated') {
      const index = target.seq - this.#firstGeneratedSeq;
      if (allocated.created) this.#generatedCreationPrecedence[index] = value.creationPrecedence;
      targetCreationPrecedence = this.#generatedCreationPrecedence[index];
      if (targetCreationPrecedence === undefined) {
        throw new Error(`Generated target ${target.seq} has no creation precedence`);
      }
    }
    if (value.secondAlias === null) {
      if (primary.has(value.firstAlias)) {
        throw new Error(`Duplicate primary semantic path ${value.emission.rootSeq}/${value.firstAlias}`);
      }
      primary.set(value.firstAlias, target.seq);
      this.#primaryTargets.set(value.emission.rootSeq, primary);
    }
    return {
      ordinal: value.ordinal,
      targetSeq: target.seq,
      viaTargetSeq: viaTargetSeq ?? null,
      targetCreationPrecedence
    };
  }

  finish(): readonly PhysicalTarget[] {
    return this.#pool.finish();
  }

  /** Apply one late addConjReading target mutation after all scheduled allocation. */
  appendChronologicalForm(
    targetSeq: number,
    route: 'kana' | 'kanji',
    baseText: string,
    text: string
  ): boolean {
    return this.#pool.appendChronologicalForm(targetSeq, route, baseText, text);
  }

  target(seq: number): PhysicalTarget {
    return this.#pool.target(seq);
  }
}

/**
 * Assigns storage targets after the semantic relation is complete. A physical
 * member may own several properties, but those memberships never create new
 * semantic emissions.
 */
export function assignPhysicalTargets(
  orderedEmissions: readonly ConjugationEmission[],
  lexicalTargets: readonly PhysicalTarget[],
  firstGeneratedSeq: number
): PhysicalConjugationResult {
  const allocator = new PhysicalTargetAllocator(lexicalTargets, firstGeneratedSeq);
  for (const emission of orderedEmissions) allocator.add(emission);
  return allocator.finish();
}
