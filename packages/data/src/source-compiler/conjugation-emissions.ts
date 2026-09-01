import { createHash } from 'node:crypto';
import {
  constructConjugation,
  DO_NOT_CONJUGATE,
  DO_NOT_CONJUGATE_SEQ,
  getConjRules,
  getPosIndex,
  SECONDARY_CONJUGATION_TYPES,
  SECONDARY_CONJUGATION_TYPES_FROM,
  type ConjugationRule
} from '../data/conj-rules.js';
import { isRootPayloadKanaSurface } from '../browser-pack/root-payload.js';
import {
  entryPartOfSpeech,
  type CanonicalEntry,
  type CanonicalRoute,
  type ConjugationProperty
} from './model.js';
import { conjugationPropertyKey } from './conjugation-identity.js';

const NON_CONJUGATING_POSITIONS = new Set(DO_NOT_CONJUGATE);
const NON_CONJUGATING_ENTRIES = new Set(DO_NOT_CONJUGATE_SEQ);
const SECONDARY_TYPES = new Set(SECONDARY_CONJUGATION_TYPES);
const SECONDARY_SOURCE_TYPES = new Set(SECONDARY_CONJUGATION_TYPES_FROM);

export interface EmissionRule extends ConjugationProperty {
  readonly order: number;
  readonly stem: number;
  readonly okuri: string;
  readonly euphr: string;
  readonly euphk: string;
}

export interface EmissionForm {
  readonly route: CanonicalRoute;
  readonly surface: string;
  readonly sourceText: string;
  readonly sourceEvent: number;
  readonly sourceOrdinal: number;
  /** Whether this form existed before secondary target expansion. */
  readonly secondaryEligible: boolean;
  /** Generated paired reading/form used only by physical target allocation. */
  readonly physicalCounterpart: string | null;
  readonly intermediate: string | null;
  readonly firstRule: EmissionRule;
  readonly secondRule: EmissionRule | null;
}

/** One semantic candidate before any physical target is chosen. */
export interface ConjugationEmission {
  readonly rootSeq: number;
  readonly rootEvent: number;
  readonly stage: 'primary' | 'secondary';
  readonly ordinal: number;
  readonly first: ConjugationProperty;
  readonly second: ConjugationProperty | null;
  readonly via: string | null;
  /** Complete pre-installation reading matrix used for physical identity and CSR surfaces. */
  readonly physicalForms: readonly EmissionForm[];
  /** Route/source-selected reverse-relation forms installed for analyzer lookup. */
  readonly forms: readonly EmissionForm[];
}

export interface ConjugationRelationSummary {
  readonly emissions: number;
  readonly surfaces: number;
  readonly uniqueKeys: number;
  readonly duplicates: number;
  readonly sha256: string;
}

interface PendingEmission {
  readonly property: ConjugationProperty;
  readonly physicalForms: EmissionForm[];
  readonly forms: EmissionForm[];
  hasKanaCandidate: boolean;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function sourceEvent(entry: CanonicalEntry): number {
  const events = [
    ...entry.kanji.map(form => form.sourceOrder.event),
    ...entry.kana.map(form => form.sourceOrder.event),
    ...entry.senses.flatMap(sense => sense.properties.map(property => property.sourceOrder.event))
  ];
  return events.length === 0 ? entry.source.ordinal : Math.min(...events);
}

function semanticPosition(position: string): string {
  return position === 'cop-da' ? 'cop' : position;
}

function effectiveRule(position: string, rule: ConjugationRule, peers: readonly ConjugationRule[]): EmissionRule {
  const sameType = peers.filter(peer => peer.conj === rule.conj);
  return {
    pos: semanticPosition(position),
    type: rule.conj,
    negative: sameType.some(peer => peer.neg) ? rule.neg : null,
    formal: sameType.some(peer => peer.fml) ? rule.fml : null,
    order: rule.onum,
    stem: rule.stem,
    okuri: rule.okuri,
    euphr: rule.euphr,
    euphk: rule.euphk
  };
}

function formKey(form: EmissionForm): string {
  return JSON.stringify([
    form.route,
    form.surface,
    form.sourceText,
    form.physicalCounterpart,
    form.intermediate,
    conjugationPropertyKey(form.firstRule),
    form.secondRule === null ? null : conjugationPropertyKey(form.secondRule)
  ]);
}

function addForm(forms: EmissionForm[], form: EmissionForm): void {
  const key = formKey(form);
  if (!forms.some(existing => formKey(existing) === key)) forms.push(form);
}

export function conjugationSourceKey(route: CanonicalRoute, text: string): string {
  return `${route}\u0000${text}`;
}

function sourceForms(entry: CanonicalEntry, selected?: ReadonlySet<string>): Array<{
  route: CanonicalRoute;
  text: string;
  event: number;
  ordinal: number;
  counterpart: string | null;
}> {
  return [
    ...entry.kanji.filter(form => form.conjugatable
      && (selected === undefined || selected.has(conjugationSourceKey('kanji', form.text))))
      .sort((left, right) => left.ordinal - right.ordinal || compareText(left.text, right.text))
      .map(form => ({
        route: 'kanji' as const, text: form.text,
        event: form.sourceOrder.event, ordinal: form.ordinal,
        counterpart: form.best ?? null
      })),
    ...entry.kana.filter(form => form.conjugatable
      && (selected === undefined || selected.has(conjugationSourceKey('kana', form.text))))
      .sort((left, right) => left.ordinal - right.ordinal || compareText(left.text, right.text))
      .map(form => ({
        route: 'kana' as const, text: form.text,
        event: form.sourceOrder.event, ordinal: form.ordinal,
        counterpart: form.best ?? null
      }))
  ];
}

export function emitPrimaryConjugations(
  entry: CanonicalEntry,
  options: {
    readonly positions?: readonly string[];
    readonly types?: ReadonlySet<number>;
    readonly sources?: ReadonlySet<string>;
    /** Exact source forms for each configured POS; prevents a root-wide cross-product. */
    readonly sourcesByPosition?: ReadonlyMap<string, ReadonlySet<string>>;
  } = {}
): ConjugationEmission[] {
  if (NON_CONJUGATING_ENTRIES.has(entry.seq)) return [];
  const original = new Set([...entry.kanji, ...entry.kana]
    .map(form => form.text.normalize('NFKC').trim()));
  const groups = new Map<string, PendingEmission>();

  for (const position of options.positions ?? entryPartOfSpeech(entry)) {
    if (NON_CONJUGATING_POSITIONS.has(position)) continue;
    const positionId = getPosIndex(position);
    if (positionId === undefined) continue;
    const rules = getConjRules(positionId);

    const selectedSources = options.sourcesByPosition?.get(position) ?? options.sources;
    if (options.sourcesByPosition && selectedSources === undefined) {
      throw new Error(`Configured position ${position} has no source-form selection`);
    }
    for (const source of sourceForms(entry)) {
      rules.forEach(rule => {
        if (options.types && !options.types.has(rule.conj)) return;
        if (position === 'v5r-i' && rule.conj === 52) return;
        const surface = constructConjugation(source.text, rule);
        if (original.has(surface.normalize('NFKC').trim())) return;
        const applied = effectiveRule(position, rule, rules);
        const key = conjugationPropertyKey(applied);
        const group = groups.get(key) ?? {
          property: applied,
          physicalForms: [],
          forms: [],
          hasKanaCandidate: false
        };
        if (source.route === 'kana') group.hasKanaCandidate = true;
        const form = {
          route: source.route,
          surface,
          sourceText: source.text,
          sourceEvent: source.event,
          sourceOrdinal: source.ordinal,
          secondaryEligible: true,
          physicalCounterpart: source.counterpart === null
            ? null : constructConjugation(source.counterpart, rule),
          intermediate: null,
          firstRule: applied,
          secondRule: null
        } satisfies EmissionForm;
        addForm(group.physicalForms, form);
        if (selectedSources !== undefined
          && !selectedSources.has(conjugationSourceKey(source.route, source.text))) {
          groups.set(key, group);
          return;
        }
        if (selectedSources !== undefined
          && isRootPayloadKanaSurface(surface) !== (source.route === 'kana')) {
          groups.set(key, group);
          return;
        }
        addForm(group.forms, form);
        groups.set(key, group);
      });
    }
  }

  return [...groups.values()].filter(group => group.hasKanaCandidate && group.physicalForms.length > 0)
    .map((group, ordinal) => ({
      rootSeq: entry.seq,
      rootEvent: sourceEvent(entry),
      stage: 'primary',
      ordinal,
      first: group.property,
      second: null,
      via: null,
      physicalForms: group.physicalForms,
      forms: group.forms
    }));
}

function isSecondarySource(emission: ConjugationEmission): boolean {
  return emission.stage === 'primary'
    && SECONDARY_SOURCE_TYPES.has(emission.first.type)
    && emission.first.pos !== 'vs-i'
    && emission.first.pos !== 'vs-s'
    && (emission.first.negative === false || emission.first.negative === null)
    && (emission.first.formal === false || emission.first.formal === null);
}

export function emitSecondaryConjugations(
  primary: ConjugationEmission,
  options: {
    readonly types?: ReadonlySet<number>;
    readonly enforceSurfaceRoute?: boolean;
  } = {}
): ConjugationEmission[] {
  if (!isSecondarySource(primary)) return [];
  const secondPosition = primary.first.type === 53 ? 'v5s' : 'v1';
  const positionId = getPosIndex(secondPosition);
  if (positionId === undefined) throw new Error(`No conjugation rules for ${secondPosition}`);
  const rules = getConjRules(positionId);
  const intermediates = new Set(primary.physicalForms
    .map(form => form.surface.normalize('NFKC').trim()));
  const installedSources = new Set(primary.forms);
  const groups = new Map<string, PendingEmission>();

  for (const source of primary.physicalForms) {
    for (const rule of rules) {
      if (!SECONDARY_TYPES.has(rule.conj) || (options.types && !options.types.has(rule.conj))) continue;
      const surface = constructConjugation(source.surface, rule);
      if (intermediates.has(surface.normalize('NFKC').trim())) continue;
      const applied = effectiveRule(secondPosition, rule, rules);
      const key = conjugationPropertyKey(applied);
      const group = groups.get(key) ?? {
        property: applied,
        physicalForms: [],
        forms: [],
        hasKanaCandidate: true
      };
      const form = {
        route: source.route,
        surface,
        sourceText: source.sourceText,
        sourceEvent: source.sourceEvent,
        sourceOrdinal: source.sourceOrdinal,
        secondaryEligible: false,
        physicalCounterpart: source.physicalCounterpart === null
          ? null : constructConjugation(source.physicalCounterpart, rule),
        intermediate: source.surface,
        firstRule: source.firstRule,
        secondRule: applied
      } satisfies EmissionForm;
      addForm(group.physicalForms, form);
      if (installedSources.has(source)
        && (!options.enforceSurfaceRoute
          || isRootPayloadKanaSurface(surface) === (source.route === 'kana'))) {
        addForm(group.forms, form);
      }
      groups.set(key, group);
    }
  }

  const via = conjugationEmissionKey(primary);
  return [...groups.values()].filter(group => group.hasKanaCandidate && group.physicalForms.length > 0)
    .map((group, ordinal) => ({
      rootSeq: primary.rootSeq,
      rootEvent: primary.rootEvent,
      stage: 'secondary',
      ordinal,
      first: primary.first,
      second: group.property,
      via,
      physicalForms: group.physicalForms,
      forms: group.forms
    }));
}

export function emitCanonicalConjugations(entry: CanonicalEntry): ConjugationEmission[] {
  const primary = emitPrimaryConjugations(entry);
  return [...primary, ...primary.flatMap(emission => emitSecondaryConjugations(emission))];
}

/** Stable lineage identity before route/source installation or chronological filtering. */
export function conjugationEmissionKey(emission: ConjugationEmission): string {
  return JSON.stringify([
    emission.rootSeq,
    conjugationPropertyKey(emission.first),
    emission.second === null ? null : conjugationPropertyKey(emission.second),
    emission.physicalForms.map(form => semanticConjugationKey(emission, form)).sort()
  ]);
}

/** Complete forward-relation identity for one route and generated surface. */
export function semanticConjugationKey(
  emission: ConjugationEmission,
  form: EmissionForm
): string {
  return JSON.stringify([
    emission.rootSeq,
    form.route,
    form.surface,
    form.sourceText,
    conjugationPropertyKey(emission.first),
    emission.second === null ? null : conjugationPropertyKey(emission.second),
    form.intermediate
  ]);
}

export function summarizeConjugationRelation(
  emissions: readonly ConjugationEmission[]
): ConjugationRelationSummary {
  const keys = emissions.flatMap(emission => emission.forms
    .map(form => semanticConjugationKey(emission, form))).sort();
  const unique = [...new Set(keys)];
  const hash = createHash('sha256');
  for (const key of unique) {
    const bytes = Buffer.from(key);
    const length = Buffer.allocUnsafe(4);
    length.writeUInt32LE(bytes.length);
    hash.update(length).update(bytes);
  }
  return {
    emissions: emissions.length,
    surfaces: keys.length,
    uniqueKeys: unique.length,
    duplicates: keys.length - unique.length,
    sha256: hash.digest('hex')
  };
}
