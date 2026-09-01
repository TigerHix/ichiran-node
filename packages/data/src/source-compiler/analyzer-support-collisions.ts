import type { AnalyzerSupportCollisionSource } from '../browser-pack/analyzer-support.js';
import type {
  CompiledMorphologyArtifact,
  CompiledMorphologyRule
} from '../browser-pack/morphology-format.js';
import {
  conjugationEmissionKey,
  type ConjugationEmission,
  type EmissionRule
} from './conjugation-emissions.js';
import type { PhysicalConjugationResult } from './conjugation-emissions-physical.js';
import type { CanonicalEntry, CanonicalRoute, CanonicalSense } from './model.js';

const NONE = 0xffff_ffff;
const OBSOLETE = new Set(['arch', 'obsc', 'rare']);

// These are analyzer behavior, not database facts. They are the qualified
// 260118 lists after the reviewed skip-word addition and removal.
const SKIP_WORDS = new Set([
  2_822_120, 2_013_800, 2_108_590, 2_029_040, 2_428_180, 2_654_250,
  2_561_100, 2_210_270, 2_210_710, 2_257_550, 2_210_320, 2_017_560,
  2_394_890, 2_194_000, 2_568_000, 2_537_250, 2_760_890, 2_831_062,
  2_831_063, 2_029_030, 2_568_020, 900_000, 2_827_357
]);
const FINAL_PARTICLES = new Set([
  2_017_770, 2_425_930, 2_130_430, 2_029_130, 2_834_812,
  2_718_360, 2_201_380, 2_722_170, 2_751_630
]);
const SEMI_FINAL_PARTICLES = new Set([
  ...FINAL_PARTICLES, 2_029_120, 2_086_640, 2_029_110, 2_029_080, 2_029_100
]);
const NON_FINAL_PARTICLES = new Set([2_139_720]);
const COPULAE = new Set([2_089_020]);
const NO_KANJI_BREAK_PENALTY = new Set([
  1_169_870, 1_198_360, 1_277_450, 2_028_980,
  1_423_000, 1_164_690, 1_587_040, 2_827_864
]);

export interface CollisionEntryFacts {
  readonly nKanji: number;
  readonly nKana: number;
  readonly primaryNokanji: boolean;
  readonly archived: boolean;
  readonly preferKana: boolean;
  readonly preferKanaOnOrdinalZero: boolean;
  readonly pos: readonly string[];
  readonly skipWord: boolean;
  readonly finalParticle: boolean;
  readonly semiFinalParticle: boolean;
  readonly nonFinalParticle: boolean;
  readonly copula: boolean;
  readonly noKanjiBreakPenalty: boolean;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function isObsolete(sense: CanonicalSense): boolean {
  return sense.properties.some(property =>
    property.tag === 'misc' && OBSOLETE.has(property.text));
}

export function canonicalCollisionEntryFacts(entry: CanonicalEntry): CollisionEntryFacts {
  const positions = new Set<string>();
  let preferKana = false;
  let preferKanaOnOrdinalZero = false;
  for (const sense of entry.senses) {
    const obsolete = isObsolete(sense);
    for (const property of sense.properties) {
      if (property.tag === 'misc' && property.text === 'uk') {
        preferKana = true;
        preferKanaOnOrdinalZero ||= sense.ordinal === 0;
      }
      if (!obsolete && property.tag === 'pos') positions.add(property.text);
    }
  }
  const seq = entry.seq;
  return {
    nKanji: entry.kanji.length,
    nKana: entry.kana.length,
    primaryNokanji: entry.primaryNoKanji,
    archived: entry.senses.length > 0
      && entry.senses.every(isObsolete),
    preferKana,
    preferKanaOnOrdinalZero,
    pos: [...positions].sort(compareText),
    skipWord: SKIP_WORDS.has(seq),
    finalParticle: FINAL_PARTICLES.has(seq),
    semiFinalParticle: SEMI_FINAL_PARTICLES.has(seq),
    nonFinalParticle: NON_FINAL_PARTICLES.has(seq),
    copula: COPULAE.has(seq),
    noKanjiBreakPenalty: NO_KANJI_BREAK_PENALTY.has(seq)
  };
}

function ruleKey(rule: CompiledMorphologyRule): string {
  return JSON.stringify(rule);
}

function emissionRuleKey(rule: EmissionRule): string {
  return JSON.stringify({
    pos: rule.pos,
    type: rule.type,
    negative: rule.negative,
    formal: rule.formal,
    ordinal: rule.order,
    stem: rule.stem,
    okuri: rule.okuri,
    euphr: rule.euphr,
    euphk: rule.euphk
  });
}

function routeCode(route: CanonicalRoute): number {
  return route === 'kana' ? 0 : 1;
}

export function sourceNativeCollisionKey(value: Pick<
  AnalyzerSupportCollisionSource,
  'rootSeq' | 'ruleIds' | 'route' | 'surface'
>): string {
  return `${value.rootSeq.toString().padStart(10, '0')}\u0000${value.ruleIds[0].toString().padStart(10, '0')}\u0000${(value.ruleIds[1] ?? NONE).toString().padStart(10, '0')}\u0000${routeCode(value.route)}\u0000${value.surface}`;
}

function tombstoneKey(
  route: CanonicalRoute,
  surface: string,
  rootSeq: number,
  first: number,
  second: number | null
): string {
  return JSON.stringify([route, surface, rootSeq, first, second]);
}

/**
 * Project lexical-target conjugations into the exact collision facts consumed
 * by analyzer-support format v2. Physical sequence IDs remain isolated in the
 * target allocator; this module only joins its semantic bindings back to roots.
 */
export function compileAnalyzerSupportCollisions(
  entries: readonly CanonicalEntry[],
  emissions: readonly ConjugationEmission[],
  physical: PhysicalConjugationResult,
  morphology: CompiledMorphologyArtifact
): AnalyzerSupportCollisionSource[] {
  const roots = new Map<number, CanonicalEntry>();
  for (const entry of entries) {
    if (roots.has(entry.seq)) throw new RangeError(`Duplicate canonical root ${entry.seq}`);
    roots.set(entry.seq, entry);
  }

  const bindings = new Map<string, PhysicalConjugationResult['bindings'][number]>();
  for (const binding of physical.bindings) {
    if (bindings.has(binding.emissionKey)) {
      throw new Error(`Duplicate physical binding ${binding.emissionKey}`);
    }
    bindings.set(binding.emissionKey, binding);
  }
  const ruleIds = new Map(morphology.rules.map((rule, id) => [ruleKey(rule), id]));
  const tombstones = new Set(morphology.tombstones.map(value => tombstoneKey(
    value.route,
    value.surface,
    value.rootSeq,
    value.firstRule,
    value.secondRule
  )));
  const output = new Map<string, AnalyzerSupportCollisionSource>();

  for (const emission of emissions) {
    const emissionKey = conjugationEmissionKey(emission);
    const binding = bindings.get(emissionKey);
    if (!binding) throw new Error(`Conjugation emission has no physical binding ${emissionKey}`);
    const target = roots.get(binding.targetSeq);
    if (!target) continue;
    const facts = canonicalCollisionEntryFacts(target);

    for (const form of emission.forms) {
      const first = ruleIds.get(emissionRuleKey(form.firstRule));
      if (first === undefined) {
        throw new Error(`Collision emission has no morphology rule ${emissionKey}`);
      }
      let second: number | null = null;
      let rulePath: readonly [number] | readonly [number, number] = [first];
      if (form.secondRule !== null) {
        const secondId = ruleIds.get(emissionRuleKey(form.secondRule));
        if (secondId === undefined) {
          throw new Error(`Collision emission has no morphology rule ${emissionKey}`);
        }
        second = secondId;
        rulePath = [first, secondId];
      }
      if (tombstones.has(tombstoneKey(
        form.route,
        form.surface,
        emission.rootSeq,
        first,
        second
      ))) continue;

      const value: AnalyzerSupportCollisionSource = {
        rootSeq: emission.rootSeq,
        collisionSeq: target.seq,
        viaSeq: binding.viaTargetSeq,
        route: form.route,
        surface: form.surface,
        ruleIds: rulePath,
        ...facts
      };
      const key = sourceNativeCollisionKey(value);
      const prior = output.get(key);
      if (prior && JSON.stringify(prior) !== JSON.stringify(value)) {
        throw new Error(`Conflicting source-native collision ${key}`);
      }
      output.set(key, value);
    }
  }

  return [...output.values()].sort((left, right) =>
    compareText(sourceNativeCollisionKey(left), sourceNativeCollisionKey(right)));
}
