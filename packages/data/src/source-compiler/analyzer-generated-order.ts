import type {
  AnalyzerSupportGeneratedSource,
  AnalyzerSupportLookupOrderSource
} from '../browser-pack/analyzer-support.js';
import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import { isRootPayloadKanaSurface } from '../browser-pack/root-payload.js';
import {
  compileLookupOrders,
  type LookupOrderRow
} from '../browser-pack/lookup-order-compression.js';
import type { GeneratedLookupOccurrence } from './analyzer-generated-records.js';
import type { CanonicalEntry } from './model.js';

export type LookupClassPrecedence = ReadonlyMap<string, number>;

export interface SourceNativeLookupOrderProjection {
  readonly lookupOrders: AnalyzerSupportGeneratedSource['lookupOrders'];
  readonly lookupOrderSourceRows: number;
  readonly lookupOrderSourceSha256: string;
  readonly lookupOrderSurfaces: number;
  readonly lookupOrderClasses: number;
  readonly lookupOrderEquivalenceClasses: number;
  readonly lookupOrderComponents: number;
  readonly lookupOrderCyclicComponents: number;
  readonly lookupOrderEdges: number;
  readonly lookupOrderMaxRank: number;
  readonly lookupOrderProjectionSha256: string;
  readonly lookupOrderExceptions: AnalyzerSupportGeneratedSource['lookupOrderExceptions'];
  readonly lookupOrderExceptionClasses: number;
  readonly lookupOrderExceptionLocators: number;
}

interface Locator extends AnalyzerSupportLookupOrderSource {
  readonly targetSeq: number;
}

interface SurfaceClass {
  readonly targetSeq: number;
  readonly precedence: number;
  readonly locators: Map<string, Locator>;
}

function locatorKey(value: Pick<Locator, 'rootSeq' | 'firstAlias' | 'secondAlias'>): string {
  return `${value.rootSeq}\u0000${value.firstAlias ?? -1}\u0000${value.secondAlias ?? -1}`;
}

export function lookupClassKey(
  route: 'kana' | 'kanji',
  surface: string,
  targetSeq: number
): string {
  return JSON.stringify([route, surface, targetSeq]);
}

function surfaceKey(route: 'kana' | 'kanji', surface: string): string {
  return `${route}\u0000${surface}`;
}

function routeForCanonicalForm(route: 'kana' | 'kanji', text: string): 'kana' | 'kanji' | null {
  if (route === 'kana') return isRootPayloadKanaSurface(text) ? 'kana' : null;
  return isRootPayloadKanaSurface(text) ? null : 'kanji';
}

function tombstoneKeys(
  morphology: CompiledMorphologyArtifact,
  aliases: readonly number[]
): Set<string> {
  return new Set(morphology.tombstones.map(value => JSON.stringify([
    value.route,
    value.surface,
    value.rootSeq,
    aliases[value.firstRule],
    value.secondRule === null ? null : aliases[value.secondRule]
  ])));
}

/**
 * Compile exact source-native lookup order. `precedence` is a required dense
 * creation order for every ambiguous physical `(route,surface,target)` class;
 * higher values are newer and therefore receive lower lookup ranks.
 *
 * The qualified SCC compressor then derives sparse global ranks plus exact
 * local exceptions. Compression changes no source row or physical rank.
 */
export function compileSourceNativeLookupOrder(
  entries: readonly CanonicalEntry[],
  occurrences: readonly GeneratedLookupOccurrence[],
  morphology: CompiledMorphologyArtifact,
  ruleAliases: readonly number[],
  aliasCount: number,
  precedence: LookupClassPrecedence
): SourceNativeLookupOrderProjection {
  const located = new Map<string, Map<number, Map<string, Locator>>>();
  const generatedSurfaces = new Set<string>();
  const add = (
    route: 'kana' | 'kanji',
    surface: string,
    targetSeq: number,
    locator: Omit<Locator, 'rank' | 'targetSeq'>
  ): void => {
    const key = surfaceKey(route, surface);
    const targets = located.get(key) ?? new Map<number, Map<string, Locator>>();
    const locators = targets.get(targetSeq) ?? new Map<string, Locator>();
    const value = { ...locator, targetSeq, rank: 0 };
    locators.set(locatorKey(value), value);
    targets.set(targetSeq, locators);
    located.set(key, targets);
  };

  for (const entry of entries) {
    for (const form of entry.kana) {
      const route = routeForCanonicalForm('kana', form.text);
      if (route !== null) add(route, form.text, entry.seq, {
        rootSeq: entry.seq, firstAlias: null, secondAlias: null
      });
    }
    for (const form of entry.kanji) {
      const route = routeForCanonicalForm('kanji', form.text);
      if (route !== null) add(route, form.text, entry.seq, {
        rootSeq: entry.seq, firstAlias: null, secondAlias: null
      });
    }
  }

  const tombstones = tombstoneKeys(morphology, ruleAliases);
  for (const occurrence of occurrences) {
    if (occurrence.firstAlias < 0 || occurrence.firstAlias >= aliasCount
      || (occurrence.secondAlias !== null
        && (occurrence.secondAlias < 0 || occurrence.secondAlias >= aliasCount))) {
      throw new Error(`Generated lookup occurrence has an unknown alias for root ${occurrence.rootSeq}`);
    }
    if (tombstones.has(JSON.stringify([
      occurrence.route,
      occurrence.surface,
      occurrence.rootSeq,
      occurrence.firstAlias,
      occurrence.secondAlias
    ]))) continue;
    const key = surfaceKey(occurrence.route, occurrence.surface);
    generatedSurfaces.add(key);
    add(occurrence.route, occurrence.surface, occurrence.targetSeq, {
      rootSeq: occurrence.rootSeq,
      firstAlias: occurrence.firstAlias,
      secondAlias: occurrence.secondAlias
    });
  }

  const ambiguous = new Map<string, readonly SurfaceClass[]>();
  let physicalClasses = 0;
  for (const [key, targets] of located) {
    if (targets.size < 2 || !generatedSurfaces.has(key)) continue;
    const delimiter = key.indexOf('\u0000');
    const route = key.slice(0, delimiter);
    const surface = key.slice(delimiter + 1);
    if (route !== 'kana' && route !== 'kanji') throw new Error(`Invalid surface key ${key}`);
    const classes = [...targets].map(([targetSeq, locators]) => {
      const order = precedence.get(lookupClassKey(route, surface, targetSeq));
      if (!Number.isSafeInteger(order) || order! < 0) {
        throw new Error(`Missing lookup precedence for ${lookupClassKey(route, surface, targetSeq)}`);
      }
      return { targetSeq, precedence: order!, locators };
    }).sort((left, right) => right.precedence - left.precedence || left.targetSeq - right.targetSeq);
    if (new Set(classes.map(value => value.precedence)).size !== classes.length) {
      throw new Error(`Lookup precedence is not strict for ${JSON.stringify(key)}`);
    }
    if (classes.length > 0x40) throw new Error(`Lookup surface ${JSON.stringify(key)} exceeds 64 classes`);
    physicalClasses += classes.length;
    ambiguous.set(key, classes);
  }

  if (ambiguous.size === 0) {
    return {
      lookupOrders: [], lookupOrderSourceRows: 0, lookupOrderSourceSha256: '',
      lookupOrderSurfaces: 0, lookupOrderClasses: 0,
      lookupOrderEquivalenceClasses: 0, lookupOrderComponents: 0,
      lookupOrderCyclicComponents: 0, lookupOrderEdges: 0,
      lookupOrderMaxRank: 0, lookupOrderProjectionSha256: '',
      lookupOrderExceptions: [], lookupOrderExceptionClasses: 0,
      lookupOrderExceptionLocators: 0
    };
  }
  const rows: LookupOrderRow[] = [];
  for (const [key, classes] of ambiguous) {
    const delimiter = key.indexOf('\u0000');
    const routeValue = key.slice(0, delimiter);
    if (routeValue !== 'kana' && routeValue !== 'kanji') throw new Error(`Invalid surface key ${key}`);
    const surface = key.slice(delimiter + 1);
    classes.forEach((group, rank) => {
      for (const locator of group.locators.values()) {
        rows.push({
          rootSeq: locator.rootSeq,
          firstAlias: locator.firstAlias,
          secondAlias: locator.secondAlias,
          route: routeValue,
          surface,
          rank,
          physicalClasses,
          locatedClasses: physicalClasses,
          ambiguousSurfaces: ambiguous.size,
          loadedPatches: morphology.patches.length
        });
      }
    });
  }
  const compressed = compileLookupOrders(rows, aliasCount, morphology.patches.length);
  return {
    lookupOrders: compressed.values,
    lookupOrderSourceRows: compressed.sourceRows,
    lookupOrderSourceSha256: compressed.sourceSha256,
    lookupOrderSurfaces: compressed.surfaces,
    lookupOrderClasses: compressed.physicalClasses,
    lookupOrderEquivalenceClasses: compressed.equivalenceClasses,
    lookupOrderComponents: compressed.components,
    lookupOrderCyclicComponents: compressed.cyclicComponents,
    lookupOrderEdges: compressed.edges,
    lookupOrderMaxRank: compressed.maxRank,
    lookupOrderProjectionSha256: compressed.sha256,
    lookupOrderExceptions: compressed.exceptions,
    lookupOrderExceptionClasses: compressed.exceptionClasses,
    lookupOrderExceptionLocators: compressed.exceptionLocators
  };
}
