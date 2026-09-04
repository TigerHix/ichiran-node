import { createHash } from 'node:crypto';
import {
  ANALYZER_SUPPORT_SECTION_ID,
  encodePack,
  MORPHOLOGY_SECTION_ID,
  ROOT_PAYLOAD_SECTION_ID,
  SURFACE_INDEX_SECTION_ID
} from '@ichiran/core/compiler';
import {
  ANALYZER_ANNOTATIONS_SECTION_ID,
  buildAnalyzerAnnotations
} from '../browser-pack/analyzer-annotations.js';
import {
  buildAnalyzerSupportCore,
  type AnalyzerSupportGeneratedSource,
  type AnalyzerSupportSource
} from '../browser-pack/analyzer-support.js';
import { buildLexiconStore } from '../browser-pack/lexicon.js';
import {
  buildLocaleGlossStore,
  type LocaleGlossEntrySource
} from '../browser-pack/locale-gloss.js';
import {
  buildMorphology,
  type MorphologySource
} from '../browser-pack/morphology-compiler.js';
import { buildRootPayload } from '../browser-pack/root-payload.js';
import {
  assertLocaleGlossEntriesMatchLexicon,
  canonicalEnglishLocaleEntries,
  canonicalLexiconEntries,
  canonicalRootPayloadSource
} from './pack-input.js';
import {
  canonicalSurfaceIndexRows,
  encodeSurfaceIndexTsv,
  type MorphologySurface
} from './surface-index-input.js';
import type { CanonicalEntry } from './model.js';
import type { ConjugationRulePaths } from '../data/conj-rules.js';

export interface SourceCompilerSemanticInput {
  readonly entries: readonly CanonicalEntry[];
  readonly zhHans: readonly LocaleGlossEntrySource[];
  readonly morphology: MorphologySource;
  readonly morphologySurfaces: Iterable<MorphologySurface>;
  readonly support: AnalyzerSupportSource;
  readonly conjugationRules: ConjugationRulePaths;
}

export type SourceCompilerBinaryInput = Omit<SourceCompilerSemanticInput, 'morphologySurfaces'>;

export interface SourceCompilerBinarySections {
  readonly root: ReturnType<typeof buildRootPayload>;
  readonly lexicon: ReturnType<typeof buildLexiconStore>;
  readonly locales: Readonly<{
    readonly en: ReturnType<typeof buildLocaleGlossStore>;
    readonly 'zh-Hans': ReturnType<typeof buildLocaleGlossStore>;
  }>;
  readonly morphology: ReturnType<typeof buildMorphology>;
  readonly support: ReturnType<typeof buildAnalyzerSupportCore>;
  readonly annotations: ReturnType<typeof buildAnalyzerAnnotations>;
}

export interface SourceCompilerSections extends SourceCompilerBinarySections {
  readonly surfaceTsv: Uint8Array;
}

const EMPTY_GENERATED: AnalyzerSupportGeneratedSource = {
  ruleAliases: [], aliasCount: 0, records: [], semanticPaths: 0, matchedPaths: 0,
  countExceptions: 0, lookupOrders: [], lookupOrderSourceRows: 0,
  lookupOrderSourceSha256: '', lookupOrderSurfaces: 0, lookupOrderClasses: 0,
  lookupOrderEquivalenceClasses: 0, lookupOrderComponents: 0,
  lookupOrderCyclicComponents: 0, lookupOrderEdges: 0, lookupOrderMaxRank: 0,
  lookupOrderProjectionSha256: '', lookupOrderExceptions: [],
  lookupOrderExceptionClasses: 0, lookupOrderExceptionLocators: 0,
  physicalGroups: 0, physicalMembers: 0, propertyOverrides: 0,
  maxMemberOrd: 0, maxViaMemberOrd: 0, maxPropOrd: 0, projectionSha256: ''
};

export function buildSourceCompilerBinarySections(
  input: SourceCompilerBinaryInput
): SourceCompilerBinarySections {
  const root = buildRootPayload(canonicalRootPayloadSource(input.entries));
  const lexiconEntries = canonicalLexiconEntries(input.entries);
  const lexicon = buildLexiconStore(lexiconEntries);
  const lexiconSha256 = createHash('sha256').update(lexicon.bytes).digest('hex');
  const englishEntries = canonicalEnglishLocaleEntries(input.entries);
  assertLocaleGlossEntriesMatchLexicon(lexiconEntries, englishEntries, 'en');
  assertLocaleGlossEntriesMatchLexicon(lexiconEntries, input.zhHans, 'zh-Hans');
  const locales = {
    en: buildLocaleGlossStore({
      locale: 'en',
      lexiconSha256,
      entries: englishEntries
    }),
    'zh-Hans': buildLocaleGlossStore({
      locale: 'zh-Hans',
      lexiconSha256,
      entries: input.zhHans
    })
  } as const;
  const morphology = buildMorphology(input.morphology, {
    conjugationRules: input.conjugationRules
  });
  const support = buildAnalyzerSupportCore(input.support);
  const generated = input.support.generated ?? EMPTY_GENERATED;
  const annotations = buildAnalyzerAnnotations(input.support.splits, input.support.hints, generated);
  return { root, lexicon, locales, morphology, support, annotations };
}

export function buildSourceCompilerSections(input: SourceCompilerSemanticInput): SourceCompilerSections {
  const sections = buildSourceCompilerBinarySections(input);
  const surfaceTsv = encodeSurfaceIndexTsv(canonicalSurfaceIndexRows(
    input.entries,
    input.morphologySurfaces
  ));
  return { ...sections, surfaceTsv };
}

export function buildSourceCompilerHotPack(
  sections: SourceCompilerBinarySections,
  surfaceIndex: Uint8Array
): Uint8Array {
  return encodePack([
    { id: SURFACE_INDEX_SECTION_ID, bytes: surfaceIndex },
    { id: ROOT_PAYLOAD_SECTION_ID, bytes: sections.root.bytes },
    { id: MORPHOLOGY_SECTION_ID, bytes: sections.morphology.bytes },
    { id: ANALYZER_SUPPORT_SECTION_ID, bytes: sections.support.bytes },
    { id: ANALYZER_ANNOTATIONS_SECTION_ID, bytes: sections.annotations.bytes }
  ]);
}
