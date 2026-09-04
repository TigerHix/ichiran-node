import { readFile } from 'node:fs/promises';

import type {
  LocaleGlossEntrySource,
  LocaleGlossGroupSource,
  LocaleGlossTextSource
} from '../browser-pack/locale-gloss.js';
import type { CanonicalEntry } from './model.js';
import {
  ZH_HANS_SENSE_INFO_PATTERN_POLICY,
  ZH_HANS_SENSE_INFO_PATTERN_RULES,
  translateZhHansSenseInfoPattern,
  type ZhHansSenseInfoPatternRuleId
} from './zh-hans-sense-info-patterns.js';

export interface ZhHansSenseInfoTranslation {
  readonly source: string;
  readonly target: string;
}

export interface ZhHansSenseInfoCatalog {
  readonly formatVersion: 1;
  readonly locale: 'zh-Hans';
  readonly sourceLocale: 'en';
  readonly translations: readonly ZhHansSenseInfoTranslation[];
}

export interface ZhHansSenseInfoProjection {
  readonly entries: readonly LocaleGlossEntrySource[];
  readonly stats: {
    readonly catalogTranslationCount: number;
    readonly patternPolicy: typeof ZH_HANS_SENSE_INFO_PATTERN_POLICY;
    readonly sourceInfoCount: number;
    readonly translatedInfoCount: number;
    readonly catalogTranslatedInfoCount: number;
    readonly patternTranslatedInfoCount: number;
    readonly fallbackInfoCount: number;
    readonly uniqueSourceInfoCount: number;
    readonly translatedUniqueInfoCount: number;
    readonly catalogTranslatedUniqueInfoCount: number;
    readonly patternTranslatedUniqueInfoCount: number;
    readonly unusedTranslationCount: number;
    readonly patternRuleCounts: Readonly<Record<ZhHansSenseInfoPatternRuleId, number>>;
  };
}

function record(value: unknown, label: string): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
}

function exactKeys(
  value: Record<string, unknown>,
  expected: readonly string[],
  label: string
): void {
  const keys = Object.keys(value);
  const unknown = keys.filter(key => !expected.includes(key));
  const missing = expected.filter(key => !keys.includes(key));
  if (unknown.length > 0) throw new Error(`${label} has unknown fields: ${unknown.join(', ')}`);
  if (missing.length > 0) throw new Error(`${label} is missing fields: ${missing.join(', ')}`);
}

function nonemptyText(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.trim().length === 0 || value !== value.trim()) {
    throw new Error(`${label} must be trimmed, non-empty text`);
  }
  return value;
}

export function parseZhHansSenseInfoCatalog(value: unknown): ZhHansSenseInfoCatalog {
  const catalog = record(value, 'zh-Hans sense-info catalog');
  exactKeys(
    catalog,
    ['formatVersion', 'locale', 'sourceLocale', 'translations'],
    'zh-Hans sense-info catalog'
  );
  if (catalog.formatVersion !== 1) {
    throw new Error('Unsupported zh-Hans sense-info catalog format');
  }
  if (catalog.locale !== 'zh-Hans' || catalog.sourceLocale !== 'en') {
    throw new Error('Sense-info catalog must translate en to zh-Hans');
  }
  if (!Array.isArray(catalog.translations)) {
    throw new Error('zh-Hans sense-info catalog translations must be an array');
  }
  let previousSource = '';
  const translations = catalog.translations.map((value, index) => {
    const translation = record(value, `zh-Hans sense-info translation ${index}`);
    exactKeys(translation, ['source', 'target'], `zh-Hans sense-info translation ${index}`);
    const source = nonemptyText(
      translation.source,
      `zh-Hans sense-info translation ${index} source`
    );
    const target = nonemptyText(
      translation.target,
      `zh-Hans sense-info translation ${index} target`
    );
    if (source <= previousSource) {
      throw new Error('zh-Hans sense-info translations must be unique and sorted by source');
    }
    previousSource = source;
    return { source, target };
  });
  return {
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations
  };
}

function translatedInfoBySense(
  entry: CanonicalEntry,
  translations: ReadonlyMap<string, string>,
  used: Set<string>,
  sourceNotes: Set<string>,
  catalogTranslatedNotes: Set<string>,
  patternTranslatedNotes: Set<string>,
  counts: {
    source: number;
    translated: number;
    catalogTranslated: number;
    patternTranslated: number;
    patternRules: Record<ZhHansSenseInfoPatternRuleId, number>;
  }
): ReadonlyMap<number, readonly LocaleGlossTextSource[]> {
  const result = new Map<number, readonly LocaleGlossTextSource[]>();
  for (const sense of entry.senses) {
    const info: LocaleGlossTextSource[] = [];
    for (const property of sense.properties) {
      if (property.tag !== 's_inf') continue;
      counts.source++;
      sourceNotes.add(property.text);
      const exactTarget = translations.get(property.text);
      const pattern = exactTarget === undefined
        ? translateZhHansSenseInfoPattern(property.text)
        : null;
      const target = exactTarget ?? pattern?.target;
      if (target === undefined) continue;
      counts.translated++;
      if (exactTarget !== undefined) {
        counts.catalogTranslated++;
        catalogTranslatedNotes.add(property.text);
        used.add(property.text);
      } else {
        counts.patternTranslated++;
        patternTranslatedNotes.add(property.text);
        counts.patternRules[pattern!.rule]++;
      }
      info.push({ ord: property.ordinal, text: target });
    }
    if (info.length > 0) result.set(sense.ordinal, info);
  }
  return result;
}

function mergeEntryInfo(
  entry: LocaleGlossEntrySource,
  infoBySense: ReadonlyMap<number, readonly LocaleGlossTextSource[]>
): LocaleGlossEntrySource {
  if (infoBySense.size === 0) return entry;
  const consumed = new Set<number>();
  const groups: LocaleGlossGroupSource[] = entry.groups.map(group => {
    if (group.targets.length !== 1) return group;
    const target = group.targets[0]!;
    const info = infoBySense.get(target);
    if (!info) return group;
    consumed.add(target);
    return { ...group, info };
  });
  for (const [target, info] of infoBySense) {
    if (!consumed.has(target)) groups.push({ targets: [target], glosses: [], info });
  }
  groups.sort((left, right) => (left.targets[0] ?? -1) - (right.targets[0] ?? -1));
  return { ...entry, groups };
}

/**
 * Add the first-party zh-Hans JMdict usage-note catalog to an existing locale
 * layer. Notes are matched by exact English source text, so a changed upstream
 * note becomes an explicit fallback instead of silently inheriting an obsolete
 * translation.
 */
export function projectZhHansSenseInfo(
  baseEntries: readonly CanonicalEntry[],
  localeEntries: readonly LocaleGlossEntrySource[],
  catalog: ZhHansSenseInfoCatalog
): ZhHansSenseInfoProjection {
  if (baseEntries.length !== localeEntries.length) {
    throw new Error(
      `zh-Hans sense-info merge has ${baseEntries.length} base entries and `
      + `${localeEntries.length} locale entries`
    );
  }
  const translations = new Map(catalog.translations.map(value => [value.source, value.target]));
  const used = new Set<string>();
  const sourceNotes = new Set<string>();
  const catalogTranslatedNotes = new Set<string>();
  const patternTranslatedNotes = new Set<string>();
  const patternRules = Object.fromEntries(
    ZH_HANS_SENSE_INFO_PATTERN_RULES.map(rule => [rule.id, 0])
  ) as Record<ZhHansSenseInfoPatternRuleId, number>;
  const counts = {
    source: 0,
    translated: 0,
    catalogTranslated: 0,
    patternTranslated: 0,
    patternRules
  };
  const entries = baseEntries.map((base, index) => {
    const locale = localeEntries[index]!;
    if (locale.seq !== base.seq) {
      throw new Error(
        `zh-Hans sense-info merge entry ${index} is ${locale.seq}; expected ${base.seq}`
      );
    }
    return mergeEntryInfo(
      locale,
      translatedInfoBySense(
        base,
        translations,
        used,
        sourceNotes,
        catalogTranslatedNotes,
        patternTranslatedNotes,
        counts
      )
    );
  });
  return {
    entries,
    stats: {
      catalogTranslationCount: catalog.translations.length,
      patternPolicy: ZH_HANS_SENSE_INFO_PATTERN_POLICY,
      sourceInfoCount: counts.source,
      translatedInfoCount: counts.translated,
      catalogTranslatedInfoCount: counts.catalogTranslated,
      patternTranslatedInfoCount: counts.patternTranslated,
      fallbackInfoCount: counts.source - counts.translated,
      uniqueSourceInfoCount: sourceNotes.size,
      translatedUniqueInfoCount: catalogTranslatedNotes.size + patternTranslatedNotes.size,
      catalogTranslatedUniqueInfoCount: catalogTranslatedNotes.size,
      patternTranslatedUniqueInfoCount: patternTranslatedNotes.size,
      unusedTranslationCount: catalog.translations.length - used.size,
      patternRuleCounts: counts.patternRules
    }
  };
}

export async function loadZhHansSenseInfo(
  path: string,
  baseEntries: readonly CanonicalEntry[],
  localeEntries: readonly LocaleGlossEntrySource[]
): Promise<ZhHansSenseInfoProjection> {
  const bytes = await readFile(path, 'utf8');
  let value: unknown;
  try {
    value = JSON.parse(bytes);
  } catch {
    throw new Error(`zh-Hans sense-info catalog ${path} is not valid JSON`);
  }
  return projectZhHansSenseInfo(
    baseEntries,
    localeEntries,
    parseZhHansSenseInfoCatalog(value)
  );
}
