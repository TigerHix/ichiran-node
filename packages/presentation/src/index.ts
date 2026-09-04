import type { TokenEntityKind, TokenSuffixId } from '@ichiran/core';
import { EN_CATALOG } from './catalogs/en.js';
import { ZH_HANS_CATALOG } from './catalogs/zh-Hans.js';
import {
  PRESENTATION_LOCALES,
  type PresentationCatalog,
  type PresentationLocale,
  type SampleId,
  type UiMessageId
} from './schema.js';

export { CATALOG_CONTEXT, PRESENTATION_LOCALES } from './schema.js';
export type { PresentationCatalog, PresentationLocale, SampleId, UiMessageId } from './schema.js';

export type PartOfSpeechCategory =
  | 'noun' | 'verb' | 'adjective' | 'adverb' | 'particle' | 'auxiliary'
  | 'conjunction' | 'pronoun' | 'copula' | 'interjection' | 'counter'
  | 'expression' | 'numeric' | 'prefix-suffix' | 'other';

const CATALOGS: Readonly<Record<PresentationLocale, PresentationCatalog>> = {
  en: EN_CATALOG,
  'zh-Hans': ZH_HANS_CATALOG
};

export interface Presentation {
  readonly locale: PresentationLocale;
  message(id: UiMessageId, values?: Readonly<Record<string, string | number>>): string;
  sampleLabel(id: SampleId): string;
  partOfSpeechLabel(code: string): string;
  fieldLabel(code: string): string;
  conjugationLabel(type: number): string;
  suffixLabel(id: TokenSuffixId): string;
  entityLabel(kind: TokenEntityKind): string;
}

function interpolate(template: string, values: Readonly<Record<string, string | number>> = {}): string {
  return template.replace(/\{([A-Za-z][A-Za-z0-9]*)\}/g, (placeholder, name: string) => {
    const value = values[name];
    return value === undefined ? placeholder : String(value);
  });
}

export function isPresentationLocale(value: string): value is PresentationLocale {
  return (PRESENTATION_LOCALES as readonly string[]).includes(value);
}

export function createPresentation(locale: PresentationLocale): Presentation {
  const catalog = CATALOGS[locale];
  return {
    locale,
    message: (id, values) => interpolate(catalog.ui[id], values),
    sampleLabel: id => catalog.samples[id],
    partOfSpeechLabel: code => catalog.pos[code] ?? code,
    fieldLabel: code => catalog.fields[code] ?? code,
    conjugationLabel: type => catalog.conjugations[type]
      ?? interpolate(catalog.ui.unknownConjugation, { type }),
    suffixLabel: id => catalog.suffixes[id],
    entityLabel: _kind => catalog.ui.properNoun
  };
}

export function partOfSpeechCategory(value: string): PartOfSpeechCategory {
  if (value === 'n' || value.startsWith('n-')) return 'noun';
  if (value.startsWith('v') || value === 'vi' || value === 'vt') return 'verb';
  if (value.startsWith('adj')) return 'adjective';
  if (value.startsWith('adv')) return 'adverb';
  if (value === 'prt') return 'particle';
  if (value.startsWith('aux')) return 'auxiliary';
  if (value === 'conj') return 'conjunction';
  if (value === 'pn') return 'pronoun';
  if (value.startsWith('cop')) return 'copula';
  if (value === 'int') return 'interjection';
  if (value === 'ctr') return 'counter';
  if (value === 'exp') return 'expression';
  if (value === 'num') return 'numeric';
  if (value === 'pref' || value === 'suf') return 'prefix-suffix';
  return 'other';
}
