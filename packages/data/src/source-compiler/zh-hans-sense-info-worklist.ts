import type { CanonicalEntry } from './model.js';
import type { ZhHansSenseInfoCatalog } from './zh-hans-sense-info.js';

export interface ZhHansSenseInfoWorkItem {
  readonly source: string;
  readonly occurrenceCount: number;
  readonly occurrences: readonly {
    readonly seq: number;
    readonly sense: number;
    readonly info: number;
    readonly headwords: readonly string[];
    readonly englishGlosses: readonly string[];
  }[];
}

/** Build a deterministic, context-rich queue for translation and LQA work. */
export function buildZhHansSenseInfoWorklist(
  entries: readonly CanonicalEntry[],
  catalog: ZhHansSenseInfoCatalog
): readonly ZhHansSenseInfoWorkItem[] {
  const translated = new Set(catalog.translations.map(value => value.source));
  const occurrences = new Map<string, ZhHansSenseInfoWorkItem['occurrences'][number][]>();
  for (const entry of entries) {
    const headwords = [...new Set([
      ...entry.kanji.map(form => form.text),
      ...entry.kana.map(form => form.text)
    ])];
    for (const sense of entry.senses) {
      for (const property of sense.properties) {
        if (property.tag !== 's_inf' || translated.has(property.text)) continue;
        const values = occurrences.get(property.text) ?? [];
        values.push({
          seq: entry.seq,
          sense: sense.ordinal,
          info: property.ordinal,
          headwords,
          englishGlosses: sense.glosses
        });
        occurrences.set(property.text, values);
      }
    }
  }
  return [...occurrences].map(([source, values]) => ({
    source,
    occurrenceCount: values.length,
    occurrences: values
  })).sort((left, right) =>
    right.occurrenceCount - left.occurrenceCount || left.source.localeCompare(right.source, 'en'));
}
