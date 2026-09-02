import type { CanonicalEntry, CanonicalForm } from './model.js';

function withBest(form: CanonicalForm, best: string | null): CanonicalForm {
  return { ...form, best };
}

export function deriveBestReadings(entry: CanonicalEntry): CanonicalEntry {
  const restrictionsByReading = new Map<string, Set<string>>();
  for (const restriction of entry.restrictions) {
    const written = restrictionsByReading.get(restriction.reading) ?? new Set<string>();
    written.add(restriction.written);
    restrictionsByReading.set(restriction.reading, written);
  }

  const kanji = entry.kanji.map(written => {
    const best = entry.kana.find(reading => {
      if (reading.noKanji) return false;
      const restrictions = restrictionsByReading.get(reading.text);
      return restrictions === undefined || restrictions.has(written.text);
    });
    return withBest(written, best?.text ?? null);
  });

  const kana = entry.kana.map(reading => {
    if (reading.noKanji) return withBest(reading, null);
    const restrictions = restrictionsByReading.get(reading.text);
    const best = restrictions === undefined
      ? entry.kanji[0]
      : entry.kanji.find(written => restrictions.has(written.text));
    return withBest(reading, best?.text ?? null);
  });

  return { ...entry, kanji, kana };
}
