import type {
  DetailEntrySource,
  DetailPropertySource
} from '../browser-pack/details.js';
import {
  compareRootPayloadText,
  isRootPayloadKanaSurface,
  type RootPayloadEntrySource,
  type RootPayloadFormSource,
  type RootPayloadSource
} from '../browser-pack/root-payload.js';
import type {
  CanonicalEntry,
  CanonicalSense
} from './model.js';

const ARCHIVED = new Set(['arch', 'obsc', 'rare']);

function senseArchived(sense: CanonicalSense): boolean {
  return sense.properties.some(property =>
    property.tag === 'misc' && ARCHIVED.has(property.text));
}

function rootEntry(entry: CanonicalEntry): RootPayloadEntrySource {
  const activeSenses = entry.senses.filter(sense => !senseArchived(sense));
  const pos = [...new Set(activeSenses.flatMap(sense =>
    sense.properties
      .filter(property => property.tag === 'pos')
      .map(property => property.text)
  ))].sort(compareRootPayloadText);
  const preferKanaSenses = entry.senses.filter(sense =>
    sense.properties.some(property => property.tag === 'misc' && property.text === 'uk'));

  return {
    seq: entry.seq,
    nKanji: entry.kanji.length,
    nKana: entry.kana.length,
    primaryNokanji: entry.primaryNoKanji,
    archived: entry.senses.every(senseArchived),
    preferKana: preferKanaSenses.length > 0,
    preferKanaOnOrdinalZero: preferKanaSenses.some(sense => sense.ordinal === 0),
    pos
  };
}

function commonTags(tags: readonly string[]): string {
  return tags.map(tag => `[${tag}]`).join('');
}

function directForms(entries: readonly CanonicalEntry[]): RootPayloadFormSource[] {
  const forms: Array<RootPayloadFormSource & {
    readonly sourceEvent: number;
    readonly sourceOrdinal: number;
  }> = [];
  for (const entry of entries) {
    for (const form of entry.kanji) {
      if (isRootPayloadKanaSurface(form.text)) continue;
      forms.push({
        surface: form.text,
        route: 'kanji',
        seq: entry.seq,
        ord: form.ordinal,
        common: form.common,
        commonTags: commonTags(form.priorityTags),
        conjugatable: form.conjugatable,
        nokanji: form.noKanji,
        best: form.best,
        sourceEvent: form.sourceOrder.event,
        sourceOrdinal: form.sourceOrder.ordinal
      });
    }
    for (const form of entry.kana) {
      if (!isRootPayloadKanaSurface(form.text)) continue;
      forms.push({
        surface: form.text,
        route: 'kana',
        seq: entry.seq,
        ord: form.ordinal,
        common: form.common,
        commonTags: commonTags(form.priorityTags),
        conjugatable: form.conjugatable,
        nokanji: form.noKanji,
        best: form.best,
        sourceEvent: form.sourceOrder.event,
        sourceOrdinal: form.sourceOrder.ordinal
      });
    }
  }

  forms.sort((left, right) =>
    compareRootPayloadText(left.surface, right.surface) ||
    compareRootPayloadText(left.route, right.route) ||
    right.sourceEvent - left.sourceEvent ||
    right.sourceOrdinal - left.sourceOrdinal ||
    right.ord - left.ord ||
    right.seq - left.seq);

  let prior = '';
  let lookupOrder = 0;
  return forms.map(({ sourceEvent: _sourceEvent, sourceOrdinal: _sourceOrdinal, ...form }) => {
    const key = `${form.route}\u0000${form.surface}`;
    lookupOrder = key === prior ? lookupOrder + 1 : 0;
    prior = key;
    return { ...form, lookupOrder };
  });
}

export function canonicalRootPayloadSource(entries: readonly CanonicalEntry[]): RootPayloadSource {
  const ordered = [...entries].sort((left, right) => left.seq - right.seq);
  return {
    entries: ordered.map(rootEntry),
    forms: directForms(ordered),
    restrictions: ordered.flatMap(entry => entry.restrictions.map(restriction => ({
      seq: entry.seq,
      reading: restriction.reading,
      written: restriction.written
    }))).sort((left, right) =>
      left.seq - right.seq ||
      compareRootPayloadText(left.reading, right.reading) ||
      compareRootPayloadText(left.written, right.written))
  };
}

function detailProperties(sense: CanonicalSense): DetailPropertySource[] {
  return sense.properties
    .map(property => ({ tag: property.tag, ord: property.ordinal, text: property.text, sourceOrder: property.sourceOrder }))
    .sort((left, right) =>
      compareRootPayloadText(left.tag, right.tag) ||
      left.ord - right.ord ||
      left.sourceOrder.event - right.sourceOrder.event ||
      left.sourceOrder.ordinal - right.sourceOrder.ordinal)
    .map(({ sourceOrder: _sourceOrder, ...property }) => property);
}

export function canonicalDetailEntries(entries: readonly CanonicalEntry[]): DetailEntrySource[] {
  return [...entries]
    .sort((left, right) => left.seq - right.seq)
    .map(entry => ({
      seq: entry.seq,
      forms: [
        ...entry.kanji.map(form => ({ route: 'kanji' as const, form })),
        ...entry.kana.map(form => ({ route: 'kana' as const, form }))
      ].map(({ route, form }) => ({
        route,
        text: form.text,
        ord: form.ordinal,
        common: form.common,
        commonTags: commonTags(form.priorityTags),
        conjugatable: form.conjugatable,
        nokanji: form.noKanji,
        best: form.best
      })),
      senses: entry.senses.map(sense => ({
        ord: sense.ordinal,
        glosses: sense.glosses.map((text, ord) => ({ ord, text })),
        properties: detailProperties(sense)
      }))
    }));
}
