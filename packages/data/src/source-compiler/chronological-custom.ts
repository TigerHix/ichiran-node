import { testWord } from '@ichiran/core';
import type {
  CanonicalEntry,
  CanonicalSense,
  CanonicalSenseProperty
} from './model.js';
import {
  CUSTOM_SOURCE_IDS,
  loadExtraRootDrafts,
  loadGeographicDrafts,
  type GeographicDraft
} from './custom-sources.js';

export const QUALIFIED_FIRST_GEOGRAPHIC_SEQ = 12_294_577;

export interface CustomSourcePaths {
  readonly extra: string;
  readonly municipality: string;
  readonly ward: string;
}

interface CustomEditBase {
  readonly event: number;
  readonly sourceId: string;
  readonly sourceOrdinal: number;
  readonly seq: number;
}

export type CustomEdit =
  | (CustomEditBase & { readonly kind: 'create-root' })
  | (CustomEditBase & { readonly kind: 'add-sense'; readonly definition: string })
  | (CustomEditBase & {
    readonly kind: 'replace-gloss';
    readonly oldGloss: string;
    readonly definition: string;
  });

export interface CustomCompilation {
  readonly createdRoots: readonly CanonicalEntry[];
  readonly updatedEntries: readonly CanonicalEntry[];
  readonly edits: readonly CustomEdit[];
  readonly skipped: number;
  readonly nextEvent: number;
}

function sourceId(draft: GeographicDraft): string {
  return draft.kind === 'municipality' ? CUSTOM_SOURCE_IDS.municipality : CUSTOM_SOURCE_IDS.ward;
}

function normalizeGeo(text: string): string {
  return text.toLowerCase().replaceAll('ū', 'u').replaceAll('ō', 'o');
}

function matchWords(gloss: string, words: readonly string[]): boolean {
  const normalized = normalizeGeo(gloss);
  return words.every(word => normalized.includes(normalizeGeo(word)));
}

function words(draft: GeographicDraft): string[] {
  const name = draft.definition.split(' ', 1)[0] ?? '';
  if (draft.kind === 'ward') return [name, 'Ward', draft.city];
  const type = draft.type === '道' ? null : {
    都: 'Metropolis',
    府: 'Prefecture',
    県: 'Prefecture',
    市: '(city',
    区: 'Ward',
    町: '(town',
    村: '(village'
  }[draft.type];
  return [name, type, draft.prefecture].filter((value): value is string => Boolean(value));
}

function replacementGloss(draft: GeographicDraft, gloss: string): boolean {
  if (draft.kind !== 'municipality' || (draft.type !== '市' && draft.type !== '県')) return false;
  const normalized = normalizeGeo(gloss);
  const name = normalizeGeo(draft.definition.split(' ', 1)[0] ?? '');
  if (draft.type === '市') {
    const prefecture = normalizeGeo(draft.prefecture?.split(' ', 1)[0] ?? '');
    return normalized === `${name} (city)` || normalized === `${name} (city in ${prefecture})`;
  }
  return normalized === `${name} (prefecture)` || normalized === `${name} (city, prefecture)`;
}

function entryGlosses(entry: CanonicalEntry): string[] {
  return entry.senses.flatMap(sense => sense.glosses);
}

function candidateKey(text: string, reading: string): string {
  return `${text}\u0000${reading}`;
}

function indexEntries(entries: Iterable<CanonicalEntry>): {
  readonly bySeq: Map<number, CanonicalEntry>;
  readonly candidates: Map<string, number[]>;
} {
  const bySeq = new Map<number, CanonicalEntry>();
  const candidates = new Map<string, number[]>();
  for (const entry of entries) {
    bySeq.set(entry.seq, entry);
    const written = entry.kanji[0]?.text;
    const reading = entry.kana[0]?.text;
    if (!reading) continue;
    const text = written ?? reading;
    const key = candidateKey(text, reading);
    const values = candidates.get(key) ?? [];
    values.push(entry.seq);
    candidates.set(key, values);
  }
  for (const values of candidates.values()) values.sort((left, right) => left - right);
  return { bySeq, candidates };
}

function posProperty(event: number): CanonicalSenseProperty {
  return { tag: 'pos', text: 'n', ordinal: 0, sourceOrder: { event, ordinal: 0 } };
}

function requiredEntry(entries: ReadonlyMap<number, CanonicalEntry>, seq: number): CanonicalEntry {
  const entry = entries.get(seq);
  if (!entry) throw new RangeError(`Missing canonical root ${seq}`);
  return entry;
}

function newRoot(draft: GeographicDraft, seq: number, event: number): CanonicalEntry {
  const form = (text: string, ordinal: number) => ({
    text,
    ordinal,
    sourceOrder: { event, ordinal },
    common: null,
    priorityTags: [],
    conjugatable: true,
    noKanji: false,
    best: null
  });
  const kanaOnly = testWord(draft.text, 'kana');
  return {
    seq,
    source: { sourceId: sourceId(draft), ordinal: draft.sourceOrdinal },
    kanji: kanaOnly ? [] : [form(draft.text, 0)],
    kana: [form(kanaOnly ? draft.text : draft.reading, 0)],
    senses: [{ ordinal: 0, glosses: [draft.definition], properties: [posProperty(event)] }],
    restrictions: [],
    primaryNoKanji: false
  };
}

function addSense(entry: CanonicalEntry, definition: string, event: number): CanonicalEntry {
  let lastPosition: string | undefined;
  for (let ordinal = entry.senses.length - 1; ordinal >= 0 && lastPosition === undefined; ordinal--) {
    lastPosition = entry.senses[ordinal]?.properties
      .find(property => property.tag === 'pos')?.text;
  }
  const sense: CanonicalSense = {
    ordinal: entry.senses.length,
    glosses: [definition],
    properties: lastPosition === 'n' ? [] : [posProperty(event)]
  };
  return { ...entry, senses: [...entry.senses, sense] };
}

function replaceGloss(entry: CanonicalEntry, oldGloss: string, definition: string): CanonicalEntry {
  return {
    ...entry,
    senses: entry.senses.map(sense => ({
      ...sense,
      glosses: sense.glosses.map(gloss => gloss === oldGloss ? definition : gloss)
    }))
  };
}

/** Applies extra.xml, then the stable municipality type order, then ward file order. */
export async function compileQualifiedCustomData(
  baseEntries: Iterable<CanonicalEntry>,
  paths: CustomSourcePaths,
  firstEvent: number
): Promise<CustomCompilation> {
  const index = indexEntries(baseEntries);
  const roots: CanonicalEntry[] = [];
  const rootPositions = new Map<number, number>();
  const updated = new Map<number, CanonicalEntry>();
  const edits: CustomEdit[] = [];
  let event = firstEvent;
  let nextSeq = QUALIFIED_FIRST_GEOGRAPHIC_SEQ;
  let skipped = 0;

  for (const { entry } of await loadExtraRootDrafts(paths.extra, event)) {
    if (index.bySeq.has(entry.seq)) throw new RangeError(`Duplicate extra root ${entry.seq}`);
    index.bySeq.set(entry.seq, entry);
    rootPositions.set(entry.seq, roots.length);
    roots.push(entry);
    edits.push({
      event,
      sourceId: entry.source.sourceId,
      sourceOrdinal: entry.source.ordinal,
      kind: 'create-root',
      seq: entry.seq
    });
    event++;
  }

  for (const draft of await loadGeographicDrafts(paths.municipality, paths.ward)) {
    const key = candidateKey(draft.text, draft.reading);
    const candidates = index.candidates.get(key) ?? [];
    let matched = false;
    let replacement: readonly [number, string] | null = null;
    const wantedWords = words(draft);
    for (const seq of candidates) {
      const entry = requiredEntry(index.bySeq, seq);
      for (const gloss of entryGlosses(entry)) {
        if (replacementGloss(draft, gloss)) {
          replacement = [seq, gloss];
          break;
        }
        if (matchWords(gloss, wantedWords)) {
          matched = true;
          break;
        }
      }
      if (matched || replacement) break;
    }

    if (matched) {
      skipped++;
      continue;
    }

    const provenance = {
      event,
      sourceId: sourceId(draft),
      sourceOrdinal: draft.sourceOrdinal
    };
    if (replacement) {
      const [seq, gloss] = replacement;
      const entry = replaceGloss(requiredEntry(index.bySeq, seq), gloss, draft.definition);
      index.bySeq.set(seq, entry);
      const rootPosition = rootPositions.get(seq);
      if (rootPosition !== undefined) roots[rootPosition] = entry;
      else updated.set(seq, entry);
      edits.push({
        ...provenance,
        kind: 'replace-gloss',
        seq,
        oldGloss: gloss,
        definition: draft.definition
      });
      event++;
      continue;
    }
    if (candidates.length > 0) {
      const seq = candidates[0];
      if (seq === undefined) throw new Error('Candidate index is empty');
      const entry = addSense(requiredEntry(index.bySeq, seq), draft.definition, event);
      index.bySeq.set(seq, entry);
      const rootPosition = rootPositions.get(seq);
      if (rootPosition !== undefined) roots[rootPosition] = entry;
      else updated.set(seq, entry);
      edits.push({ ...provenance, kind: 'add-sense', seq, definition: draft.definition });
      event++;
      continue;
    }

    const entry = newRoot(draft, nextSeq++, event);
    index.bySeq.set(entry.seq, entry);
    index.candidates.set(key, [entry.seq]);
    rootPositions.set(entry.seq, roots.length);
    roots.push(entry);
    edits.push({ ...provenance, kind: 'create-root', seq: entry.seq });
    event++;
  }

  return {
    createdRoots: roots,
    updatedEntries: [...updated.values()].sort((left, right) => left.seq - right.seq),
    edits,
    skipped,
    nextEvent: event
  };
}
