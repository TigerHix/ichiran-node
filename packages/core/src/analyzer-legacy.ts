import { asHiragana, testWord } from './characters.js';
import type { DetailEntry, DetailSense, DetailStoreReader } from './details.js';
import { romanizeWord, type RomanizationName } from './romanization.js';
import type { AnalyzerSupportReader } from './analyzer-support.js';
import type { RootPayloadReader } from './root-payload.js';
import type {
  PortableAnalysisAlternative,
  PortableAnalysisComponent,
  PortableAnalysisInflection,
  PortableAnalysisResult,
  PortableAnalysisRoot,
  PortableAnalysisToken
} from './analyzer.js';

export interface PortableLegacySerializeOptions {
  readonly method?: RomanizationName;
  readonly wordProperty?: (
    romanized: string,
    token: PortableAnalysisToken
  ) => unknown;
}

export interface PortableLegacyPresentationFacts {
  /** Dense, pack-local identity shared by semantic candidates from one legacy target. */
  readonly physicalGroup: number | null;
  /** Exact suffix class selected while materializing this candidate. */
  readonly suffixClass: string | null;
  /** Physical definition identity used by split/hint/suffix presentation. */
  readonly definitionSeq: number | null;
  /** Semantic paths collapsed into this one legacy physical target. */
  readonly semanticMembers?: readonly PortableLegacySemanticMember[];
  /** Complete root identity of the physical row before explicit suffix filtering. */
  readonly identityRoots?: readonly number[];
  /** Legacy WordInfo conjugation-id mode for this exact presentation value. */
  readonly conjugationSelection?: 'default' | 'explicit' | 'root';
  /** Contextual post-processing replaced an alternative wrapper's outer reading. */
  readonly contextualReading?: boolean;
}

export interface PortableLegacySemanticMember {
  readonly entryIndex: number | null;
  readonly root: PortableAnalysisRoot | null;
  readonly inflection: readonly PortableAnalysisInflection[];
  /** Dense physical identities from the first stage through the final stage. */
  readonly stageGroups: readonly (number | null)[];
  /** Semantic locator for a physically unique stage without a dense group. */
  readonly stageKeys?: readonly (string | null)[];
  /** Physical conjugation-row order for each corresponding generated stage. */
  readonly stageMemberOrds: readonly (number | null)[];
  /** Physical conj_prop order for each corresponding semantic alias. */
  readonly stagePropOrds?: readonly (number | null)[];
  /** Exact stable order of this physical member when one is available. */
  readonly memberOrd: number | null;
}

export type PortableLegacyPresentationValue =
  | PortableAnalysisToken
  | PortableAnalysisAlternative
  | PortableAnalysisComponent;

/** Hot readers and analyzer-private facts required only by the legacy view. */
export interface PortableLegacyPresentationContext {
  readonly roots: RootPayloadReader;
  readonly support: AnalyzerSupportReader;
  readonly directSurface: (rank: number) => string;
  readonly hint?: (
    definitionSeq: number,
    route: 'kana' | 'kanji',
    surface: string,
    reading: string
  ) => string | null;
  readonly presentationFacts?: (
    value: PortableLegacyPresentationValue
  ) => PortableLegacyPresentationFacts | null;
}

export interface PortableLegacyWordInfo {
  readonly type: 'KANJI' | 'KANA' | 'GAP';
  readonly text: string;
  readonly truetext?: string;
  readonly kana: string | readonly string[];
  readonly seq?: number | readonly number[];
  /** Semantic properties replace snapshot-specific generated conjugation IDs. */
  readonly conjugations?: readonly PortableAnalysisInflection[];
  readonly score: number;
  readonly components?: readonly PortableLegacyWordInfo[];
  readonly alternative?: true;
  readonly primary?: boolean;
  readonly start?: number;
  readonly end?: number;
  readonly counter?: readonly [string, boolean];
  readonly skipped: number;
  readonly isEntity?: true;
}

export type PortableLegacyCompactToken = readonly [
  romanized: string,
  word: PortableLegacyWordInfo,
  property: unknown
];
export type PortableLegacyCompactPath = readonly [
  words: readonly PortableLegacyCompactToken[],
  score: number
];
export type PortableLegacyCompactResult = readonly (
  | string
  | readonly PortableLegacyCompactPath[]
)[];

export interface PortableLegacySenseJson {
  readonly pos: string;
  readonly gloss: string;
  readonly field?: string;
  readonly info?: string;
}

export interface PortableLegacyConjugationJson {
  readonly prop: readonly {
    readonly pos: string;
    readonly type: string;
    readonly fml?: true;
    readonly neg?: true;
  }[];
  readonly reading?: string;
  readonly gloss?: readonly PortableLegacySenseJson[];
  readonly readok?: boolean;
  readonly via?: readonly PortableLegacyConjugationJson[];
  /** Non-JSON facts retained only for the historical text-info renderer. */
  readonly [PORTABLE_LEGACY_INFO]?: PortableLegacyConjugationInfoFacts;
}

export interface PortableLegacyGlossJson {
  readonly reading?: string;
  readonly text?: string;
  readonly kana?: string | readonly string[];
  readonly score?: number;
  readonly compound?: readonly string[];
  readonly components?: readonly PortableLegacyGlossJson[];
  readonly counter?: { readonly value: string; readonly ordinal: true | readonly [] };
  readonly seq?: number | readonly number[];
  readonly gloss?: readonly PortableLegacySenseJson[];
  readonly suffix?: string;
  readonly conj?: readonly PortableLegacyConjugationJson[];
  readonly alternative?: readonly PortableLegacyGlossJson[];
  /** Non-JSON facts retained only for the historical text-info renderer. */
  readonly [PORTABLE_LEGACY_INFO]?: PortableLegacyWordInfoFacts;
}

export interface PortableLegacyWordInfoFacts {
  /** Dictionary definition used to hydrate unfiltered word-info senses. */
  readonly definitionSeq: number | null;
  readonly conjugationSelection: 'default' | 'explicit' | 'root';
  /** Generated inflected WordInfo rows have no senses of their own. */
  readonly inflected: boolean;
}

export interface PortableLegacyConjugationInfoFacts {
  readonly flags: readonly {
    readonly negative: boolean | null;
    readonly formal: boolean | null;
  }[];
  /** Current PostgreSQL Node renderer's entry-info-short result. */
  readonly shortGloss?: string;
}

/**
 * In-memory compatibility metadata. Symbol keys never alter the public
 * detailed JSON shape and add no bytes to analyzer packs.
 */
export const PORTABLE_LEGACY_INFO: unique symbol = Symbol('ichiran.legacy-info');

function attachLegacyInfo<T extends object>(
  value: T,
  facts: PortableLegacyWordInfoFacts | PortableLegacyConjugationInfoFacts
): T {
  Object.defineProperty(value, PORTABLE_LEGACY_INFO, { value: facts });
  return value;
}

export type PortableLegacyTransformedToken = readonly [
  romanized: string,
  word: PortableLegacyGlossJson,
  property: unknown
];
export type PortableLegacyTransformedPath = readonly [
  words: readonly PortableLegacyTransformedToken[],
  score: number
];
export type PortableLegacyTransformedResult = readonly (
  | string
  | readonly PortableLegacyTransformedPath[]
)[];

const CONJUGATION_DESCRIPTIONS: Readonly<Record<number, string>> = Object.freeze({
  1: 'Non-past',
  2: 'Past (~ta)',
  3: 'Conjunctive (~te)',
  4: 'Provisional (~eba)',
  5: 'Potential',
  6: 'Passive',
  7: 'Causative',
  8: 'Causative-Passive',
  9: 'Volitional',
  10: 'Imperative',
  11: 'Conditional (~tara)',
  12: 'Alternative (~tari)',
  13: 'Continuative (~i)',
  50: 'Adverbial',
  51: 'Adjective Stem',
  52: 'Negative Stem',
  53: 'Causative (~su)',
  54: 'Old/literary form'
});

// Kept local because the portable package must not pull the PostgreSQL core
// into a browser bundle. This is the complete analyzer suffix description
// table used by the frozen core presentation layer.
const SUFFIX_DESCRIPTIONS: Readonly<Record<string, string>> = Object.freeze({
  ':chau': 'indicates completion (to finish ...)',
  ':ha': 'topic marker particle',
  ':tai': 'want to... / would like to...',
  ':iru': 'indicates continuing action (to be ...ing)',
  ':oru': 'indicates continuing action (to be ...ing) (humble)',
  ':aru': 'indicates completion / finished action',
  ':kuru': 'indicates action that had been continuing up till now / came to be ',
  ':oku': 'to do in advance / to leave in the current state expecting a later change',
  ':kureru': '(asking) to do something for one',
  ':morau': '(asking) to get somebody to do something',
  ':itadaku': '(asking) to get somebody to do something (polite)',
  ':iku': 'is becoming / action starting now and continuing',
  ':suru': 'makes a verb from a noun',
  ':itasu': 'makes a verb from a noun (humble)',
  ':sareru': 'makes a verb from a noun (honorific or passive)',
  ':saseru': 'let/make someone/something do ...',
  ':rou': 'probably / it seems that... / I guess ...',
  ':ii': "it's ok if ... / is it ok if ...?",
  ':mo': 'even if ...',
  ':sugiru': 'to be too (much) ...',
  ':nikui': 'difficult to...',
  ':gatai': 'difficult to...',
  ':sa': '-ness (degree or condition of adjective)',
  ':tsutsu': 'while ... / in the process of ...',
  ':tsutsuaru': 'to be doing ... / to be in the process of doing ...',
  ':uru': 'can ... / to be able to ...',
  ':sou': 'looking like ... / seeming ...',
  ':nai': 'negative suffix',
  ':ra': 'pluralizing suffix (not polite)',
  ':kudasai': 'please do ...',
  ':yagaru': 'indicates disdain or contempt',
  ':naru': 'to become ...',
  ':desu': 'formal copula',
  ':desho': "it seems/perhaps/don't you think?",
  ':tosuru': 'to try to .../to be about to...',
  ':garu': 'to feel .../have a ... impression of someone',
  ':me': 'somewhat/-ish',
  ':gai': 'worth it to ...',
  ':tasou': 'seem to want to... (tai+sou)',
  '2826528': 'polite prefix',
  '2028980': 'at / in / by',
  '2028970': 'or / questioning particle',
  '2028990': 'to / at / in',
  '2029010': 'indicates direct object of action',
  '1469800': "indicates possessive (...'s)",
  '2086960': 'quoting particle',
  '1002980': 'from / because'
});

function wordType(route: 'kana' | 'kanji' | 'gap'): 'KANJI' | 'KANA' | 'GAP' {
  return route === 'gap' ? 'GAP' : route.toUpperCase() as 'KANJI' | 'KANA';
}

function simplifyReadingList(readings: readonly string[]): string[] {
  const values: Array<{ text: string; count: number; spaces: number[] }> = [];
  for (const reading of readings) {
    const spaces: number[] = [];
    let text = '';
    for (const character of reading) {
      if (character === ' ') spaces.push(text.length);
      else text += character;
    }
    const current = values.find(value => value.text === text);
    if (current) {
      current.count++;
      current.spaces.push(...spaces);
    } else {
      values.push({ text, count: 1, spaces });
    }
  }
  return values.map(value => {
    const positions = [...new Set(value.spaces)].sort((a, b) => a - b);
    let output = '';
    for (let index = 0; index < value.text.length; index++) {
      if (positions.includes(index)) {
        const count = value.spaces.filter(position => position === index).length;
        output += count === value.count ? ' ' : '·';
      }
      output += value.text[index]!;
    }
    return output;
  });
}

function romanizedReadings(
  readings: readonly string[],
  text: string,
  method?: RomanizationName
): string {
  return simplifyReadingList(readings.map(reading => romanizeWord(reading, {
    method,
    originalSpelling: text
  }))).join('/');
}

function componentWord(component: PortableAnalysisComponent): PortableLegacyWordInfo {
  return {
    type: wordType(component.route),
    text: component.text,
    truetext: component.trueText ?? component.text,
    kana: component.reading,
    seq: component.root?.seq,
    conjugations: component.inflection.length > 0 ? component.inflection : undefined,
    score: 0,
    primary: component.primary,
    skipped: 0
  };
}

function alternativeWord(
  alternative: PortableAnalysisAlternative,
  start: number,
  end: number
): PortableLegacyWordInfo {
  return {
    type: wordType(alternative.route),
    text: alternative.text,
    truetext: alternative.trueText ?? alternative.text,
    kana: alternative.reading,
    seq: alternative.root?.seq,
    conjugations: alternative.inflection.length > 0 ? alternative.inflection : undefined,
    score: alternative.score,
    components: alternative.components.length > 0
      ? alternative.components.map(componentWord)
      : undefined,
    start,
    end,
    counter: alternative.counter ?? undefined,
    skipped: 0
  };
}

function compactWord(
  token: PortableAnalysisToken,
  chunkStart: number,
  context?: Pick<PortableLegacyPresentationContext, 'presentationFacts'>
): PortableLegacyWordInfo {
  const start = token.start - chunkStart;
  const end = token.end - chunkStart;
  if (token.alternatives.length > 1) {
    return {
      type: wordType(token.route),
      text: token.text,
      kana: context?.presentationFacts?.(token)?.contextualReading
        ? token.reading
        : [...new Set(token.alternatives.map(value => value.reading))],
      seq: token.alternatives.flatMap(value => value.root ? [value.root.seq] : []),
      score: token.score,
      components: token.alternatives.map(value => alternativeWord(value, start, end)),
      alternative: true,
      start,
      end,
      skipped: token.skipped
    };
  }
  return {
    type: wordType(token.route),
    text: token.text,
    truetext: token.trueText ?? token.text,
    kana: token.reading,
    seq: token.root?.seq,
    conjugations: token.inflection.length > 0 ? token.inflection : undefined,
    score: token.score,
    components: token.components.length > 0 ? token.components.map(componentWord) : undefined,
    start,
    end,
    counter: token.counter ?? undefined,
    skipped: token.skipped,
    isEntity: token.entity ? true : undefined
  };
}

function tokenRomanized(
  token: PortableAnalysisToken,
  method?: RomanizationName,
  context?: Pick<PortableLegacyPresentationContext, 'presentationFacts'>
): string {
  if (
    token.alternatives.length > 1
    && !context?.presentationFacts?.(token)?.contextualReading
  ) {
    return romanizedReadings(
      token.alternatives.map(value => value.reading),
      token.text,
      method
    );
  }
  return romanizeWord(token.reading, { method, originalSpelling: token.text });
}

/**
 * Raw, compact WordInfo-like output with exact romanize* basicSplit nesting.
 * Generated identities and conjugations are semantic, not frozen DB row IDs.
 */
export function serializePortableLegacyCompact(
  result: PortableAnalysisResult,
  options: PortableLegacySerializeOptions = {},
  context?: Pick<PortableLegacyPresentationContext, 'presentationFacts'>
): PortableLegacyCompactResult {
  const property = options.wordProperty ?? (() => []);
  return result.chunks.map(chunk => {
    if (chunk.type === 'misc') return chunk.text;
    return chunk.paths.map(path => [path.tokens.map(token => {
      const romanized = tokenRomanized(token, options.method, context);
      return [romanized, compactWord(token, chunk.start, context), property(romanized, token)] as const;
    }), path.score] as const);
  });
}

function properties(sense: DetailSense, tag: string): string[] {
  return sense.properties
    .filter(property => property.tag === tag)
    .sort((a, b) => a.ord - b.ord)
    .map(property => property.text);
}

const PRESENTED_PROPERTY_TAGS = new Set(['field', 'pos', 's_inf', 'stagk', 'stagr']);

/**
 * Core/Lisp reverses the final property bag returned by getSensesRaw while all
 * preceding bags retain database order. Preserve that observable quirk here;
 * it affects the displayed order when the last property group has >1 value.
 */
function finalPropertyGroup(entry: DetailEntry): {
  readonly senseOrd: number;
  readonly tag: string;
} | null {
  let result: { senseOrd: number; tag: string } | null = null;
  for (const sense of [...entry.senses].sort((left, right) => left.ord - right.ord)) {
    const tags = [...new Set(sense.properties
      .filter(property => PRESENTED_PROPERTY_TAGS.has(property.tag))
      .map(property => property.tag))].sort();
    const tag = tags.at(-1);
    if (tag !== undefined) result = { senseOrd: sense.ord, tag };
  }
  return result;
}

interface ReadingRestriction {
  readonly reading: string;
  readonly written: string;
}

function entryRestrictions(
  context: PortableLegacyPresentationContext,
  entryIndex: number
): ReadingRestriction[] {
  const values: ReadingRestriction[] = [];
  const start = context.roots.restrictionStart(entryIndex);
  const end = context.roots.restrictionEnd(entryIndex);
  for (let index = start; index < end; index++) {
    values.push({
      reading: context.roots.resolveSurfaceReference(
        context.roots.restrictionReadingReference(index),
        context.directSurface
      )!,
      written: context.roots.resolveSurfaceReference(
        context.roots.restrictionWrittenReference(index),
        context.directSurface
      )!
    });
  }
  return values;
}

function readingMatchesWritten(
  reading: DetailEntry['forms'][number],
  written: string,
  restrictions: readonly ReadingRestriction[]
): boolean {
  if (reading.nokanji) return false;
  const restricted = restrictions
    .filter(value => value.reading === reading.text)
    .map(value => value.written);
  return restricted.length === 0 || restricted.includes(written);
}

/** Exact root-payload port of core matchSenseRestrictions/matchKanaKanji. */
function senseAllowed(
  sense: DetailSense,
  entry: DetailEntry,
  route: 'kana' | 'kanji',
  form: string,
  reading: string,
  restrictions: readonly ReadingRestriction[]
): boolean {
  const stagk = properties(sense, 'stagk');
  const stagr = properties(sense, 'stagr');
  if (stagk.length === 0 && stagr.length === 0) return true;

  const current = route === 'kanji' ? form : reading;
  if (stagk.includes(current) || stagr.includes(current)) return true;
  if (route === 'kana' && stagr.includes(asHiragana(current))) return true;
  if (route === 'kanji' && stagr.length === 0) return false;
  if (route === 'kana' && stagk.length === 0) return false;

  if (route === 'kanji') {
    return entry.forms.some(value =>
      value.route === 'kana'
      && stagr.includes(value.text)
      && readingMatchesWritten(value, current, restrictions));
  }

  const currentReadings = entry.forms.filter(value =>
    value.route === 'kana' && value.text === current);
  return currentReadings.some(currentReading => entry.forms.some(value =>
    value.route === 'kanji'
    && stagk.includes(value.text)
    && readingMatchesWritten(currentReading, value.text, restrictions)));
}

function senses(
  entry: DetailEntry,
  entryIndex: number,
  context: PortableLegacyPresentationContext,
  route: 'kana' | 'kanji',
  form: string,
  reading: string,
  posFilter?: readonly string[]
): PortableLegacySenseJson[] {
  const result: PortableLegacySenseJson[] = [];
  const restrictions = entryRestrictions(context, entryIndex);
  const reversed = finalPropertyGroup(entry);
  let carriedPos: string[] = [];
  for (const sense of [...entry.senses].sort((a, b) => a.ord - b.ord)) {
    const values = (tag: string): string[] => {
      const found = properties(sense, tag);
      return reversed?.senseOrd === sense.ord && reversed.tag === tag
        ? found.reverse()
        : found;
    };
    const pos = values('pos');
    if (pos.length > 0) carriedPos = pos;
    if (posFilter && !carriedPos.some(value => posFilter.includes(value))) continue;
    if (!senseAllowed(sense, entry, route, form, reading, restrictions)) continue;
    const gloss = [...sense.glosses]
      .sort((a, b) => a.ord - b.ord)
      .map(value => value.text)
      .join('; ');
    const field = values('field');
    const info = values('s_inf');
    const value: {
      pos: string;
      gloss: string;
      field?: string;
      info?: string;
    } = {
      pos: `[${carriedPos.join(',')}]`,
      gloss
    };
    if (field.length > 0) value.field = `{${field.join(',')}}`;
    if (info.length > 0) value.info = info.join('; ');
    result.push(value);
  }
  return result;
}

function readingLabel(
  route: 'kana' | 'kanji',
  text: string,
  reading: string,
  counter: readonly [string, boolean] | null
): string {
  return route === 'kanji' || counter ? `${text} 【${reading}】` : text;
}

/**
 * The pinned Lisp presenter resolves a conjugation leaf from every matching
 * `conj_source_reading.source_text`, then takes the first dictionary row
 * returned by PostgreSQL. Two 260118 kana routes therefore select a secondary
 * lexical spelling. Keep that database-local presentation choice at the leaf;
 * direct lexical words and kanji-route conjugations retain their own spelling.
 */
function legacyConjugationSourceRoot(
  root: PortableAnalysisRoot,
  sourceRoute: 'kana' | 'kanji'
): PortableAnalysisRoot {
  if (sourceRoute !== 'kana') return root;
  if (root.seq === 1_547_720) {
    return { seq: root.seq, form: '来る', reading: 'クる' };
  }
  if (root.seq === 2_827_915) {
    return { seq: root.seq, form: '置けばよい', reading: 'おけばよい' };
  }
  return root;
}

function conjProperty(inflection: PortableAnalysisInflection): {
  pos: string;
  type: string;
  fml?: true;
  neg?: true;
} {
  const value: {
    pos: string;
    type: string;
    fml?: true;
    neg?: true;
  } = {
    pos: inflection.pos,
    type: CONJUGATION_DESCRIPTIONS[inflection.type] ?? String(inflection.type)
  };
  if (inflection.formal) value.fml = true;
  if (inflection.negative) value.neg = true;
  return value;
}

class DetailHydrator {
  readonly context: PortableLegacyPresentationContext;
  readonly #details: DetailStoreReader;
  readonly #entries = new Map<number, Promise<DetailEntry>>();

  constructor(details: DetailStoreReader, context: PortableLegacyPresentationContext) {
    this.#details = details;
    this.context = context;
  }

  entry(index: number | null): Promise<DetailEntry> | null {
    if (index === null) return null;
    let value = this.#entries.get(index);
    if (!value) {
      value = this.#details.entry(index);
      this.#entries.set(index, value);
    }
    return value;
  }
}

interface ConjugationStageItem {
  readonly member: PortableLegacySemanticMember;
  readonly depth: number;
  readonly order: number;
}

interface ConjugationStageRow {
  readonly items: ConjugationStageItem[];
  readonly properties: readonly PortableAnalysisInflection[];
  readonly via: boolean;
  readonly memberOrd: number | null;
  readonly order: number;
}

function inflectionKey(value: PortableAnalysisInflection): string {
  return JSON.stringify([
    value.pos, value.type, value.negative, value.formal, value.ordinal
  ]);
}

function conjugationOrder(type: number): number {
  if (type === 10) return 13;
  if (type === 13) return 10;
  return type;
}

function stageRows(items: readonly ConjugationStageItem[]): ConjugationStageRow[] {
  // The legacy presenter suppresses a direct row only when the same semantic
  // root also reaches this target through an intermediate target. Applying
  // that preference globally incorrectly drops unrelated roots.
  const rootsWithVia = new Set(items
    .filter(item => item.depth > 0)
    .map(item => item.member.root?.seq ?? `unique:${item.order}`));
  const active = items.filter(item =>
    item.depth > 0
    || !rootsWithVia.has(item.member.root?.seq ?? `unique:${item.order}`));
  const rows = new Map<string, ConjugationStageItem[]>();
  for (const item of active) {
    const group = item.member.stageGroups[item.depth] ?? null;
    const stageKey = item.member.stageKeys?.[item.depth] ?? null;
    const memberOrd = item.member.stageMemberOrds[item.depth] ?? null;
    const key = group !== null && memberOrd !== null
      ? `${group}:${memberOrd}`
      : stageKey !== null && memberOrd !== null
        ? `semantic:${stageKey}:${item.depth}:${memberOrd}`
        : `unique:${item.order}:${item.depth}`;
    const values = rows.get(key) ?? [];
    values.push(item);
    rows.set(key, values);
  }
  return [...rows.values()].map(values => {
    const seen = new Set<string>();
    const properties: PortableAnalysisInflection[] = [];
    const ordered = [...values].sort((left, right) =>
      (left.member.stagePropOrds?.[left.depth] ?? Number.MAX_SAFE_INTEGER)
        - (right.member.stagePropOrds?.[right.depth] ?? Number.MAX_SAFE_INTEGER)
      || left.order - right.order);
    for (const item of ordered) {
      const property = item.member.inflection[item.depth]!;
      const propOrd = item.member.stagePropOrds?.[item.depth] ?? null;
      const key = propOrd === null ? inflectionKey(property) : `ord:${propOrd}`;
      if (!seen.has(key)) {
        seen.add(key);
        properties.push(property);
      }
    }
    return {
      items: values,
      properties,
      via: values[0]!.depth > 0,
      memberOrd: values[0]!.member.stageMemberOrds[values[0]!.depth] ?? null,
      order: Math.min(...values.map(value => value.order))
    };
  }).sort((left, right) =>
    Number(left.via) - Number(right.via)
    || Math.min(...left.properties.map(value => conjugationOrder(value.type)))
      - Math.min(...right.properties.map(value => conjugationOrder(value.type)))
    || (left.memberOrd ?? Number.MAX_SAFE_INTEGER)
      - (right.memberOrd ?? Number.MAX_SAFE_INTEGER)
    || left.order - right.order);
}

/** Core collapses repeated outer rows that recurse through the same via target. */
function mergeViaRows(rows: readonly ConjugationStageRow[]): ConjugationStageRow[] {
  const output: ConjugationStageRow[] = [];
  const byVia = new Map<number, ConjugationStageRow>();
  for (const row of rows) {
    if (!row.via) {
      output.push(row);
      continue;
    }
    const first = row.items[0]!;
    const viaGroup = first.member.stageGroups[first.depth - 1] ?? null;
    if (viaGroup === null) {
      output.push(row);
      continue;
    }
    const prior = byVia.get(viaGroup);
    if (prior) {
      prior.items.push(...row.items);
    } else {
      byVia.set(viaGroup, row);
      output.push(row);
    }
  }
  return output;
}

async function conjugationForest(
  members: readonly PortableLegacySemanticMember[],
  hydrate: DetailHydrator,
  selection: 'default' | 'explicit' | 'root',
  sourceRoute: 'kana' | 'kanji'
): Promise<PortableLegacyConjugationJson[]> {
  if (selection === 'root') return [];
  const inflected = members.filter(member => member.inflection.length > 0);
  // Generated dictionary rows carry no explicit conjugation-id selection.
  // conjInfoJson first chooses every null-via row when at least one exists;
  // only an all-via target recurses. Length-zero lexical members do not alter
  // the physical target's conjugation-row selection (root collisions included).
  const selected = selection === 'default'
    && inflected.some(member => member.inflection.length === 1)
    ? inflected.filter(member => member.inflection.length === 1)
    : inflected;
  const initial = selected.flatMap((member, order) => member.inflection.length === 0
    ? []
    : [{ member, depth: member.inflection.length - 1, order }]);

  const render = async (
    items: readonly ConjugationStageItem[]
  ): Promise<PortableLegacyConjugationJson[]> => {
    const nodes: PortableLegacyConjugationJson[] = [];
    for (const row of mergeViaRows(stageRows(items))) {
      const prop = row.properties.map(conjProperty);
      if (row.via) {
        const via = await render(row.items.map(item => ({
          ...item,
          depth: item.depth - 1
        })));
        if (via.length > 0) {
          nodes.push(attachLegacyInfo(
            { prop, via, readok: via[0]!.readok },
            {
              flags: row.properties.map(value => ({
                negative: value.negative,
                formal: value.formal
              }))
            }
          ));
        }
        continue;
      }

      const member = row.items[0]!.member;
      const root = member.root;
      // ea958336 drops an indirect conjugation branch when its original text
      // cannot be reconstructed. In the portable semantic graph that missing
      // reading chain is represented by a null root; emitting a blank leaf
      // would keep the invalid outer `via` node alive.
      if (root === null) continue;
      const presentationRoot = legacyConjugationSourceRoot(root, sourceRoute);
      const detailPromise = hydrate.entry(member.entryIndex);
      const entry = detailPromise ? await detailPromise : null;
      // A KanaText source can display its best-kanji spelling in the label,
      // but core still checks sense restrictions against the kana row.
      const labelRoute = testWord(presentationRoot.form, 'kana') ? 'kana' : 'kanji';
      const hintedReading = hydrate.context.hint?.(
        presentationRoot.seq,
        'kana',
        presentationRoot.reading,
        presentationRoot.reading
      ) ?? hydrate.context.hint?.(
        presentationRoot.seq,
        labelRoute,
        presentationRoot.form,
        presentationRoot.reading
      ) ?? presentationRoot.reading;
      const gloss = entry && member.entryIndex !== null
        ? senses(
            entry,
            member.entryIndex,
            hydrate.context,
            sourceRoute,
            presentationRoot.form,
            presentationRoot.reading,
            row.properties.map(value => value.pos)
          )
        : [];
      nodes.push(attachLegacyInfo(
        {
          prop,
          reading: readingLabel(labelRoute, presentationRoot.form, hintedReading, null),
          gloss,
          readok: true
        },
        {
          flags: row.properties.map(value => ({
            negative: value.negative,
            formal: value.formal
          })),
          // The PostgreSQL implementation maps snake_case result keys to
          // camelCase, while entryInfoShort reads `gloss_text`. Preserve its
          // observable empty short gloss without changing detailed JSON.
          shortGloss: ''
        }
      ));
    }
    const readable = nodes.filter(node => node.readok);
    return readable.length > 0 ? readable : nodes;
  };

  return initial.length > 0 ? render(initial) : [];
}

interface DetailWord {
  readonly text: string;
  readonly route: 'kana' | 'kanji';
  readonly reading: string;
  readonly score: number;
  readonly entryIndex: number | null;
  readonly seq: number | null;
  readonly rootForm: string;
  readonly rootReading: string;
  readonly inflection: readonly PortableAnalysisInflection[];
  readonly components: readonly PortableAnalysisComponent[];
  readonly counter: readonly [string, boolean] | null;
  readonly entity: boolean;
  readonly suffix: boolean;
  readonly component: boolean;
  readonly presentationValue: PortableLegacyPresentationValue;
}

async function detailedComponent(
  component: PortableAnalysisComponent,
  hydrate: DetailHydrator
): Promise<PortableLegacyGlossJson> {
  return detailedWord({
    text: component.text,
    route: component.route,
    reading: component.reading,
    score: 0,
    entryIndex: component.entryIndex,
    seq: component.root?.seq ?? null,
    rootForm: component.root?.form ?? component.text,
    rootReading: component.root?.reading ?? component.reading,
    inflection: component.inflection,
    components: [],
    counter: null,
    entity: false,
    suffix: !component.primary,
    component: true,
    presentationValue: component
  }, hydrate);
}

async function detailedWord(
  word: DetailWord,
  hydrate: DetailHydrator
): Promise<PortableLegacyGlossJson> {
  const output: {
    reading: string;
    text: string;
    kana: string;
    score: number;
    compound?: readonly string[];
    components?: readonly PortableLegacyGlossJson[];
    counter?: { value: string; ordinal: true | readonly [] };
    seq?: number | readonly number[];
    gloss?: readonly PortableLegacySenseJson[];
    suffix?: string;
    conj?: readonly PortableLegacyConjugationJson[];
  } = {
    reading: readingLabel(
      word.route,
      word.text,
      word.reading,
      word.seq === null ? null : word.counter
    ),
    text: word.text,
    kana: word.reading,
    score: word.score
  };
  const facts = hydrate.context.presentationFacts?.(word.presentationValue) ?? null;
  const conjugationSelection = facts?.conjugationSelection
    ?? (word.component ? 'explicit' : 'default');
  const definitionSeq = facts ? facts.definitionSeq : word.seq;
  const finish = (): PortableLegacyGlossJson => attachLegacyInfo(output, {
    definitionSeq,
    conjugationSelection,
    inflected: word.inflection.length > 0
  });
  if (word.components.length > 0) {
    output.compound = word.components.map(value => value.text);
    const components: PortableLegacyGlossJson[] = [];
    for (const component of word.components) {
      components.push(await detailedComponent(component, hydrate));
    }
    output.components = components;
    return finish();
  }
  const members = facts?.semanticMembers ?? [{
    entryIndex: word.entryIndex,
    root: word.seq === null ? null : {
      seq: word.seq,
      form: word.rootForm,
      reading: word.rootReading
    },
    inflection: word.inflection,
    stageGroups: word.inflection.map(() => null),
    stageMemberOrds: word.inflection.map(() => null),
    memberOrd: null
  }];
  const detailPromise = hydrate.entry(word.entryIndex);
  const entry = detailPromise ? await detailPromise : null;
  if (word.seq !== null) {
    const hasDirectMember = members.some(member => member.inflection.length === 0);
    const semanticSeqs = [...new Set(facts?.identityRoots ?? members.flatMap(member =>
      member.root === null ? [] : [member.root.seq]))].sort((left, right) => left - right);
    output.seq = hasDirectMember || semanticSeqs.length === 0
      ? word.seq
      : semanticSeqs.length === 1
        ? semanticSeqs[0]!
        : semanticSeqs;
  }
  const senseRoute = word.inflection.length > 0
    ? (testWord(word.rootForm, 'kana') ? 'kana' : 'kanji')
    : word.route;
  let rootGloss = entry && word.entryIndex !== null ? senses(
    entry,
    word.entryIndex,
    hydrate.context,
    senseRoute,
    word.rootForm,
    word.rootReading,
    word.counter ? ['ctr'] : undefined
  ) : [];
  const properNoun = { pos: '[n-pr]', gloss: 'proper noun (named entity)' };
  if (word.entity && !rootGloss.some(value => value.pos === '[n-pr]')) {
    rootGloss = [properNoun, ...rootGloss];
  }
  if (word.counter) {
    if (rootGloss.length > 0) output.gloss = rootGloss;
    output.counter = { value: word.counter[0], ordinal: word.counter[1] ? true : [] };
    return finish();
  }
  const suffixClass = facts?.suffixClass
    ?? (facts?.definitionSeq !== null && facts?.definitionSeq !== undefined
      ? hydrate.context.support.suffixClass(facts.definitionSeq)
      : null)
    ?? (word.seq !== null ? hydrate.context.support.suffixClass(word.seq) : null);
  const suffixDescription = word.suffix
    ? SUFFIX_DESCRIPTIONS[suffixClass ?? '']
      ?? (word.seq !== null ? SUFFIX_DESCRIPTIONS[String(word.seq)] : undefined)
    : undefined;
  if (suffixDescription) output.suffix = suffixDescription;
  if (
    !suffixDescription
    && word.inflection.length === 0
    && facts?.conjugationSelection !== 'explicit'
    && rootGloss.length > 0
  ) {
    output.gloss = rootGloss;
  }
  if (word.seq === null) return finish();
  output.conj = await conjugationForest(
    members,
    hydrate,
    conjugationSelection,
    word.route
  );
  // Core applies entity marking after ordinary gloss/conjugation rendering.
  // Inflected dictionary entities therefore still receive a top-level n-pr
  // gloss even though their lexical senses live under `conj`.
  if (word.entity && !output.gloss?.some(value => value.pos === '[n-pr]')) {
    output.gloss = [properNoun, ...(output.gloss ?? [])];
  }
  return finish();
}

async function detailedAlternative(
  alternative: PortableAnalysisAlternative,
  hydrate: DetailHydrator
): Promise<PortableLegacyGlossJson> {
  return detailedWord({
    text: alternative.text,
    route: alternative.route,
    reading: alternative.reading,
    score: alternative.score,
    entryIndex: alternative.entryIndex,
    seq: alternative.root?.seq ?? null,
    rootForm: alternative.root?.form ?? alternative.text,
    rootReading: alternative.root?.reading ?? alternative.reading,
    inflection: alternative.inflection,
    components: alternative.components,
    counter: alternative.counter,
    entity: false,
    suffix: false,
    component: false,
    presentationValue: alternative
  }, hydrate);
}

async function detailedToken(
  token: PortableAnalysisToken,
  hydrate: DetailHydrator
): Promise<PortableLegacyGlossJson> {
  if (token.alternatives.length > 1) {
    const alternatives: PortableLegacyGlossJson[] = [];
    for (const alternative of token.alternatives) {
      alternatives.push(await detailedAlternative(alternative, hydrate));
    }
    return { alternative: alternatives };
  }
  return detailedWord({
    text: token.text,
    // Core synthetic entity hints are KanaText records even when their surface
    // contains kanji. Dictionary-backed entity hints retain their real route.
    route: token.route === 'gap' || (token.entity && token.root === null)
      ? 'kana'
      : token.route,
    reading: token.reading,
    score: token.score,
    entryIndex: token.entryIndex,
    seq: token.root?.seq ?? null,
    rootForm: token.root?.form ?? token.text,
    rootReading: token.root?.reading ?? token.reading,
    inflection: token.inflection,
    components: token.components,
    counter: token.counter,
    entity: token.entity,
    suffix: false,
    component: false,
    presentationValue: token
  }, hydrate);
}

/**
 * Cold transformed view backed by DetailStoreReader. Hot presentation context
 * supplies exact sense restrictions and suffix identity without making the
 * cold detail file resident.
 */
export async function serializePortableLegacyDetailed(
  result: PortableAnalysisResult,
  details: DetailStoreReader,
  context: PortableLegacyPresentationContext,
  options: PortableLegacySerializeOptions = {}
): Promise<PortableLegacyTransformedResult> {
  const property = options.wordProperty ?? (() => []);
  const hydrate = new DetailHydrator(details, context);
  const output: Array<string | PortableLegacyTransformedPath[]> = [];
  for (const chunk of result.chunks) {
    if (chunk.type === 'misc') {
      output.push(chunk.text);
      continue;
    }
    const paths: PortableLegacyTransformedPath[] = [];
    for (const path of chunk.paths) {
      const words: PortableLegacyTransformedToken[] = [];
      for (const token of path.tokens) {
        const romanized = tokenRomanized(token, options.method, context);
        words.push([
          romanized,
          await detailedToken(token, hydrate),
          property(romanized, token)
        ]);
      }
      paths.push([words, path.score]);
    }
    output.push(paths);
  }
  return output;
}
