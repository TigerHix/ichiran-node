import { validatePortableAnalyzeRequest, type PortableAnalyzeOptions } from './analyzer-options.js';
import { DictionaryStoreError } from './dictionary-contract.js';
import { PORTABLE_LEGACY_INFO } from './legacy-contract.js';
import type { RomanizationName } from './romanization-contract.js';
import {
  analyzerError,
  analyzerQualificationState,
  readAnalyzerDiagnostics,
  type Analyzer,
  type AnalyzerDiagnostics
} from './runtime.js';

interface DictionaryRange {
  readonly offset: number;
  readonly byteLength: number;
}

type LegacyStep =
  | { readonly state: 'ready'; readonly value: unknown; readonly metadata: LegacyMetadata }
  | {
      readonly state: 'missing-dictionary';
      readonly store: 'lexicon' | 'locale' | 'fallback';
      readonly entryIndex: number;
      readonly range: DictionaryRange;
    };

interface LegacyMetadata {
  readonly words: readonly (Record<string, unknown> | null)[];
  readonly conjugations: readonly (Record<string, unknown> | null)[];
}

const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8', { fatal: true });

function utf16(text: string): Uint16Array {
  const units = new Uint16Array(text.length);
  for (let index = 0; index < text.length; index++) units[index] = text.charCodeAt(index);
  return units;
}

function json<T>(bytes: Uint8Array): T {
  return JSON.parse(decoder.decode(bytes)) as T;
}

function revive(value: unknown, metadata: LegacyMetadata): unknown {
  let wordIndex = 0;
  let conjugationIndex = 0;
  const object = (target: unknown, label: string): Record<string | symbol, unknown> => {
    if (typeof target !== 'object' || target === null || Array.isArray(target)) {
      throw new Error(`Invalid Rust legacy ${label} value`);
    }
    return target as Record<string | symbol, unknown>;
  };
  const attach = (target: Record<string | symbol, unknown>, facts: unknown): void => {
    if (typeof facts === 'object' && facts !== null) {
      Object.defineProperty(target, PORTABLE_LEGACY_INFO, { value: facts });
    }
  };
  const visitConjugation = (target: unknown): void => {
    const conjugation = object(target, 'conjugation');
    attach(conjugation, metadata.conjugations[conjugationIndex++]);
    if (Array.isArray(conjugation.via)) {
      for (const child of conjugation.via) visitConjugation(child);
    }
  };
  const visitWord = (target: unknown): void => {
    const word = object(target, 'word');
    attach(word, metadata.words[wordIndex++]);
    if (Array.isArray(word.components)) {
      for (const component of word.components) visitWord(component);
    }
    if (Array.isArray(word.alternative)) {
      for (const alternative of word.alternative) visitWord(alternative);
    }
    if (Array.isArray(word.conj)) {
      for (const conjugation of word.conj) visitConjugation(conjugation);
    }
  };
  if (!Array.isArray(value)) throw new Error('Invalid Rust legacy output');
  for (const chunk of value) {
    if (typeof chunk === 'string') continue;
    if (!Array.isArray(chunk)) throw new Error('Invalid Rust legacy chunk');
    for (const path of chunk) {
      if (!Array.isArray(path) || !Array.isArray(path[0])) throw new Error('Invalid Rust legacy path');
      for (const token of path[0]) {
        if (!Array.isArray(token)) throw new Error('Invalid Rust legacy token');
        visitWord(token[1]);
      }
    }
  }
  if (wordIndex !== metadata.words.length || conjugationIndex !== metadata.conjugations.length) {
    throw new Error('Rust legacy metadata shape does not match the serialized result');
  }
  return value;
}

/** Frozen detailed projection retained solely for differential qualification. */
export async function legacyAnalysis(
  analyzer: Analyzer,
  text: string,
  options: PortableAnalyzeOptions & {
    readonly method?: RomanizationName;
    readonly locale?: string;
  } = {}
): Promise<unknown> {
  const state = analyzerQualificationState(analyzer);
  const english = state.locales.get('en');
  if (!english) throw new Error('English dictionary locale is not installed');
  const selected = state.locales.get(options.locale ?? 'en');
  if (!selected) throw new Error(`Dictionary locale is not installed: ${options.locale}`);
  try {
    const { locale: _locale, method: _method, ...analyzeOptions } = options;
    const validated = validatePortableAnalyzeRequest(text, analyzeOptions);
    const operation = state.kernel.legacy_begin_utf16(
      utf16(validated.input),
      encoder.encode(JSON.stringify(validated.options)),
      options.method ?? ''
    );
    try {
      const loaded = new Set<string>();
      for (;;) {
        const step = json<LegacyStep>(operation.legacy_step(
          state.kernel,
          state.lexicon,
          selected.store,
          english.store
        ));
        if (step.state === 'ready') return revive(step.value, step.metadata);
        const key = `${step.store}:${step.entryIndex}:${step.range.offset}:${step.range.byteLength}`;
        if (loaded.has(key)) throw new Error(`Detail range ${key} remained unavailable after preload`);
        loaded.add(key);
        const requested = step.store === 'lexicon'
          ? { source: state.lexiconSource, store: state.lexicon }
          : step.store === 'locale'
            ? selected
            : english;
        const compressed = await requested.source.read(step.range.offset, step.range.byteLength);
        if (compressed.byteLength !== step.range.byteLength) {
          throw new DictionaryStoreError(
            'corrupt-block',
            `Dictionary source returned ${compressed.byteLength} bytes; expected ${step.range.byteLength}`
          );
        }
        requested.store.entry_json(step.entryIndex, compressed);
      }
    } finally {
      operation.free();
    }
  } catch (error) {
    throw analyzerError(error);
  }
}

export { readAnalyzerDiagnostics, type AnalyzerDiagnostics };
