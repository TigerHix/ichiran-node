import type { PortableAnalyzeOptions } from './analyzer-options.js';
import type { PortableAnalysisResult as AnalysisResult } from './analyzer-result-contract.js';
import type { AnalyzerEntityHint as EntityHint } from './analyzer-types.js';
import {
  DictionaryStoreError,
  type DictionaryEntry,
  type DictionaryGloss,
  type DictionaryProperty,
  type DictionarySense,
  type DictionaryStoreErrorCode
} from './dictionary-contract.js';
import type { RomanizationName as RomanizationScheme } from './romanization-contract.js';
import type { TokenDetails } from './token-details-contract.js';
import init, {
  lexicon_prefix_length,
  locale_prefix_length,
  WasmLexiconStore,
  WasmLocaleStore,
  WasmKernel,
  type InitOutput
} from './rust-kernel/generated/ichiran_kernel.js';
import { AnalyzerInputError, validatePortableAnalyzeRequest } from './analyzer-options.js';

interface DictionaryRange {
  readonly offset: number;
  readonly byteLength: number;
}

export type AnalyzerErrorCode =
  | 'invalid-input'
  | 'invalid-pack'
  | 'not-found'
  | 'internal';

export class AnalyzerError extends Error {
  readonly code: AnalyzerErrorCode;

  constructor(code: AnalyzerErrorCode, message: string) {
    super(message);
    this.name = 'AnalyzerError';
    this.code = code;
  }
}

export interface AnalyzeOptions {
  readonly limit?: number;
  readonly entities?: readonly EntityHint[];
  readonly normalizePunctuation?: boolean;
}

export interface RomanizeOptions {
  readonly method?: RomanizationScheme;
  readonly entities?: readonly EntityHint[];
  readonly normalizePunctuation?: boolean;
}

export interface TokenDetailsOptions extends AnalyzeOptions {
  readonly pathIndex: number;
  readonly tokenIndex: number;
  /** BCP 47 locale for dictionary glosses. Defaults to English. */
  readonly locale?: string;
}

export interface DictionaryEntryOptions {
  /** BCP 47 locale for dictionary glosses. Defaults to English. */
  readonly locale?: string;
}

export interface RandomAccessSource {
  readonly byteLength: number;
  read(offset: number, byteLength: number): Promise<Uint8Array>;
  dispose?(): void;
}

export interface AnalyzerSource {
  /** Installed, uncompressed hot pack bytes. */
  readonly hot: Uint8Array;
  /** Language-neutral Japanese lexicon and the digest locale packs bind to. */
  readonly lexicon: {
    readonly source: RandomAccessSource;
    readonly sha256: string;
  };
  /** Installed locale gloss stores, keyed by canonical BCP 47 locale. Must include `en`. */
  readonly locales: Readonly<Record<string, RandomAccessSource>>;
  /** Hosts may supply the emitted WASM bytes when URL loading is unavailable. */
  readonly wasm?: Uint8Array;
}

export interface AnalyzerDiagnostics {
  readonly openMs: number;
  readonly transientBytes: number;
  readonly wasmLinearMemoryBytes: number;
  readonly kernelPayloadBytes: number;
  readonly lexiconResidentBytes: number;
  readonly localeResidentBytes: Readonly<Record<string, number>>;
  readonly workerHeapBytes: number | null;
}

interface RuntimeState {
  readonly kernel: WasmKernel;
  readonly lexicon: WasmLexiconStore;
  readonly lexiconSource: RandomAccessSource;
  readonly locales: ReadonlyMap<string, RuntimeLocale>;
  readonly memory: WebAssembly.Memory;
  readonly openMs: number;
  readonly transientBytes: number;
}

interface RuntimeLocale {
  readonly store: WasmLocaleStore;
  readonly source: RandomAccessSource;
}

type TokenDetailsStep =
  | { readonly state: 'ready'; readonly value: TokenDetails }
  | {
      readonly state: 'missing-dictionary';
      readonly store: 'lexicon' | 'locale' | 'fallback';
      readonly entryIndex: number;
      readonly range: DictionaryRange;
    };

const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8', { fatal: true });
const runtimeStates = new WeakMap<Analyzer, RuntimeState>();
let initialized: Promise<InitOutput> | null = null;

export const ANALYZER_WASM_URL = new URL(
  './rust-kernel/generated/ichiran_kernel_bg.wasm',
  import.meta.url
);

function initialize(wasm?: Uint8Array): Promise<InitOutput> {
  if (initialized) return initialized;
  const attempt = wasm === undefined
    ? init()
    : init({ module_or_path: wasm });
  initialized = attempt;
  void attempt.catch(() => {
    if (initialized === attempt) initialized = null;
  });
  return attempt;
}

function utf16(text: string): Uint16Array {
  const units = new Uint16Array(text.length);
  for (let index = 0; index < text.length; index++) units[index] = text.charCodeAt(index);
  return units;
}

function fromUtf16(units: Uint16Array): string {
  let output = '';
  for (let offset = 0; offset < units.length; offset += 1024) {
    output += String.fromCharCode(...units.subarray(offset, offset + 1024));
  }
  return output;
}

function json<T>(bytes: Uint8Array): T {
  return JSON.parse(decoder.decode(bytes)) as T;
}

function optionsJson(options: ReturnType<typeof validatePortableAnalyzeRequest>['options']): Uint8Array {
  return encoder.encode(JSON.stringify(options));
}

const LOCALE_PATTERN = /^[A-Za-z]{2,8}(?:-[A-Za-z0-9]{1,8})*$/;

function dictionaryLocale(value: unknown, label = 'locale'): string {
  if (typeof value !== 'string' || value.length > 31 || !LOCALE_PATTERN.test(value)) {
    throw new AnalyzerInputError(`${label} must be a valid BCP 47 language tag`);
  }
  try {
    return Intl.getCanonicalLocales(value)[0]!;
  } catch {
    throw new AnalyzerInputError(`${label} must be a valid BCP 47 language tag`);
  }
}

function sha256Bytes(value: string): Uint8Array {
  if (!/^[0-9a-f]{64}$/i.test(value)) {
    throw new AnalyzerInputError('lexicon.sha256 must be a 64-character hexadecimal digest');
  }
  const bytes = new Uint8Array(32);
  for (let index = 0; index < bytes.length; index++) {
    bytes[index] = Number.parseInt(value.slice(index * 2, index * 2 + 2), 16);
  }
  return bytes;
}

interface LexiconEntryWire {
  readonly seq: number;
  readonly forms: DictionaryEntry['forms'];
  readonly senses: readonly {
    readonly ord: number;
    readonly properties: readonly DictionaryProperty[];
  }[];
}

interface LocaleGroupWire {
  readonly targets: readonly number[];
  readonly glosses: readonly DictionaryGloss[];
  readonly info: readonly DictionaryGloss[];
}

interface LocaleEntryWire {
  readonly seq: number;
  readonly groups: readonly LocaleGroupWire[];
}

function exactGroups(entry: LocaleEntryWire, senseOrd: number): readonly LocaleGroupWire[] {
  return entry.groups.filter(group => group.targets.includes(senseOrd));
}

function entryGroups(entry: LocaleEntryWire): readonly LocaleGroupWire[] {
  return entry.groups.filter(group => group.targets.length === 0);
}

function localizeDictionaryEntry(
  lexicon: LexiconEntryWire,
  locale: LocaleEntryWire,
  fallback: LocaleEntryWire
): DictionaryEntry {
  if (lexicon.seq !== locale.seq || lexicon.seq !== fallback.seq) {
    throw new DictionaryStoreError(
      'corrupt-block',
      'Dictionary entry sequence does not match across stores'
    );
  }
  const senseOrds = new Set(lexicon.senses.map(sense => sense.ord));
  for (const entry of [locale, fallback]) {
    for (const group of entry.groups) {
      if (group.targets.some(target => !senseOrds.has(target))) {
        throw new DictionaryStoreError(
          'corrupt-block',
          'Locale group targets a sense that is absent from the lexicon entry'
        );
      }
    }
  }
  const senses: DictionarySense[] = lexicon.senses.map(sense => {
    const selected = exactGroups(locale, sense.ord);
    const english = exactGroups(fallback, sense.ord);
    const glossGroups = selected.some(group => group.glosses.length > 0) ? selected : english;
    const infoGroups = selected.some(group => group.info.length > 0) ? selected : english;
    return {
      ord: sense.ord,
      glosses: glossGroups.flatMap(group => group.glosses),
      properties: [
        ...sense.properties,
        ...infoGroups.flatMap(group => group.info.map(value => ({
          tag: 's_inf' as const,
          ord: value.ord,
          text: value.text
        })))
      ]
    };
  });
  const selectedEntryGroups = entryGroups(locale);
  const fallbackEntryGroups = entryGroups(fallback);
  const entryGlosses = (
    selectedEntryGroups.some(group => group.glosses.length > 0)
      ? selectedEntryGroups : fallbackEntryGroups
  ).flatMap(group => group.glosses);
  const entryInfo = (
    selectedEntryGroups.some(group => group.info.length > 0)
      ? selectedEntryGroups : fallbackEntryGroups
  ).flatMap(group => group.info);
  let nextOrd = lexicon.senses.reduce((maximum, sense) => Math.max(maximum, sense.ord + 1), 0);
  if (entryGlosses.length > 0 || entryInfo.length > 0) {
    senses.push({
      ord: nextOrd++,
      glosses: entryGlosses,
      properties: entryInfo.map(value => ({
        tag: 's_inf',
        ord: value.ord,
        text: value.text
      }))
    });
  }
  return { seq: lexicon.seq, forms: lexicon.forms, senses };
}

const romanizationSchemes = new Set<RomanizationScheme>([
  'hepburn-basic',
  'hepburn-simple',
  'hepburn-passport',
  'hepburn-traditional',
  'hepburn-modified',
  'kunrei-siki'
]);

function romanizationScheme(value: unknown): RomanizationScheme | '' {
  if (value === undefined) return '';
  if (typeof value === 'string' && romanizationSchemes.has(value as RomanizationScheme)) {
    return value as RomanizationScheme;
  }
  throw new AnalyzerInputError('method must be a supported romanization scheme');
}

async function readExact(
  source: RandomAccessSource,
  offset: number,
  byteLength: number,
  code: DictionaryStoreErrorCode
): Promise<Uint8Array> {
  const bytes = await source.read(offset, byteLength);
  if (bytes.byteLength !== byteLength) {
    throw new DictionaryStoreError(
      code,
      `Dictionary source returned ${bytes.byteLength} bytes; expected ${byteLength}`
    );
  }
  return bytes;
}

function now(): number {
  return globalThis.performance?.now() ?? Date.now();
}

function errorCode(error: unknown): unknown {
  return typeof error === 'object' && error !== null && 'code' in error
    ? (error as { readonly code?: unknown }).code
    : undefined;
}

/** Collapse implementation-specific failures into the stable product contract. */
export function analyzerError(
  error: unknown,
  fallback: AnalyzerErrorCode = 'internal'
): AnalyzerError {
  if (error instanceof AnalyzerError) return error;
  const code = errorCode(error);
  let publicCode = fallback;
  if (error instanceof AnalyzerInputError || code === 'invalid-input') {
    publicCode = 'invalid-input';
  } else if (code === 'out-of-range' && fallback === 'not-found') {
    publicCode = 'not-found';
  } else if (
    error instanceof DictionaryStoreError
    || code === 'invalid-header'
    || code === 'unsupported-version'
    || code === 'invalid-directory'
    || code === 'corrupt-section'
    || code === 'corrupt-payload'
    || code === 'corrupt-index'
    || code === 'corrupt-block'
    || code === 'missing-section'
    || code === 'out-of-range'
  ) {
    publicCode = 'invalid-pack';
  }
  const message = error instanceof Error ? error.message : String(error);
  return new AnalyzerError(publicCode, message);
}

function call<T>(operation: () => T, fallback?: AnalyzerErrorCode): T {
  try {
    return operation();
  } catch (error) {
    throw analyzerError(error, fallback);
  }
}

/**
 * The production analyzer. It owns its random-access source after a successful
 * open and releases both WASM state and the source when disposed.
 */
export class Analyzer {
  readonly #kernel: WasmKernel;
  readonly #lexicon: WasmLexiconStore;
  readonly #lexiconSource: RandomAccessSource;
  readonly #locales: ReadonlyMap<string, RuntimeLocale>;
  #disposed = false;

  private constructor(
    kernel: WasmKernel,
    lexicon: WasmLexiconStore,
    lexiconSource: RandomAccessSource,
    locales: ReadonlyMap<string, RuntimeLocale>,
    state: Omit<RuntimeState, 'kernel' | 'lexicon' | 'lexiconSource' | 'locales'>
  ) {
    this.#kernel = kernel;
    this.#lexicon = lexicon;
    this.#lexiconSource = lexiconSource;
    this.#locales = locales;
    runtimeStates.set(this, { kernel, lexicon, lexiconSource, locales, ...state });
  }

  static async open(source: AnalyzerSource): Promise<Analyzer> {
    const started = now();
    let kernel: WasmKernel | null = null;
    let lexicon: WasmLexiconStore | null = null;
    const locales = new Map<string, RuntimeLocale>();
    try {
      if (typeof source !== 'object' || source === null || !source.lexicon) {
        throw new AnalyzerInputError('analyzer source must include a lexicon');
      }
      const digest = sha256Bytes(source.lexicon.sha256);
      const localeSources = new Map<string, RandomAccessSource>();
      for (const [key, localeSource] of Object.entries(source.locales ?? {})) {
        const locale = dictionaryLocale(key, 'locale source key');
        if (locale !== key || localeSources.has(locale)) {
          throw new AnalyzerInputError(`locale source key must be canonical and unique: ${key}`);
        }
        localeSources.set(locale, localeSource);
      }
      if (!localeSources.has('en')) {
        throw new AnalyzerInputError('analyzer source must include the en locale');
      }
      const [wasm, lexiconHeader, localeHeaders] = await Promise.all([
        initialize(source.wasm),
        readExact(source.lexicon.source, 0, 96, 'invalid-header'),
        Promise.all([...localeSources].map(async ([locale, localeSource]) => [
          locale,
          localeSource,
          await readExact(localeSource, 0, 128, 'invalid-header')
        ] as const))
      ]);
      const lexiconPrefixLength = lexicon_prefix_length(
        lexiconHeader,
        source.lexicon.source.byteLength
      );
      const [lexiconPrefix, localePrefixes] = await Promise.all([
        readExact(source.lexicon.source, 0, lexiconPrefixLength, 'corrupt-index'),
        Promise.all(localeHeaders.map(async ([locale, localeSource, header]) => {
          const prefixLength = locale_prefix_length(header, localeSource.byteLength);
          return [
            locale,
            localeSource,
            await readExact(localeSource, 0, prefixLength, 'corrupt-index')
          ] as const;
        }))
      ]);
      kernel = new WasmKernel(source.hot);
      lexicon = new WasmLexiconStore(lexiconPrefix, source.lexicon.source.byteLength);
      for (const [locale, localeSource, prefix] of localePrefixes) {
        locales.set(locale, {
          source: localeSource,
          store: new WasmLocaleStore(
            prefix,
            localeSource.byteLength,
            digest,
            locale,
            lexicon.entry_count()
          )
        });
      }
      const transientBytes = wasm.memory.buffer.byteLength
        + source.hot.byteLength
        + lexiconPrefix.byteLength
        + localePrefixes.reduce((sum, value) => sum + value[2].byteLength, 0);
      return new Analyzer(kernel, lexicon, source.lexicon.source, locales, {
        memory: wasm.memory,
        openMs: now() - started,
        transientBytes
      });
    } catch (error) {
      for (const value of locales.values()) value.store.free();
      lexicon?.free();
      kernel?.free();
      throw analyzerError(error);
    }
  }

  async analyze(text: string, options: AnalyzeOptions = {}): Promise<AnalysisResult> {
    this.#assertOpen();
    try {
      const validated = validatePortableAnalyzeRequest(text, options);
      return call(() => json<AnalysisResult>(
        this.#kernel.analyze_utf16_options(utf16(validated.input), optionsJson(validated.options))
      ));
    } catch (error) {
      throw analyzerError(error);
    }
  }

  async romanize(text: string, options: RomanizeOptions = {}): Promise<string> {
    this.#assertOpen();
    try {
      const validated = validatePortableAnalyzeRequest(text, options as PortableAnalyzeOptions);
      const topPathOptions = { ...validated.options, limit: 1 };
      return call(() => fromUtf16(this.#kernel.romanize_utf16_options(
        utf16(validated.input),
        optionsJson(topPathOptions),
        romanizationScheme(options.method)
      )));
    } catch (error) {
      throw analyzerError(error);
    }
  }

  async details(text: string, options: TokenDetailsOptions): Promise<TokenDetails> {
    this.#assertOpen();
    if (typeof options !== 'object' || options === null || Array.isArray(options)) {
      throw new AnalyzerError('invalid-input', 'token detail options must be an object');
    }
    const { pathIndex, tokenIndex, locale: localeOption = 'en', ...analyzeOptions } = options;
    if (
      !Number.isSafeInteger(pathIndex)
      || pathIndex < 0
      || pathIndex > 0xffff_ffff
      || !Number.isSafeInteger(tokenIndex)
      || tokenIndex < 0
      || tokenIndex > 0xffff_ffff
    ) {
      throw new AnalyzerError(
        'invalid-input',
        'pathIndex and tokenIndex must be non-negative uint32 integers'
      );
    }
    try {
      const locale = dictionaryLocale(localeOption);
      const localized = this.#locales.get(locale);
      if (!localized) {
        throw new AnalyzerError('not-found', `dictionary locale is not installed: ${locale}`);
      }
      const fallback = this.#locales.get('en')!;
      const validated = validatePortableAnalyzeRequest(text, analyzeOptions);
      const operation = call(() => this.#kernel.token_details_begin_utf16(
        utf16(validated.input),
        optionsJson(validated.options),
        pathIndex,
        tokenIndex
      ), 'not-found');
      try {
        for (;;) {
          const step = call(() => json<TokenDetailsStep>(
            operation.token_details_step(
              this.#kernel,
              this.#lexicon,
              localized.store,
              fallback.store
            )
          ));
          if (step.state === 'ready') return step.value;
          const selected = step.store === 'lexicon'
            ? { source: this.#lexiconSource, store: this.#lexicon }
            : step.store === 'locale'
              ? localized
              : fallback;
          const compressed = await readExact(
            selected.source,
            step.range.offset,
            step.range.byteLength,
            'corrupt-block'
          );
          call(
            () => selected.store.entry_json(step.entryIndex, compressed),
            'invalid-pack'
          );
        }
      } finally {
        operation.free();
      }
    } catch (error) {
      throw analyzerError(error, errorCode(error) === 'out-of-range' ? 'not-found' : 'internal');
    }
  }

  async entry(
    entryIndex: number,
    options: DictionaryEntryOptions = {}
  ): Promise<DictionaryEntry> {
    this.#assertOpen();
    if (!Number.isSafeInteger(entryIndex) || entryIndex < 0 || entryIndex > 0xffff_ffff) {
      throw new AnalyzerError('invalid-input', 'entryIndex must be a non-negative uint32 integer');
    }
    if (typeof options !== 'object' || options === null || Array.isArray(options)) {
      throw new AnalyzerError('invalid-input', 'dictionary entry options must be an object');
    }
    try {
      const locale = dictionaryLocale(options.locale ?? 'en');
      const localized = this.#locales.get(locale);
      if (!localized) throw new AnalyzerError('not-found', `dictionary locale is not installed: ${locale}`);
      const fallback = this.#locales.get('en')!;
      const [lexicon, english] = await Promise.all([
        this.#readDictionaryEntry<LexiconEntryWire>(
          this.#lexiconSource,
          this.#lexicon,
          entryIndex
        ),
        this.#readDictionaryEntry<LocaleEntryWire>(fallback.source, fallback.store, entryIndex)
      ]);
      const selected = locale === 'en'
        ? english
        : await this.#readDictionaryEntry<LocaleEntryWire>(
          localized.source,
          localized.store,
          entryIndex
        );
      return localizeDictionaryEntry(lexicon, selected, english);
    } catch (error) {
      throw analyzerError(error);
    }
  }

  dispose(): void {
    if (this.#disposed) return;
    this.#disposed = true;
    runtimeStates.delete(this);
    try {
      for (const value of this.#locales.values()) value.store.free();
      this.#lexicon.free();
    } finally {
      try {
        this.#kernel.free();
      } finally {
        const sources = new Set<RandomAccessSource>([
          this.#lexiconSource,
          ...[...this.#locales.values()].map(value => value.source)
        ]);
        for (const source of sources) source.dispose?.();
      }
    }
  }

  #assertOpen(): void {
    if (this.#disposed) throw new AnalyzerError('internal', 'Analyzer has been disposed');
  }

  async #readDictionaryEntry<T>(
    source: RandomAccessSource,
    store: Pick<WasmLexiconStore, 'range_json' | 'entry_json'>,
    entryIndex: number
  ): Promise<T> {
    const range = call(() => json<DictionaryRange>(store.range_json(entryIndex)), 'not-found');
    const compressed = await readExact(source, range.offset, range.byteLength, 'corrupt-block');
    return call(() => json<T>(store.entry_json(entryIndex, compressed)), 'invalid-pack');
  }
}

/** Qualification-only diagnostics; exported only by the qualification/runtime subpath. */
export function readAnalyzerDiagnostics(analyzer: Analyzer): AnalyzerDiagnostics {
  const state = runtimeStates.get(analyzer);
  if (!state) throw new AnalyzerError('internal', 'Analyzer has been disposed');
  const memory = performance as Performance & {
    readonly memory?: { readonly usedJSHeapSize: number };
  };
  return {
    openMs: state.openMs,
    transientBytes: state.transientBytes,
    wasmLinearMemoryBytes: state.memory.buffer.byteLength,
    kernelPayloadBytes: state.kernel.resident_payload_bytes(),
    lexiconResidentBytes: state.lexicon.resident_bytes(),
    localeResidentBytes: Object.freeze(Object.fromEntries(
      [...state.locales].map(([locale, value]) => [locale, value.store.resident_bytes()])
    )),
    workerHeapBytes: memory.memory?.usedJSHeapSize ?? null
  };
}

/** Internal state bridge used only by explicit qualification modules. */
export function analyzerQualificationState(analyzer: Analyzer): RuntimeState {
  const state = runtimeStates.get(analyzer);
  if (!state) throw new AnalyzerError('internal', 'Analyzer has been disposed');
  return state;
}
