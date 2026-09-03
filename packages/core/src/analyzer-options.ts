import type { AnalyzerEntityHint } from './analyzer-types.js';

/** Public bounds keep one malformed request from monopolizing an edge device. */
export const MAX_ANALYZER_LIMIT = 10;
export const MAX_ANALYZER_TEXT_LENGTH = 4096;
export const MAX_ANALYZER_WORD_LENGTH = 256;
export const MAX_ANALYZER_ENTITIES = 64;
export const MAX_ANALYZER_ENTITY_ABS_BOOST = 1_000_000;

export interface PortableAnalyzeOptions {
  readonly limit?: number;
  readonly entities?: readonly AnalyzerEntityHint[];
  /** Match romanize* defaults: punctuation is preserved unless explicitly normalized. */
  readonly normalizePunctuation?: boolean;
}

export interface ValidatedPortableAnalyzeOptions {
  readonly limit: number;
  readonly entities: readonly AnalyzerEntityHint[];
  readonly normalizePunctuation: boolean;
}

export class AnalyzerInputError extends RangeError {
  constructor(message: string) {
    super(message);
    this.name = 'AnalyzerInputError';
  }
}

export function validateAnalyzerLimit(value: unknown, fallback = 5): number {
  const limit = value ?? fallback;
  if (!Number.isSafeInteger(limit) || (limit as number) < 1 || (limit as number) > MAX_ANALYZER_LIMIT) {
    throw new AnalyzerInputError(`limit must be an integer from 1 to ${MAX_ANALYZER_LIMIT}`);
  }
  return limit as number;
}

export function validateAnalyzerEntities(
  textLength: number,
  value: unknown
): readonly AnalyzerEntityHint[] {
  if (value === undefined) return [];
  if (!Array.isArray(value)) throw new AnalyzerInputError('entities must be an array');
  if (value.length > MAX_ANALYZER_ENTITIES) {
    throw new AnalyzerInputError(`entities must contain at most ${MAX_ANALYZER_ENTITIES} hints`);
  }
  return value.map((candidate, index) => {
    if (typeof candidate !== 'object' || candidate === null) {
      throw new AnalyzerInputError(`entities[${index}] must be an object`);
    }
    const hint = candidate as Partial<AnalyzerEntityHint>;
    if (
      !Number.isSafeInteger(hint.start)
      || !Number.isSafeInteger(hint.end)
      || (hint.start as number) < 0
      || (hint.end as number) <= (hint.start as number)
      || (hint.end as number) > textLength
    ) {
      throw new AnalyzerInputError(
        `entities[${index}] must be a non-empty span within the input`
      );
    }
    const boost = hint.boost;
    if (
      boost !== undefined
      && (!Number.isFinite(boost) || Math.abs(boost) > MAX_ANALYZER_ENTITY_ABS_BOOST)
    ) {
      throw new AnalyzerInputError(
        `entities[${index}].boost must be finite and between -${MAX_ANALYZER_ENTITY_ABS_BOOST} and ${MAX_ANALYZER_ENTITY_ABS_BOOST}`
      );
    }
    return boost === undefined
      ? { start: hint.start as number, end: hint.end as number }
      : { start: hint.start as number, end: hint.end as number, boost };
  });
}

export function validatePortableAnalyzeRequest(
  input: unknown,
  options: PortableAnalyzeOptions = {}
): { readonly input: string; readonly options: ValidatedPortableAnalyzeOptions } {
  if (typeof input !== 'string') throw new AnalyzerInputError('text must be a string');
  if (typeof options !== 'object' || options === null || Array.isArray(options)) {
    throw new AnalyzerInputError('analyzer options must be an object');
  }
  if (input.length > MAX_ANALYZER_TEXT_LENGTH) {
    throw new AnalyzerInputError(
      `text must contain at most ${MAX_ANALYZER_TEXT_LENGTH} UTF-16 code units`
    );
  }
  if (
    options.normalizePunctuation !== undefined
    && typeof options.normalizePunctuation !== 'boolean'
  ) {
    throw new AnalyzerInputError('normalizePunctuation must be a boolean');
  }
  return {
    input,
    options: {
      limit: validateAnalyzerLimit(options.limit),
      entities: validateAnalyzerEntities(input.length, options.entities),
      normalizePunctuation: options.normalizePunctuation ?? false
    }
  };
}
