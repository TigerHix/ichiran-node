import type {
  PortableAnalysisAlternative,
  PortableAnalysisComponent,
  PortableAnalysisPath,
  PortableAnalysisResult,
  PortableAnalysisRoot,
  PortableAnalysisToken
} from './analyzer-result-contract.js';
import { stripHints } from './romanization.js';

function root(value: PortableAnalysisRoot | null): PortableAnalysisRoot | null {
  return value === null ? null : { ...value, reading: stripHints(value.reading) };
}

function counter(
  value: readonly [string, boolean] | null
): readonly [string, boolean] | null {
  if (value === null) return null;
  return [value[0].startsWith('Value: ') ? value[0].slice(7) : value[0], value[1]];
}

function positions(values: readonly string[]): readonly string[] {
  return values.map(value => value === 'proper-noun' ? 'n-pr' : value);
}

function component(value: PortableAnalysisComponent): PortableAnalysisComponent {
  return {
    ...value,
    reading: stripHints(value.reading),
    root: root(value.root)
  };
}

function alternative(value: PortableAnalysisAlternative): PortableAnalysisAlternative {
  return {
    ...value,
    reading: stripHints(value.reading),
    pos: positions(value.pos),
    root: root(value.root),
    components: value.components.map(component),
    counter: counter(value.counter)
  };
}

function token(value: PortableAnalysisToken): PortableAnalysisToken {
  return {
    ...value,
    reading: stripHints(value.reading),
    pos: positions(value.pos),
    root: root(value.root),
    components: value.components.map(component),
    alternatives: value.alternatives
      .filter(candidate => candidate.candidateId !== value.candidateId)
      .map(alternative),
    counter: counter(value.counter)
  };
}

function path(value: PortableAnalysisPath): PortableAnalysisPath {
  return { ...value, tokens: value.tokens.map(token) };
}

/** Qualification mirror of the Rust public serializer; never used as an analyzer. */
export function projectProductAnalysis(
  value: PortableAnalysisResult
): PortableAnalysisResult {
  return {
    ...value,
    chunks: value.chunks.map(chunk => chunk.type === 'misc'
      ? chunk
      : { ...chunk, paths: chunk.paths.map(path) }),
    paths: value.paths.map(path)
  };
}
