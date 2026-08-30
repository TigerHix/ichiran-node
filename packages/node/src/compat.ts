import type {
  DetailEntry,
  IchiranRuntime,
  PortableLegacyConjugationJson,
  PortableLegacyConjugationInfoFacts,
  PortableLegacyGlossJson,
  PortableLegacySenseJson,
  PortableLegacyTransformedToken
} from '@ichiran/core';
import {
  joinRomanizedParts,
  PORTABLE_LEGACY_INFO
} from '@ichiran/core';

interface FormatContext {
  readonly senses: ReadonlyMap<number, readonly PortableLegacySenseJson[]>;
}

function properties(sense: DetailEntry['senses'][number], tag: string): string[] {
  return sense.properties
    .filter(property => property.tag === tag)
    .sort((left, right) => left.ord - right.ord)
    .map(property => property.text);
}

const PRESENTED_PROPERTY_TAGS = new Set(['field', 'pos', 's_inf', 'stagk', 'stagr']);

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

/** The unfiltered get-senses view used by the PostgreSQL word-info-str path. */
function detailSenses(entry: DetailEntry): PortableLegacySenseJson[] {
  const result: PortableLegacySenseJson[] = [];
  const reversed = finalPropertyGroup(entry);
  for (const sense of [...entry.senses].sort((left, right) => left.ord - right.ord)) {
    const values = (tag: string): string[] => {
      const found = properties(sense, tag);
      return reversed?.senseOrd === sense.ord && reversed.tag === tag
        ? found.reverse()
        : found;
    };
    const pos = values('pos');
    const field = values('field');
    const info = values('s_inf');
    const value: {
      pos: string;
      gloss: string;
      field?: string;
      info?: string;
    } = {
      pos: `[${pos.join(',')}]`,
      gloss: [...sense.glosses]
        .sort((left, right) => left.ord - right.ord)
        .map(gloss => gloss.text)
        .join('; ')
    };
    if (field.length > 0) value.field = `{${field.join(',')}}`;
    if (info.length > 0) value.info = info.join('; ');
    result.push(value);
  }
  return result;
}

function formatSenses(senses: readonly PortableLegacySenseJson[]): string {
  const lines: string[] = [];
  let position = '';

  for (const [index, sense] of senses.entries()) {
    if (sense.pos !== '[]') position = sense.pos;
    const parts = [`${index + 1}. ${position}`];
    // The detailed legacy serializer already applies the historical braces.
    if (sense.field) parts.push(sense.field);
    if (sense.info) parts.push(`《${sense.info}》`);
    parts.push(sense.gloss);
    lines.push(parts.join(' '));
  }

  return lines.join('\n');
}

function formatConjugationProperty(
  property: PortableLegacyConjugationJson['prop'][number],
  facts: PortableLegacyConjugationInfoFacts['flags'][number] | undefined
): string {
  let output = `[${property.pos}] ${property.type}`;
  if (facts) {
    if (facts.negative !== null) output += facts.negative ? ' Negative' : ' Affirmative';
    if (facts.formal !== null) output += facts.formal ? ' Formal' : ' Plain';
  } else {
    if (property.neg !== undefined) output += property.neg ? ' Negative' : ' Affirmative';
    if (property.fml !== undefined) output += property.fml ? ' Formal' : ' Plain';
  }
  return output;
}

function appendConjugations(
  output: string[],
  conjugations: readonly PortableLegacyConjugationJson[]
): void {
  for (const conjugation of conjugations) {
    const facts = conjugation[PORTABLE_LEGACY_INFO];
    let first = true;
    for (const [index, property] of conjugation.prop.entries()) {
      output.push(
        `\n${first ? '[' : ' '} Conjugation: ${formatConjugationProperty(
          property,
          facts?.flags[index]
        )}`
      );
      first = false;
    }

    if (conjugation.via && conjugation.via.length > 0) {
      output.push('\n --(via)--');
      appendConjugations(output, conjugation.via);
    } else {
      output.push(
        `\n  ${conjugation.reading ?? ''} : ${facts?.shortGloss
          ?? conjugation.gloss?.[0]?.gloss
          ?? ''}`
      );
    }
    output.push(' ]');
  }
}

function appendWord(
  output: string[],
  word: PortableLegacyGlossJson,
  marker = false,
  context?: FormatContext
): void {
  const facts = word[PORTABLE_LEGACY_INFO];
  const definitionSeq = facts?.definitionSeq;
  const exactSenses = facts?.conjugationSelection === 'default' && facts.inflected
    ? []
    : definitionSeq === null || definitionSeq === undefined
      ? undefined
      : context?.senses.get(definitionSeq);
  if (marker) output.push(' * ');
  output.push(word.reading ?? word.text ?? '???');

  if (word.components && word.components.length > 0) {
    const componentTexts = word.compound ?? word.components.map(component => component.text ?? '');
    output.push(` Compound word: ${componentTexts.join(' + ')}`);
    for (const component of word.components) {
      output.push('\n');
      appendWord(output, component, true, context);
    }
    return;
  }

  if (word.counter) {
    output.push(`\n${word.counter.value}`);
    if (facts ? definitionSeq !== null : typeof word.seq === 'number') {
      output.push(`\n${formatSenses(exactSenses ?? word.gloss ?? [])}`);
    }
    return;
  }

  if (word.suffix) {
    output.push(`  [suffix]: ${word.suffix} `);
  } else if (facts) {
    if (facts.conjugationSelection !== 'explicit') {
      output.push('\n');
      output.push(definitionSeq === null ? '???' : formatSenses(exactSenses ?? []));
    }
  } else if (word.gloss) {
    output.push(`\n${formatSenses(word.gloss)}`);
  } else if (word.seq === undefined) {
    output.push('\n???');
  } else if (!word.conj || word.conj.length === 0) {
    // An ordinary entry with no senses prints the same empty sense line as
    // get-senses-str. The transformed model omits an empty `gloss` array.
    output.push('\n');
  }

  if (word.conj && word.conj.length > 0) appendConjugations(output, word.conj);
}

/** Legacy `word-info-str` projection; runtime context supplies physical senses. */
export function formatLegacyWordInfo(
  word: PortableLegacyGlossJson,
  context?: FormatContext
): string {
  const output: string[] = [];
  if (word.alternative) {
    for (const [index, alternative] of word.alternative.entries()) {
      if (index > 0) output.push('\n');
      output.push(`<${index + 1}>. `);
      appendWord(output, alternative, false, context);
    }
  } else {
    appendWord(output, word, false, context);
  }
  return output.join('');
}

function topLegacyProjection(value: unknown): {
  readonly parts: readonly string[];
  readonly tokens: readonly PortableLegacyTransformedToken[];
} {
  if (!Array.isArray(value)) return { parts: [], tokens: [] };
  const parts: string[] = [];
  const tokens: PortableLegacyTransformedToken[] = [];
  for (const chunk of value) {
    if (typeof chunk === 'string') {
      parts.push(chunk);
      continue;
    }
    if (!Array.isArray(chunk) || !Array.isArray(chunk[0])) continue;
    const firstPath = chunk[0];
    if (!Array.isArray(firstPath[0])) continue;
    for (const token of firstPath[0]) {
      if (
        Array.isArray(token)
        && typeof token[0] === 'string'
        && typeof token[1] === 'object'
        && token[1] !== null
      ) {
        const transformed = token as unknown as PortableLegacyTransformedToken;
        tokens.push(transformed);
        parts.push(transformed[0]);
      }
    }
  }
  return { parts, tokens };
}

function collectDefinitionSeqs(
  word: PortableLegacyGlossJson,
  output: Set<number>
): void {
  for (const component of word.components ?? []) collectDefinitionSeqs(component, output);
  for (const alternative of word.alternative ?? []) collectDefinitionSeqs(alternative, output);
  if ((word.components?.length ?? 0) > 0 || (word.alternative?.length ?? 0) > 0) return;
  const facts = word[PORTABLE_LEGACY_INFO];
  const needsSenses = word.counter !== undefined || (
    word.suffix === undefined
    && facts?.conjugationSelection !== 'explicit'
    && !(facts?.conjugationSelection === 'default' && facts.inflected)
  );
  if (needsSenses && facts?.definitionSeq !== null && facts?.definitionSeq !== undefined) {
    output.add(facts.definitionSeq);
  }
}

async function physicalSenseContext(
  runtime: IchiranRuntime,
  tokens: readonly PortableLegacyTransformedToken[]
): Promise<FormatContext> {
  const definitionSeqs = new Set<number>();
  for (const [, word] of tokens) collectDefinitionSeqs(word, definitionSeqs);
  const senses = new Map<number, readonly PortableLegacySenseJson[]>();
  await Promise.all([...definitionSeqs].map(async definitionSeq => {
    const entryIndex = runtime.roots.findEntryIndex(definitionSeq);
    senses.set(
      definitionSeq,
      entryIndex < 0 ? [] : detailSenses(await runtime.describe(entryIndex))
    );
  }));
  return { senses };
}

/** Compatibility projection for the historical romanize(..., withInfo) API. */
export async function romanizeWithInfo(
  runtime: IchiranRuntime,
  text: string,
  normalizePunctuation = false
): Promise<{ readonly romanized: string; readonly info: readonly (readonly [string, string])[] }> {
  const legacy = await runtime.legacy(text, { limit: 1, normalizePunctuation });
  const projection = topLegacyProjection(legacy);
  const context = await physicalSenseContext(runtime, projection.tokens);
  const info = [...projection.tokens]
    .reverse()
    .map(([tokenRomanized, word]) => [
      tokenRomanized,
      formatLegacyWordInfo(word, context)
    ] as const);
  return { romanized: joinRomanizedParts(projection.parts), info };
}
