import { asHiragana } from '@ichiran/core';
import type {
  AnalyzerSupportConjugationSource,
  AnalyzerSupportSuffixFormSource
} from '../browser-pack/analyzer-support.js';
import {
  compileCanonicalSuffixesFromGenerated,
  GENERATED_SUFFIX_ROOTS,
  isSkippedSuffixProperty,
  isWeakSuffixProperty
} from './analyzer-support-suffixes.js';
import {
  readGeneratedOccurrenceSpool,
  readGeneratedPathSpool
} from './generated-projection-spool.js';
import type { GeneratedProjectionStreamResult } from './generated-projection-stream.js';
import type { CanonicalEntry, CanonicalForm, ConjugationProperty } from './model.js';

interface SuffixPath {
  readonly rootSeq: number;
  readonly targetSeq: number;
  readonly viaTargetSeq: number | null;
  readonly property: ConjugationProperty;
}

interface TargetConjugations {
  readonly rootSeq: number;
  readonly targetSeq: number;
  readonly paths: SuffixPath[];
  readonly byKanaSurface: Map<string, Map<string, AnalyzerSupportConjugationSource>>;
}

type SuffixCompilation = ReturnType<typeof compileCanonicalSuffixesFromGenerated>;

function commonTags(form: CanonicalForm): string {
  return form.priorityTags.map(tag => `[${tag}]`).join('');
}

function targetKey(rootSeq: number, targetSeq: number): string {
  return `${rootSeq}\u0000${targetSeq}`;
}

function formKey(seq: number, text: string): string {
  return `${seq}\u0000${text}`;
}

function conjugation(path: SuffixPath): AnalyzerSupportConjugationSource {
  return {
    seq: path.targetSeq,
    from: path.rootSeq,
    via: path.viaTargetSeq,
    ...path.property
  };
}

function addSurfaceConjugation(
  target: TargetConjugations,
  surface: string,
  value: AnalyzerSupportConjugationSource
): void {
  const values = target.byKanaSurface.get(surface)
    ?? new Map<string, AnalyzerSupportConjugationSource>();
  values.set(JSON.stringify(value), value);
  target.byKanaSurface.set(surface, values);
}

function suffixForm(
  target: GeneratedProjectionStreamResult['targets'][number],
  ordinal: number,
  text: string,
  lexical: CanonicalForm | undefined,
  conjugations: readonly AnalyzerSupportConjugationSource[] | null
): AnalyzerSupportSuffixFormSource {
  return {
    seq: target.seq,
    text,
    bestKanji: lexical?.best ?? null,
    commonTags: lexical ? commonTags(lexical) : '',
    ord: lexical?.ordinal ?? ordinal,
    common: lexical?.common ?? null,
    conjugatable: lexical?.conjugatable ?? target.conjugatable,
    nokanji: lexical?.noKanji ?? false,
    conjugations
  };
}

/** Build suffix forms from physical targets, preserving matrix-only text rows. */
function generatedSuffixForms(input: {
  readonly entries: readonly CanonicalEntry[];
  readonly projection: GeneratedProjectionStreamResult;
}): ReadonlyMap<number, readonly AnalyzerSupportSuffixFormSource[]> {
  const entryBySeq = new Map(input.entries.map(entry => [entry.seq, entry]));
  const pathByOrdinal = new Map<number, SuffixPath>();
  const targets = new Map<string, TargetConjugations>();

  for (const path of readGeneratedPathSpool(input.projection.pathsPath)) {
    if (!GENERATED_SUFFIX_ROOTS.has(path.rootSeq)) continue;
    const alias = path.secondAlias ?? path.firstAlias;
    const property = input.projection.aliasProperties[alias];
    if (!property) throw new Error(`Suffix path has unknown property alias ${alias}`);
    const selected = { ...path, property };
    pathByOrdinal.set(path.ordinal, selected);
    const key = targetKey(path.rootSeq, path.targetSeq);
    const target = targets.get(key) ?? {
      rootSeq: path.rootSeq,
      targetSeq: path.targetSeq,
      paths: [],
      byKanaSurface: new Map<string, Map<string, AnalyzerSupportConjugationSource>>()
    };
    target.paths.push(selected);
    targets.set(key, target);
  }

  for (const occurrence of readGeneratedOccurrenceSpool(input.projection.occurrencesPath)) {
    const path = pathByOrdinal.get(occurrence.pathOrdinal);
    if (!path || isWeakSuffixProperty(path.property)) continue;
    const target = targets.get(targetKey(path.rootSeq, path.targetSeq))!;
    const surface = occurrence.route === 'kana'
      ? occurrence.surface
      : occurrence.physicalCounterpart;
    if (surface !== null) {
      for (const groupedPath of target.paths) {
        addSurfaceConjugation(target, surface, conjugation(groupedPath));
      }
    }
  }

  const selectedTargetSeqs = new Set([...targets.values()].map(value => value.targetSeq));
  const targetBySeq = new Map<number, GeneratedProjectionStreamResult['targets'][number]>();
  for (const target of input.projection.targets) {
    if (selectedTargetSeqs.has(target.seq)) targetBySeq.set(target.seq, target);
  }

  const output = new Map<number, AnalyzerSupportSuffixFormSource[]>();
  for (const selected of targets.values()) {
    if (selected.paths.every(path => isSkippedSuffixProperty(path.property))) continue;
    const target = targetBySeq.get(selected.targetSeq);
    if (!target) throw new Error(`Suffix path references missing target ${selected.targetSeq}`);
    const lexical = entryBySeq.get(target.seq);
    const forms = output.get(selected.rootSeq) ?? [];
    const admittedReadings = new Set(
      [...selected.byKanaSurface.keys()].map(asHiragana)
    );
    target.kana.forEach((text, ordinal) => {
      if (!selected.byKanaSurface.has(text)
        && (!lexical || !admittedReadings.has(asHiragana(text)))) return;
      const values = selected.byKanaSurface.get(text);
      forms.push(suffixForm(
        target,
        ordinal,
        text,
        lexical?.kana.find(form => form.text === text),
        values && values.size > 0 ? [...values.values()] : null
      ));
    });
    output.set(selected.rootSeq, forms);
  }
  return output;
}

function hydrateNullConjugations(
  projection: GeneratedProjectionStreamResult,
  compilation: SuffixCompilation
): SuffixCompilation {
  const selectedTargets = new Set(compilation.suffixes.flatMap(suffix =>
    suffix.values.flatMap(value => value.form?.conjugations === null ? [value.form.seq] : [])));
  const paths = new Map<number, SuffixPath>();
  for (const path of readGeneratedPathSpool(projection.pathsPath)) {
    if (!selectedTargets.has(path.targetSeq)) continue;
    const alias = path.secondAlias ?? path.firstAlias;
    const property = projection.aliasProperties[alias];
    if (!property) throw new Error(`Suffix hydration path has unknown property alias ${alias}`);
    paths.set(path.ordinal, { ...path, property });
  }
  const byForm = new Map<string, Map<string, AnalyzerSupportConjugationSource>>();
  const add = (
    seq: number,
    text: string,
    value: AnalyzerSupportConjugationSource
  ): void => {
    const key = formKey(seq, text);
    const values = byForm.get(key) ?? new Map<string, AnalyzerSupportConjugationSource>();
    values.set(JSON.stringify(value), value);
    byForm.set(key, values);
  };
  for (const occurrence of readGeneratedOccurrenceSpool(projection.occurrencesPath)) {
    const path = paths.get(occurrence.pathOrdinal);
    if (!path) continue;
    const value = conjugation(path);
    if (occurrence.route === 'kana') {
      add(path.targetSeq, occurrence.surface, value);
    } else if (occurrence.physicalCounterpart !== null) {
      add(path.targetSeq, occurrence.physicalCounterpart, value);
    }
  }
  return {
    suffixClasses: compilation.suffixClasses,
    suffixes: compilation.suffixes.map(suffix => ({
      ...suffix,
      values: suffix.values.map(value => {
        const form = value.form;
        if (form?.conjugations !== null) return value;
        const conjugations = [...(byForm.get(formKey(form.seq, form.text))?.values() ?? [])];
        return {
          ...value,
          form: { ...form, conjugations: conjugations.length > 0 ? conjugations : null }
        };
      })
    }))
  };
}

/** Build only the physical generated forms referenced by fixed suffix declarations. */
export function compileBoundedCanonicalSuffixes(input: {
  readonly entries: readonly CanonicalEntry[];
  readonly projection: GeneratedProjectionStreamResult;
}): ReturnType<typeof compileCanonicalSuffixesFromGenerated> {
  const compilation = compileCanonicalSuffixesFromGenerated(
    input.entries,
    generatedSuffixForms(input)
  );
  return hydrateNullConjugations(input.projection, compilation);
}
