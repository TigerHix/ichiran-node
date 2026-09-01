import { unrendaku } from '@ichiran/core';
import type {
  AnalyzerSupportCollisionSource,
  AnalyzerSupportHintSource,
  AnalyzerSupportSplitKind,
  AnalyzerSupportSplitPartSource,
  AnalyzerSupportSplitSource
} from '../browser-pack/analyzer-support.js';
import type {
  CompiledMorphologyArtifact,
  CompiledMorphologyRule
} from '../browser-pack/morphology-format.js';
import type {
  AnnotationCandidate,
  SplitAttributes,
  SplitDeclaration,
  SplitPartResolver
} from './analyzer-support-annotation-model.js';
import {
  LEGACY_EASY_HINT_DECLARATIONS,
  UPSTREAM_260118_EASY_HINT_DECLARATIONS
} from './analyzer-support-easy-hint-declarations.js';
import {
  LEGACY_SIMPLE_HINT_DECLARATIONS,
  UPSTREAM_260118_SIMPLE_HINT_DECLARATIONS,
  type HintCompiler
} from './analyzer-support-simple-hint-declarations.js';
import {
  SEGMENT_SPLIT_DECLARATIONS,
  SPLIT_DECLARATIONS
} from './analyzer-support-split-declarations.js';
import {
  compileEasyHint,
  type KanjidicHintReadings
} from './kanjidic-hints.js';
import type { CanonicalEntry, CanonicalRoute } from './model.js';

export interface AnalyzerAnnotationCompilation {
  readonly splits: readonly AnalyzerSupportSplitSource[];
  readonly hints: readonly AnalyzerSupportHintSource[];
}

export interface AnalyzerAnnotationInput {
  readonly entries: readonly CanonicalEntry[];
  readonly morphology: CompiledMorphologyArtifact;
  readonly collisions: readonly AnalyzerSupportCollisionSource[];
  readonly partResolver: SplitPartResolver;
  readonly kanjidicReadings: KanjidicHintReadings;
}

type HintDeclarationCompiler = (
  candidate: AnnotationCandidate,
  readings: KanjidicHintReadings
) => string | null;

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function safeSlice(text: string, start: number, end?: number): string | null {
  if (start < 0 || start > text.length) return null;
  if (end !== undefined && (end < start || end > text.length)) return null;
  return text.slice(start, end);
}

function candidateKey(value: AnnotationCandidate): string {
  return JSON.stringify([
    value.rootSeq,
    value.route,
    value.surface,
    value.form,
    value.reading,
    value.ruleIds
  ]);
}

function splitKey(value: Pick<
  AnalyzerSupportSplitSource,
  'definitionSeq' | 'route' | 'surface' | 'kind'
>): string {
  return JSON.stringify([value.definitionSeq, value.route, value.surface, value.kind]);
}

function hintKey(value: Pick<
  AnalyzerSupportHintSource,
  'definitionSeq' | 'route' | 'surface' | 'reading'
>): string {
  return JSON.stringify([value.definitionSeq, value.route, value.surface, value.reading]);
}

function collisionKey(value: {
  readonly rootSeq: number;
  readonly ruleIds: readonly [number] | readonly [number, number];
  readonly route: CanonicalRoute;
  readonly surface: string;
}): string {
  return JSON.stringify([value.rootSeq, value.ruleIds, value.route, value.surface]);
}

function applyMorphologyRule(word: string, rule: CompiledMorphologyRule): string {
  const kana = /^[ァ-ヺヽヾーぁ-ゔゝゞー]+$/.test(word.slice(Math.max(0, word.length - 2)));
  const euphony = kana ? rule.euphr : rule.euphk;
  return word.slice(0, word.length - rule.stem - (euphony.length > 0 ? 1 : 0))
    + euphony + rule.okuri;
}

/** Enumerate the same semantic morphology candidates used by annotation rules. */
export function enumerateAnnotationCandidates(
  artifact: CompiledMorphologyArtifact,
  selectedRoots: ReadonlySet<number>
): AnnotationCandidate[] {
  const templatesByPos = new Map<string, typeof artifact.templates>();
  for (const template of artifact.templates) {
    const pos = artifact.rules[template.firstRule]!.pos;
    const values = templatesByPos.get(pos) ?? [];
    (values as typeof artifact.templates[number][]).push(template);
    templatesByPos.set(pos, values);
  }
  const rootForms = new Map(artifact.rootGroups.map(group => [group.seq, new Set(group.forms)]));
  const tombstones = new Set(artifact.tombstones.map(value => JSON.stringify([
    value.route,
    value.surface,
    value.rootSeq,
    value.firstRule,
    value.secondRule
  ])));
  const candidates = new Map<string, AnnotationCandidate>();

  for (const key of artifact.rootKeys) {
    const templates = templatesByPos.get(key.pos) ?? [];
    for (const record of key.records) {
      const group = artifact.rootGroups[record.rootGroup]!;
      if (!selectedRoots.has(group.seq)) continue;
      for (const template of templates) {
        const first = artifact.rules[template.firstRule]!;
        const second = template.secondRule === null ? null : artifact.rules[template.secondRule]!;
        const intermediateSurface = applyMorphologyRule(key.sourceText, first);
        const surface = second
          ? applyMorphologyRule(intermediateSurface, second)
          : intermediateSurface;
        if (rootForms.get(group.seq)?.has(surface)) continue;
        if (tombstones.has(JSON.stringify([
          key.route,
          surface,
          group.seq,
          template.firstRule,
          template.secondRule
        ]))) continue;
        const intermediateForm = applyMorphologyRule(record.sourceForm, first);
        const intermediateReading = applyMorphologyRule(record.sourceReading, first);
        const value: AnnotationCandidate = {
          rootSeq: group.seq,
          route: key.route,
          surface,
          form: second ? applyMorphologyRule(intermediateForm, second) : intermediateForm,
          reading: second ? applyMorphologyRule(intermediateReading, second) : intermediateReading,
          ord: record.ord,
          common: record.common,
          ruleIds: template.secondRule === null
            ? [template.firstRule]
            : [template.firstRule, template.secondRule]
        };
        candidates.set(candidateKey(value), value);
      }
    }
  }

  for (const patch of artifact.patches) {
    if (!selectedRoots.has(patch.rootSeq)) continue;
    const value: AnnotationCandidate = {
      rootSeq: patch.rootSeq,
      route: patch.route,
      surface: patch.surface,
      form: patch.form,
      reading: patch.reading,
      ord: patch.ord,
      common: patch.common,
      ruleIds: patch.secondRule === null
        ? [patch.firstRule]
        : [patch.firstRule, patch.secondRule]
    };
    candidates.set(candidateKey(value), value);
  }
  return [...candidates.values()].sort((left, right) =>
    compareText(candidateKey(left), candidateKey(right)));
}

function directCandidates(
  entries: readonly CanonicalEntry[],
  roots: ReadonlySet<number>
): AnnotationCandidate[] {
  const output: AnnotationCandidate[] = [];
  for (const entry of entries) {
    if (!roots.has(entry.seq)) continue;
    for (const [route, forms] of [
      ['kanji', entry.kanji],
      ['kana', entry.kana]
    ] as const) {
      for (const form of forms) {
        output.push({
          rootSeq: entry.seq,
          route,
          surface: form.text,
          form: route === 'kanji' ? form.text : form.best ?? form.text,
          reading: route === 'kana' ? form.text : form.best ?? form.text,
          ord: form.ordinal,
          common: form.common,
          ruleIds: null
        });
      }
    }
  }
  return output.sort((left, right) => compareText(candidateKey(left), candidateKey(right)));
}

function declarationsBySeq(
  declarations: readonly SplitDeclaration[],
  label: string
): ReadonlyMap<number, SplitDeclaration> {
  const result = new Map<number, SplitDeclaration>();
  for (const declaration of declarations) {
    if (result.has(declaration.seq)) throw new Error(`Duplicate ${label} ${declaration.seq}`);
    result.set(declaration.seq, declaration);
  }
  return result;
}

function hintDeclarations(): ReadonlyMap<number, HintDeclarationCompiler> {
  const result = new Map<number, HintDeclarationCompiler>();
  const add = (seq: number, compile: HintDeclarationCompiler): void => {
    if (result.has(seq)) throw new Error(`Duplicate hint declaration ${seq}`);
    result.set(seq, compile);
  };
  const addSimple = (seq: number, compile: HintCompiler): void => {
    add(seq, candidate => compile(candidate.reading));
  };
  for (const [seq, compile] of LEGACY_SIMPLE_HINT_DECLARATIONS) addSimple(seq, compile);
  for (const [seq, split] of LEGACY_EASY_HINT_DECLARATIONS) {
    add(seq, (candidate, readings) => candidate.route === 'kanji'
      ? compileEasyHint(readings, split, candidate.surface, candidate.reading)
      : null);
  }
  for (const [seq, compile] of UPSTREAM_260118_SIMPLE_HINT_DECLARATIONS) {
    addSimple(seq, compile);
  }
  for (const [seq, split] of UPSTREAM_260118_EASY_HINT_DECLARATIONS) {
    add(seq, (candidate, readings) => candidate.route === 'kanji'
      ? compileEasyHint(readings, split, candidate.surface, candidate.reading)
      : null);
  }
  return result;
}

function evaluateSplit(
  declaration: SplitDeclaration,
  candidate: AnnotationCandidate,
  resolver: SplitPartResolver
): readonly [parts: readonly AnalyzerSupportSplitPartSource[], attributes: SplitAttributes] | null {
  const text = candidate.surface;
  let offset = 0;
  let attributes = declaration.score;
  const parts: Array<AnalyzerSupportSplitPartSource | null> = [];

  for (const definition of declaration.parts) {
    if (definition.type === 'guard') {
      if (!definition.condition(text.length, text, candidate)) return null;
      continue;
    }
    if (definition.type === 'test') {
      if (!definition.condition(text.length, text, candidate)) {
        if (definition.newScore !== undefined) {
          attributes = typeof attributes === 'number'
            ? definition.newScore
            : { ...attributes, score: definition.newScore };
        }
        if (definition.pushOnFail) parts.push(definition.pushOnFail);
        break;
      }
      continue;
    }
    if (definition.type === 'marker') {
      parts.push(definition.marker);
      continue;
    }

    let seqs: readonly number[];
    if (typeof definition.seqs === 'number') {
      seqs = [definition.seqs];
    } else if (typeof definition.seqs[0] === 'string') {
      const [seedText, ...seedSeqs] = definition.seqs as readonly [string, ...number[]];
      const seed = resolver.find(seedText, seedSeqs, true);
      seqs = seed && typeof seed !== 'string' ? [seed.seq] : [];
    } else {
      seqs = definition.seqs as readonly number[];
    }

    const length = definition.lengthFn(text.length, text, candidate);
    const partText = length === null
      ? safeSlice(text, offset)
      : safeSlice(text, offset, offset + length);
    if (seqs.includes(declaration.seq)) {
      parts.push(null);
    } else if (partText) {
      const lookupText = definition.modify === true
        ? unrendaku(partText)
        : typeof definition.modify === 'function'
          ? definition.modify(partText)
          : partText;
      parts.push(resolver.find(lookupText, seqs, definition.conjP ?? false));
    } else {
      parts.push(null);
    }
    if (length !== null) offset += length;
  }
  return parts.some(part => part === null)
    ? null
    : [parts as AnalyzerSupportSplitPartSource[], attributes];
}

/** Compile every qualified split and hint fact from semantic source input. */
export function compileAnalyzerSupportAnnotations(
  input: AnalyzerAnnotationInput
): AnalyzerAnnotationCompilation {
  const splitDeclarations = declarationsBySeq(SPLIT_DECLARATIONS, 'split declaration');
  const segmentDeclarations = declarationsBySeq(
    SEGMENT_SPLIT_DECLARATIONS,
    'segment split declaration'
  );
  const hints = hintDeclarations();
  const declarationSeqs = new Set([
    ...splitDeclarations.keys(),
    ...segmentDeclarations.keys(),
    ...hints.keys()
  ]);
  const selectedRoots = new Set(declarationSeqs);
  for (const collision of input.collisions) {
    if (declarationSeqs.has(collision.collisionSeq)) selectedRoots.add(collision.rootSeq);
  }

  const candidates = new Map<string, AnnotationCandidate>();
  for (const candidate of directCandidates(input.entries, selectedRoots)) {
    candidates.set(candidateKey(candidate), candidate);
  }
  for (const candidate of enumerateAnnotationCandidates(input.morphology, selectedRoots)) {
    candidates.set(candidateKey(candidate), candidate);
  }

  const collisions = new Map<string, number>();
  for (const collision of input.collisions) {
    collisions.set(collisionKey(collision), collision.collisionSeq);
  }
  const splitOutput = new Map<string, AnalyzerSupportSplitSource>();
  const hintOutput = new Map<string, AnalyzerSupportHintSource>();

  for (const candidate of candidates.values()) {
    const collisionSeq = candidate.ruleIds === null ? null : collisions.get(collisionKey({
      rootSeq: candidate.rootSeq,
      ruleIds: candidate.ruleIds,
      route: candidate.route,
      surface: candidate.surface
    })) ?? null;
    for (const [kind, declarations] of [
      ['split', splitDeclarations],
      ['segsplit', segmentDeclarations]
    ] as const satisfies readonly (readonly [AnalyzerSupportSplitKind, ReadonlyMap<number, SplitDeclaration>])[]) {
      const definitionSeq = collisionSeq !== null && declarations.has(collisionSeq)
        ? collisionSeq
        : declarations.has(candidate.rootSeq) ? candidate.rootSeq : null;
      if (definitionSeq === null) continue;
      const evaluated = evaluateSplit(
        declarations.get(definitionSeq)!,
        candidate,
        input.partResolver
      );
      if (!evaluated) continue;
      const [parts, attributes] = evaluated;
      const value: AnalyzerSupportSplitSource = {
        definitionSeq,
        route: candidate.route,
        surface: candidate.surface,
        kind,
        parts,
        score: typeof attributes === 'number' ? attributes : attributes.score,
        primary: typeof attributes === 'number' ? 0 : attributes.primary ?? 0,
        connector: typeof attributes === 'number' ? ' ' : attributes.connector ?? ' ',
        root: typeof attributes === 'number' ? [] : attributes.root ?? []
      };
      const key = splitKey(value);
      const prior = splitOutput.get(key);
      if (prior && JSON.stringify(prior) !== JSON.stringify(value)) {
        throw new Error(`Split output depends on unkeyed state for ${key}`);
      }
      splitOutput.set(key, value);
    }

    const definitionSeq = collisionSeq !== null && hints.has(collisionSeq)
      ? collisionSeq
      : hints.has(candidate.rootSeq) ? candidate.rootSeq : null;
    if (definitionSeq === null) continue;
    const hint = hints.get(definitionSeq)!(candidate, input.kanjidicReadings);
    if (hint === null) continue;
    const value: AnalyzerSupportHintSource = {
      definitionSeq,
      route: candidate.route,
      surface: candidate.surface,
      reading: candidate.reading,
      hint
    };
    const key = hintKey(value);
    const prior = hintOutput.get(key);
    if (prior && prior.hint !== hint) {
      throw new Error(`Hint output depends on unkeyed state for ${key}`);
    }
    hintOutput.set(key, value);
  }

  return {
    splits: [...splitOutput.values()].sort((left, right) =>
      compareText(splitKey(left), splitKey(right))),
    hints: [...hintOutput.values()].sort((left, right) =>
      compareText(hintKey(left), hintKey(right)))
  };
}
