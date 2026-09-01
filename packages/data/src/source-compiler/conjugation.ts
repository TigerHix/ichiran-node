import { conjugateWord } from '../data/conj-rules.js';
import {
  entryPartOfSpeech,
  type CanonicalEntry,
  type CanonicalRoute,
  type ConjugationCandidate,
  type ConjugationLink,
  type ConjugationProperty,
  type GeneratedForm,
  type GeneratedTarget
} from './model.js';

const DO_NOT_CONJUGATE = new Set(['n', 'vs', 'adj-na']);
const SECONDARY_SOURCE_TYPES = new Set([5, 6, 7, 8, 53]);

function candidateKey(property: Omit<ConjugationProperty, 'negative' | 'formal'>): string {
  return `${property.pos}\u0000${property.type}`;
}

function uniqueForms(forms: readonly GeneratedForm[]): GeneratedForm[] {
  const result: GeneratedForm[] = [];
  const seen = new Set<string>();
  for (const form of forms) {
    const key = `${form.route}\u0000${form.text}\u0000${form.sourceText}`;
    if (seen.has(key)) continue;
    seen.add(key);
    result.push(form);
  }
  return result;
}

export function conjugateCanonicalEntry(
  entry: CanonicalEntry,
  options: {
    readonly positions?: readonly string[];
    readonly types?: ReadonlySet<number>;
  } = {}
): ConjugationCandidate[] {
  const positions = options.positions ?? entryPartOfSpeech(entry);
  const original = new Set([...entry.kanji, ...entry.kana].map(form => form.text.normalize('NFKC').trim()));
  const grouped = new Map<string, {
    property: Omit<ConjugationProperty, 'negative' | 'formal'>;
    variants: Map<string, GeneratedForm[]>;
    hasNegative: boolean;
    hasFormal: boolean;
  }>();

  for (const pos of positions) {
    if (DO_NOT_CONJUGATE.has(pos)) continue;
    const forms: Array<readonly [CanonicalRoute, CanonicalEntry['kanji'][number]]> = [
      ...entry.kanji.filter(form => form.conjugatable).map(form => ['kanji', form] as const),
      ...entry.kana.filter(form => form.conjugatable).map(form => ['kana', form] as const)
    ];

    for (const [route, source] of forms) {
      conjugateWord(source.text, pos).forEach(([rule, text], ruleOrdinal) => {
        if (options.types && !options.types.has(rule.conj)) return;
        if (original.has(text.normalize('NFKC').trim())) return;

        const base = { pos, type: rule.conj };
        const key = candidateKey(base);
        const group = grouped.get(key) ?? {
          property: base,
          variants: new Map<string, GeneratedForm[]>(),
          hasNegative: false,
          hasFormal: false
        };
        group.hasNegative ||= rule.neg;
        group.hasFormal ||= rule.fml;
        const variantKey = `${rule.neg ? 1 : 0}${rule.fml ? 1 : 0}`;
        const variant = group.variants.get(variantKey) ?? [];
        variant.push({
          route,
          text,
          sourceText: source.text,
          sourceOrdinal: source.ordinal,
          ruleOrdinal
        });
        group.variants.set(variantKey, variant);
        grouped.set(key, group);
      });
    }
  }

  const candidates: ConjugationCandidate[] = [];
  for (const group of grouped.values()) {
    for (const [variant, forms] of group.variants) {
      const negative = variant[0] === '1';
      const formal = variant[1] === '1';
      if (group.property.type === 52 && group.property.pos === 'v5r-i') continue;
      candidates.push({
        property: {
          ...group.property,
          negative: group.hasNegative ? negative : null,
          formal: group.hasFormal ? formal : null
        },
        forms: uniqueForms(forms).sort((left, right) =>
          left.sourceOrdinal - right.sourceOrdinal || left.ruleOrdinal - right.ruleOrdinal),
        targetConjugatable: SECONDARY_SOURCE_TYPES.has(group.property.type)
      });
    }
  }
  return candidates;
}

function hasEvery(haystack: readonly string[], needles: readonly string[]): boolean {
  const values = new Set(haystack);
  return needles.every(value => values.has(value));
}

export function findReusableTarget(
  targets: readonly GeneratedTarget[],
  forms: readonly GeneratedForm[],
  from: number,
  via: number | null
): GeneratedTarget | null {
  const kanji = [...new Set(forms.filter(form => form.route === 'kanji').map(form => form.text))];
  const kana = [...new Set(forms.filter(form => form.route === 'kana').map(form => form.text))];
  return [...targets]
    .sort((left, right) => left.seq - right.seq)
    .find(target =>
      target.seq !== from && target.seq !== via &&
      (kanji.length > 0
        ? hasEvery(target.kanji, kanji) && hasEvery(target.kana, kana)
        : target.kanji.length === 0 && hasEvery(target.kana, kana))) ?? null;
}

export class SourceConjugations {
  readonly targets: GeneratedTarget[];
  readonly links: ConjugationLink[] = [];
  #nextSeq: number;

  constructor(existingTargets: readonly GeneratedTarget[], firstGeneratedSeq: number) {
    this.targets = [...existingTargets];
    this.#nextSeq = firstGeneratedSeq;
  }

  add(from: number, via: number | null, candidate: ConjugationCandidate): ConjugationLink {
    const reused = findReusableTarget(this.targets, candidate.forms, from, via);
    let target = reused;
    if (!target) {
      target = {
        seq: this.#nextSeq++,
        kanji: [...new Set(candidate.forms.filter(form => form.route === 'kanji').map(form => form.text))],
        kana: [...new Set(candidate.forms.filter(form => form.route === 'kana').map(form => form.text))],
        conjugatable: candidate.targetConjugatable,
        allocationOrdinal: this.targets.length
      };
      this.targets.push(target);
    }

    const link: ConjugationLink = {
      ordinal: this.links.length,
      from,
      target: target.seq,
      via,
      property: candidate.property,
      forms: candidate.forms
    };
    this.links.push(link);
    return link;
  }
}
