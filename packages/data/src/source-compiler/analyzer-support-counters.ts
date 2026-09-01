import { testWord } from '@ichiran/core';
import type { AnalyzerSupportCounterSource, AnalyzerSupportRoute } from '../browser-pack/analyzer-support.js';
import { COUNTER_SPECIALS, type CounterSpecialDeclaration } from './analyzer-support-counter-specials.js';
import type { CanonicalEntry, CanonicalForm } from './model.js';

const EXTRA_COUNTERS = new Set([1_255_430, 1_606_800]);
const SKIP_COUNTERS = new Set([
  2_426_510, 2_220_370, 2_248_360, 2_423_450, 2_671_670, 2_735_690,
  2_838_543, 2_249_290, 2_833_260, 2_833_465, 2_833_466, 2_833_467
]);
const FOREIGN_COUNTERS = new Set([1_120_410]);
const ACCEPTED_SUFFIXES = new Map<number, readonly string[]>([
  [1_194_480, ['kan']],
  [1_490_430, ['kan']],
  [1_333_450, ['kan', 'kango']]
]);
const COUNTER_SUFFIXES = new Map([
  ['kan', { text: '間', kana: 'かん', description: '[duration]' }],
  ['kango', { text: '間後', kana: 'かんご', description: '[after ...]' }],
  ['chuu', { text: '中', kana: 'ちゅう', description: '[among/out of ...]' }]
]);
interface CounterForm {
  readonly seq: number;
  readonly route: AnalyzerSupportRoute;
  readonly form: CanonicalForm;
}

interface PendingCounter extends Omit<AnalyzerSupportCounterSource, 'key' | 'order'> {
  readonly accepts: readonly string[];
}

function hasCounterPosition(entry: CanonicalEntry): boolean {
  return entry.senses.some(sense =>
    sense.properties.some(property => property.tag === 'pos' && property.text === 'ctr'));
}

function counterRestrictions(entry: CanonicalEntry): {
  readonly kanji: ReadonlySet<string> | null;
  readonly kana: ReadonlySet<string> | null;
} {
  const kanji: string[] = [];
  const kana: string[] = [];
  for (const sense of entry.senses) {
    if (!sense.properties.some(property => property.tag === 'pos' && property.text === 'ctr')) continue;
    for (const property of sense.properties) {
      if (property.tag === 'stagk') kanji.push(property.text);
      if (property.tag === 'stagr') kana.push(property.text);
    }
  }
  return {
    kanji: kanji.length === 0 ? null : new Set(kanji),
    kana: kana.length === 0 ? null : new Set(kana)
  };
}

function counterForms(entry: CanonicalEntry): { readonly kanji: CounterForm[]; readonly kana: CounterForm[] } {
  const restrictions = counterRestrictions(entry);
  const order = (left: CanonicalForm, right: CanonicalForm): number =>
    left.ordinal - right.ordinal
    || left.sourceOrder.event - right.sourceOrder.event
    || left.sourceOrder.ordinal - right.sourceOrder.ordinal
    || (left.text < right.text ? -1 : left.text > right.text ? 1 : 0);
  return {
    kanji: entry.kanji.filter(form => restrictions.kanji?.has(form.text) ?? true)
      .sort(order).map(form => ({ seq: entry.seq, route: 'kanji', form })),
    kana: entry.kana.filter(form => restrictions.kana?.has(form.text) ?? true)
      .sort(order).map(form => ({ seq: entry.seq, route: 'kana', form }))
  };
}

function source(form: CounterForm | null): AnalyzerSupportCounterSource['source'] {
  return form === null ? null : {
    seq: form.seq,
    route: form.route,
    text: form.form.text,
    ord: form.form.ordinal
  };
}

function declaredCounter(
  declaration: CounterSpecialDeclaration,
  text: string,
  forms: readonly CounterForm[]
): PendingCounter {
  const selected = declaration.sourceText === null
    ? null
    : forms.find(value => value.form.text === declaration.sourceText) ?? null;
  if (declaration.sourceText !== null && selected === null) {
    throw new Error(`Missing special counter source ${declaration.seq}/${declaration.sourceText}`);
  }
  return {
    className: declaration.className,
    text,
    kana: declaration.kana,
    suffix: declaration.suffix,
    source: source(selected),
    ordinal: declaration.ordinal,
    foreign: declaration.foreign,
    common: declaration.common,
    suffixDescriptions: declaration.suffixDescriptions,
    digitOptions: declaration.digitOptions,
    digitSet: declaration.digitSet,
    allowed: declaration.allowed,
    accepts: declaration.accepts
  };
}

/** Builds the complete counter cache from canonical POS, forms and restrictions. */
export function compileCanonicalCounters(entries: readonly CanonicalEntry[]): AnalyzerSupportCounterSource[] {
  const specialBySeq = new Map<number, CounterSpecialDeclaration[]>();
  for (const declaration of COUNTER_SPECIALS) {
    const values = specialBySeq.get(declaration.seq) ?? [];
    values.push(declaration);
    specialBySeq.set(declaration.seq, values);
  }
  const cache = new Map<string, PendingCounter[]>();

  const add = (key: string, value: PendingCounter): void => {
    const values = cache.get(key) ?? [];
    values.push(value);
    cache.set(key, values);
    for (const accepted of value.accepts) {
      const suffix = COUNTER_SUFFIXES.get(accepted);
      if (!suffix) throw new Error(`Unknown accepted counter suffix ${accepted}`);
      const suffixedKey = key + suffix.text;
      const suffixed = cache.get(suffixedKey) ?? [];
      suffixed.push({
        ...value,
        text: suffixedKey,
        suffix: (value.suffix ?? '') + suffix.kana,
        suffixDescriptions: [...value.suffixDescriptions, suffix.description]
      });
      cache.set(suffixedKey, suffixed);
    }
  };

  add('', {
    className: 'NumberText', text: '', kana: '', suffix: null, source: null,
    ordinal: false, foreign: false, common: null, suffixDescriptions: [],
    digitOptions: [], digitSet: [], allowed: [], accepts: []
  });

  const selected = entries.filter(entry =>
    !SKIP_COUNTERS.has(entry.seq) && (hasCounterPosition(entry) || EXTRA_COUNTERS.has(entry.seq)))
    .sort((left, right) => left.seq - right.seq);
  for (const entry of selected) {
    const forms = counterForms(entry);
    const allForms = [...forms.kanji, ...forms.kana];
    const declarations = specialBySeq.get(entry.seq);
    if (declarations) {
      for (const declaration of declarations) {
        for (const text of declaration.texts) add(text, declaredCounter(declaration, text, allForms));
      }
      continue;
    }

    const foreign = forms.kanji.length === 0 || FOREIGN_COUNTERS.has(entry.seq);
    const sourceForms = foreign
      ? [...forms.kanji, ...forms.kana.filter(value => testWord(value.form.text, 'katakana'))]
      : forms.kanji;
    for (const value of sourceForms) {
      const text = value.form.text;
      add(text, {
        className: 'CounterText',
        text,
        kana: forms.kana[0]?.form.text ?? '',
        suffix: null,
        source: source(value),
        ordinal: text.length > 1 && text.endsWith('目'),
        foreign,
        common: null,
        suffixDescriptions: [],
        digitOptions: [],
        digitSet: [],
        allowed: [],
        accepts: ACCEPTED_SUFFIXES.get(entry.seq) ?? []
      });
    }
  }

  for (const [key, values] of cache) {
    if (key === '' || (key.length > 1 && key.endsWith('目'))) continue;
    const ordinalKey = key + '目';
    if (cache.has(ordinalKey)) continue;
    for (const value of values) {
      if (value.ordinal) continue;
      add(ordinalKey, {
        ...value,
        text: ordinalKey,
        suffix: (value.suffix ?? '') + 'め',
        ordinal: true
      });
    }
  }

  return [...cache].flatMap(([key, values]) => values.map((value, order) => {
    const { accepts: _accepts, ...counter } = value;
    return { key, order, ...counter };
  }));
}
