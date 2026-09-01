import type {
  AnalyzerSupportSuffixFormSource,
  AnalyzerSupportSuffixSource
} from '../browser-pack/analyzer-support.js';
import type { CanonicalEntry, CanonicalForm, ConjugationProperty } from './model.js';

type SuffixForm = AnalyzerSupportSuffixFormSource;
type SuffixValue = { readonly keyword: string; readonly form: SuffixForm | null };

const GENERATED_SUFFIX_ROOT = {
  chau: 2_013_800,
  chauContracted: 2_210_750,
  tai: 2_017_560,
  nikui: 2_772_730,
  oru: 1_577_985,
  aru: 1_296_400,
  iru: 1_577_980,
  kuru: 1_547_720,
  oku: 1_421_850,
  okuTo: 2_108_590,
  chauTe: 1_305_380,
  kureru: 1_269_130,
  morau: 1_535_910,
  itadaku: 1_587_290,
  iku: 1_578_850,
  suru: 1_157_170,
  itasu: 1_421_900,
  sareru: 2_269_820,
  saseru: 1_005_160,
  sou: 1_006_610,
  souPlus: 2_141_080,
  sugiru: 1_195_970,
  tsutsuAru: 2_027_910,
  naku: 1_529_520,
  naru: 1_375_610,
  yagaru: 1_012_740,
  rashii: 1_013_240,
  toSuru: 2_136_890,
  garu: 1_631_750,
  gatai: 2_867_504
} as const;

/** Roots whose declared suffix cache includes generated conjugation forms. */
export const GENERATED_SUFFIX_ROOTS: ReadonlySet<number> = new Set(
  Object.values(GENERATED_SUFFIX_ROOT)
);

const SKIP_PROPERTIES = [
  { type: 10, negative: true },
  { type: 3, negative: true, formal: true },
  { pos: 'vs-s', type: 5 }
] as const;

function matches(
  property: ConjugationProperty,
  pattern: { readonly pos?: string; readonly type: number; readonly negative?: boolean; readonly formal?: boolean }
): boolean {
  return property.type === pattern.type
    && (pattern.pos === undefined || property.pos === pattern.pos)
    && (pattern.negative === undefined || property.negative === pattern.negative)
    && (pattern.formal === undefined || property.formal === pattern.formal);
}

/** Qualified suffixes omit these low-confidence morphology properties. */
export function isWeakSuffixProperty(property: ConjugationProperty): boolean {
  return [51, 52, 53, 54].includes(property.type)
    || (property.type === 9 && property.negative === true);
}

/** Qualified suffixes omit targets whose properties all match a skipped form. */
export function isSkippedSuffixProperty(property: ConjugationProperty): boolean {
  return SKIP_PROPERTIES.some(pattern => matches(property, pattern));
}

function commonTags(form: CanonicalForm): string {
  return form.priorityTags.map(tag => `[${tag}]`).join('');
}

function directSuffixForm(entry: CanonicalEntry, form: CanonicalForm): SuffixForm {
  return {
    seq: entry.seq,
    text: form.text,
    bestKanji: form.best,
    commonTags: commonTags(form),
    ord: form.ordinal,
    common: form.common,
    conjugatable: form.conjugatable,
    nokanji: form.noKanji,
    conjugations: ':root'
  };
}

/** Replay suffix declarations from the narrow generated forms needed by them. */
export function compileCanonicalSuffixesFromGenerated(
  entries: readonly CanonicalEntry[],
  generated: ReadonlyMap<number, readonly AnalyzerSupportSuffixFormSource[]>
): {
  readonly suffixes: readonly AnalyzerSupportSuffixSource[];
  readonly suffixClasses: readonly { readonly seq: number; readonly keyword: string }[];
} {
  const entryBySeq = new Map(entries.map(entry => [entry.seq, entry]));
  const cache = new Map<string, SuffixValue[]>();
  const classes = new Map<number, string>();

  const put = (text: string, value: SuffixValue, join = false): void => {
    const prior = cache.get(text);
    cache.set(text, join && prior ? [value, ...prior] : [value]);
  };
  const loadForm = (
    keyword: string,
    seq: number,
    text: string,
    options: {
      readonly className?: string;
      readonly cacheText?: string;
      readonly join?: boolean;
      readonly conjugations?: ':root' | null;
    } = {}
  ): void => {
    const entry = entryBySeq.get(seq);
    const canonical = entry?.kana.find(form => form.text === text);
    if (!entry || !canonical) throw new Error(`Missing declared suffix form ${seq}/${text}`);
    const form = {
      ...directSuffixForm(entry, canonical),
      conjugations: options.conjugations ?? null
    };
    put(options.cacheText ?? text, { keyword, form }, options.join);
    classes.set(form.seq, options.className ?? keyword);
  };
  const loadConjugations = (
    keyword: string,
    seq: number,
    className = keyword,
    join = false
  ): void => {
    const entry = entryBySeq.get(seq);
    if (!entry) throw new Error(`Missing declared suffix root ${seq}`);
    const forms = [
      ...entry.kana.map(form => directSuffixForm(entry, form)),
      ...(generated.get(seq) ?? [])
    ];
    for (const form of forms) {
      put(form.text, { keyword, form }, join);
      classes.set(form.seq, className);
    }
  };
  const abbreviation = (keyword: string, text: string, join = false): void =>
    put(text, { keyword, form: null }, join);
  const declaredForms = (seq: number): SuffixForm[] => {
    const entry = entryBySeq.get(seq);
    if (!entry) throw new Error(`Missing declared suffix root ${seq}`);
    return [
      ...entry.kana.map(form => directSuffixForm(entry, form)),
      ...(generated.get(seq) ?? [])
    ];
  };

  loadConjugations(':chau', GENERATED_SUFFIX_ROOT.chau);
  loadConjugations(':chau', GENERATED_SUFFIX_ROOT.chauContracted);
  loadForm(':chau', 2_028_920, 'は', { className: ':ha', cacheText: 'ちゃ' });
  loadForm(':chau', 2_028_920, 'は', { className: ':ha', cacheText: 'じゃ' });
  loadConjugations(':tai', GENERATED_SUFFIX_ROOT.tai);
  loadForm(':tai', 900_000, 'たそう', { className: ':tasou' });
  loadConjugations(':ren-', GENERATED_SUFFIX_ROOT.nikui, ':nikui');
  loadConjugations(':te', GENERATED_SUFFIX_ROOT.oru, ':oru');
  loadConjugations(':te', GENERATED_SUFFIX_ROOT.aru, ':aru');

  for (const form of declaredForms(GENERATED_SUFFIX_ROOT.iru)) {
    put(form.text, { keyword: form.text.length > 1 ? ':teiru+' : ':teiru', form });
    classes.set(form.seq, ':iru');
    if (form.text.length > 1) put(form.text.slice(1), { keyword: ':teiru', form });
  }

  loadConjugations(':te', GENERATED_SUFFIX_ROOT.kuru, ':kuru');
  loadConjugations(':te', GENERATED_SUFFIX_ROOT.oku, ':oku');
  loadConjugations(':to', GENERATED_SUFFIX_ROOT.okuTo, ':oku');
  loadConjugations(':te', GENERATED_SUFFIX_ROOT.chauTe, ':chau');
  loadConjugations(':te+space', GENERATED_SUFFIX_ROOT.kureru, ':kureru');
  loadConjugations(':te+space', GENERATED_SUFFIX_ROOT.morau, ':morau');
  loadConjugations(':te+space', GENERATED_SUFFIX_ROOT.itadaku, ':itadaku');

  for (const form of declaredForms(GENERATED_SUFFIX_ROOT.iku)) {
    if (!form.text.startsWith('い')) continue;
    put(form.text, { keyword: ':te', form });
    classes.set(form.seq, ':iku');
    if (!cache.has(form.text.slice(1))) put(form.text.slice(1), { keyword: ':te', form });
  }

  loadForm(':teii', 2_820_690, 'いい', { className: ':ii' });
  loadForm(':teii', 900_001, 'もいい', { className: ':ii' });
  loadForm(':te', 2_028_940, 'も', { className: ':mo' });
  loadForm(':kudasai', 1_184_270, 'ください', { conjugations: ':root' });
  loadConjugations(':suru', GENERATED_SUFFIX_ROOT.suru);
  loadConjugations(':suru', GENERATED_SUFFIX_ROOT.itasu, ':itasu');
  loadConjugations(':suru', GENERATED_SUFFIX_ROOT.sareru, ':sareru');
  loadConjugations(':suru', GENERATED_SUFFIX_ROOT.saseru, ':saseru');
  loadConjugations(':sou', GENERATED_SUFFIX_ROOT.sou);
  loadConjugations(':sou+', GENERATED_SUFFIX_ROOT.souPlus);
  loadForm(':rou', 1_928_670, 'だろう', { cacheText: 'ろう' });
  loadConjugations(':sugiru', GENERATED_SUFFIX_ROOT.sugiru);
  loadForm(':sa', 2_029_120, 'さ');
  loadForm(':ren', 1_008_120, 'つつ', { className: ':tsutsu' });
  loadConjugations(':ren', GENERATED_SUFFIX_ROOT.tsutsuAru, ':tsutsuaru');
  loadForm(':ren', 1_454_500, 'うる', { className: ':uru' });

  const naku = declaredForms(GENERATED_SUFFIX_ROOT.naku).find(form => form.text === 'なく');
  if (naku) {
    put('なく', { keyword: ':neg', form: naku });
    classes.set(naku.seq, ':nai');
  }

  loadConjugations(':adv', GENERATED_SUFFIX_ROOT.naru, ':naru');
  loadConjugations(':teren', GENERATED_SUFFIX_ROOT.yagaru, ':yagaru');
  loadForm(':ra', 2_067_770, 'ら');
  loadConjugations(':rashii', GENERATED_SUFFIX_ROOT.rashii);
  loadForm(':desu', 1_628_500, 'です');
  loadForm(':desho', 1_008_420, 'でしょう');
  loadForm(':desho', 1_008_420, 'でしょ');
  loadConjugations(':tosuru', GENERATED_SUFFIX_ROOT.toSuru);
  loadForm(':kurai', 1_154_340, 'くらい');
  loadForm(':kurai', 1_154_340, 'ぐらい');
  loadConjugations(':garu', GENERATED_SUFFIX_ROOT.garu);
  loadForm(':ren', 2_016_470, 'がち', { className: ':gachi' });
  loadForm(':iadj', 2_006_580, 'げ');
  loadForm(':iadj', 1_604_890, 'め', { className: ':me' });
  loadForm(':ren-', 2_606_690, 'がい', { className: ':gai' });

  for (const [keyword, text, join] of [
    [':nai', 'ねえ'], [':nai', 'ねぇ'], [':nai', 'ねー'],
    [':nai-x', 'ず'], [':nai-x', 'ざる'], [':nai-x', 'ぬ'], [':nai-n', 'ん'],
    [':nakereba', 'なきゃ'], [':nakereba', 'なくちゃ'], [':teba', 'ちゃ', true],
    [':reba', 'りゃ'], [':keba', 'きゃ'], [':geba', 'ぎゃ'], [':neba', 'にゃ'],
    [':beba', 'びゃ'], [':meba', 'みゃ'], [':seba', 'しゃ'],
    [':shimashou', 'しましょ'], [':dewanai', 'じゃない'], [':ii', 'ええ'],
    [':nakereba', 'ねば']
  ] as const) abbreviation(keyword, text, join ?? false);

  loadConjugations(':ren-', GENERATED_SUFFIX_ROOT.gatai, ':gatai');

  return {
    suffixes: [...cache].map(([text, values]) => ({ text, values })),
    suffixClasses: [...classes].map(([seq, keyword]) => ({ seq, keyword }))
  };
}
