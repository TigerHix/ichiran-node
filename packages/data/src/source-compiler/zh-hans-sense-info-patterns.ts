/**
 * Versioned, deterministic grammar for mechanically translating JMdict sense
 * notes. This is deliberately separate from the hand-authored exact catalog:
 * rules only rearrange a validated Japanese expression inside fixed Chinese
 * text or match a finite reviewed phrase table. They never invoke a runtime
 * or external translation service.
 */
export const ZH_HANS_SENSE_INFO_PATTERN_POLICY = 'jmdict-s-inf-zh-Hans-patterns-v2';

export const ZH_HANS_SENSE_INFO_PATTERN_RULES = [
  { id: 'read', sourceGrammar: 'read [Japanese expression]' },
  { id: 'also-read', sourceGrammar: 'also read [Japanese expression]' },
  { id: 'usual-read', sourceGrammar: 'usu./usually read [Japanese expression]' },
  { id: 'frequent-read', sourceGrammar: 'oft./often read [Japanese expression]' },
  { id: 'occasional-read', sourceGrammar: 'occ./occasionally/sometimes read [Japanese expression]' },
  { id: 'formerly-read', sourceGrammar: 'also formerly read as [Japanese expression]' },
  { id: 'suffix-read', sourceGrammar: '[frequency] read [Japanese expression] as a suffix' },
  { id: 'prefix-read', sourceGrammar: '[frequency] read [Japanese expression] as a prefix' },
  { id: 'modern-read', sourceGrammar: '[frequency] read [Japanese expression] in modern Japanese' },
  { id: 'pronounced', sourceGrammar: 'pronounced [Japanese expression]' },
  { id: 'also-pronounced', sourceGrammar: 'also pronounced [Japanese expression]' },
  { id: 'usual-pronounced', sourceGrammar: 'usu./usually pronounced [Japanese expression]' },
  { id: 'frequent-pronounced', sourceGrammar: 'oft./often pronounced [Japanese expression]' },
  { id: 'occasional-pronounced', sourceGrammar: 'occ./occasionally/sometimes pronounced [Japanese expression]' },
  { id: 'suffix-pronounced', sourceGrammar: '[frequency] pronounced [Japanese expression] as a suffix' },
  { id: 'prefix-pronounced', sourceGrammar: '[frequency] pronounced [Japanese expression] as a prefix' },
  { id: 'modern-pronounced', sourceGrammar: '[frequency] pronounced [Japanese expression] in modern Japanese' },
  { id: 'subject-reading', sourceGrammar: '[Japanese expression] is [frequency] read/pronounced [Japanese expression]' },
  { id: 'also-written', sourceGrammar: 'also written (as) [Japanese expression]' },
  { id: 'usual-written', sourceGrammar: 'usu./usually written (as) [Japanese expression]' },
  { id: 'frequent-written', sourceGrammar: 'oft./often written (as) [Japanese expression]' },
  { id: 'occasional-written', sourceGrammar: 'occ./occasionally written (as) [Japanese expression]' },
  { id: 'sometimes-written', sourceGrammar: 'sometimes written (as) [Japanese expression]' },
  { id: 'especially-as', sourceGrammar: 'esp./especially as [Japanese expression]' },
  { id: 'usually-as', sourceGrammar: 'usu./usually as [Japanese expression]' },
  { id: 'often-as', sourceGrammar: 'oft./often as [Japanese expression]' },
  { id: 'adverbially-as', sourceGrammar: 'adverbially as [Japanese expression]' },
  { id: 'usual-adverbially-as', sourceGrammar: 'usu./usually adverbially as [Japanese expression]' },
  { id: 'frequent-adverbially-as', sourceGrammar: 'oft./often adverbially as [Japanese expression]' },
  { id: 'as-expression', sourceGrammar: 'as [Japanese expression]' },
  { id: 'especially-expression', sourceGrammar: 'esp./especially [Japanese expression]' },
  { id: 'usually-expression', sourceGrammar: 'usu./usually [Japanese expression]' },
  { id: 'often-expression', sourceGrammar: 'oft./often [Japanese expression]' },
  { id: 'occasional-expression', sourceGrammar: 'occ./occasionally [Japanese expression]' },
  { id: 'after-expression', sourceGrammar: 'after [Japanese expression]' },
  { id: 'before-expression', sourceGrammar: 'before [Japanese expression]' },
  { id: 'with-expression', sourceGrammar: 'with [Japanese expression]' },
  { id: 'abbreviation-of', sourceGrammar: 'abbr./abbreviation of [Japanese expression]' },
  { id: 'short-for', sourceGrammar: 'short for [Japanese expression]' },
  { id: 'contraction-of', sourceGrammar: 'contraction of [Japanese expression]' },
  { id: 'variant-of', sourceGrammar: 'variant (form) of [Japanese expression]' },
  { id: 'nonstandard-variant-of', sourceGrammar: 'non-standard/unorthodox variant of [Japanese expression]' },
  { id: 'incorrect-variant-of', sourceGrammar: 'incorrect variant of [Japanese expression]' },
  { id: 'emphatic-form-of', sourceGrammar: 'emphatic form/version of [Japanese expression]' },
  { id: 'stronger-version-of', sourceGrammar: 'stronger version of [Japanese expression]' },
  { id: 'more-emphatic-than', sourceGrammar: 'more emphatic than [Japanese expression]' },
  { id: 'inflection-form-of', sourceGrammar: '[reviewed grammatical form] of [Japanese expression]' },
  { id: 'example-expression', sourceGrammar: 'e.g. [Japanese expression]' },
  { id: 'equivalent-expression', sourceGrammar: 'equiv./equivalent of/to [Japanese expression]' },
  { id: 'grammar-attachment', sourceGrammar: '[frequency] after/before/following [reviewed grammatical term]' },
  { id: 'negative-context', sourceGrammar: '[exact reviewed negative-context phrase]' },
  { id: 'place-dialect', sourceGrammar: '[reviewed Japanese place name] dialect' }
] as const;

export type ZhHansSenseInfoPatternRuleId =
  typeof ZH_HANS_SENSE_INFO_PATTERN_RULES[number]['id'];

export interface ZhHansSenseInfoPatternTranslation {
  readonly policy: typeof ZH_HANS_SENSE_INFO_PATTERN_POLICY;
  readonly rule: ZhHansSenseInfoPatternRuleId;
  readonly target: string;
}

// Lowercase Latin text would be untranslated prose. Uppercase ASCII is allowed
// solely for metavariables such as A/B/N, alongside numbers and Japanese text.
// Ordinary spaces are the only whitespace accepted, and quote marks are kept
// out because wrapping an already quoted expression would be ambiguous.
const SAFE_EXPRESSION = /^[\p{Script=Han}\p{Script=Hiragana}\p{Script=Katakana}\p{N}A-ZＡ-Ｚ０-９々〆ヶー〜～~…・、，,.／/+＋\-−―→?？()（）［］ ]+$/u;
const JAPANESE_SCRIPT = /[\p{Script=Han}\p{Script=Hiragana}\p{Script=Katakana}]/u;
const UNSAFE_EDGE_SEPARATOR = /^(?:[,，、／/+＋\-−―→])|(?:[,，、／/+＋\-−―→])$/u;

const ATTACHMENT_TERMS: Readonly<Record<string, string>> = {
  '-masu stem': '动词ます形去掉「ます」后的词干',
  'a -masu stem': '动词ます形去掉「ます」后的词干',
  '-masu stem of a verb': '动词ます形去掉「ます」后的词干',
  '-masu stem of verb': '动词ます形去掉「ます」后的词干',
  'the -masu stem of a verb': '动词ます形去掉「ます」后的词干',
  '-nai stem of verb': '动词ない形去掉「ない」后的词干',
  'a -nai stem': '动词ない形去掉「ない」后的词干',
  'the -nai stem of a verb': '动词ない形去掉「ない」后的词干',
  '-te form of verb': '动词て形',
  'the -te form of a verb': '动词て形',
  'the ～て form of a verb': '动词て形',
  'the -ta form of a verb': '动词た形',
  'plain form of a verb or adjective': '动词或形容词的普通形',
  'the plain past form of a verb': '动词普通形的过去式',
  'volitional form of verb': '动词意志形',
  'the volitional form of verb': '动词意志形',
  'a noun': '名词',
  noun: '名词',
  'a verb': '动词',
  verb: '动词',
  'an adjective': '形容词',
  adjective: '形容词',
  'a number': '数词',
  number: '数词',
  'a quantity': '数量词',
  'an amount': '表示数量、时长或金额的词',
  'a name': '人名',
  "a person's name": '人名',
  'a place name': '地名',
  'the name of an addressee': '收信人姓名',
  'a negative': '否定表达',
  'a negative form': '否定形式',
  'a verb in negative form': '否定形式的动词',
  'a negative verb': '否定形式的动词',
  'negative verb': '否定形式的动词',
  'neg. verb': '否定形式的动词',
  'neg. stem of verb': '动词否定形词干',
  'negative stem of verb': '动词否定形词干',
  'a duration noun': '表示持续时间的名词',
  'a copula': '系词',
  'a counter': '量词',
  'a currency': '货币单位',
  'a family name': '姓氏',
  dates: '日期',
  育てる: '「育てる」'
};

const NEGATIVE_CONTEXTS: Readonly<Record<string, string>> = {
  'in a negative sentence': '用于否定句',
  'in the negative': '用于否定表达',
  'oft. in the negative': '常用于否定表达',
  'oft. in negative': '常用于否定表达',
  'oft. in negative form': '常用于否定表达',
  'often in negative': '常用于否定表达',
  'often in negative form': '常用于否定表达',
  'often in negative contexts': '常用于否定语境',
  'used in the negative': '用于否定表达',
  'used with negative verb': '与否定形式的动词连用',
  'usu. accompanied by a verb in negative form': '通常与否定形式的动词连用',
  'usu. followed by a negative verb': '通常后接否定形式的动词',
  'usu. in a negative sentence': '通常用于否定句',
  'usu. in negative': '通常用于否定表达',
  'usu. in negative form': '通常用于否定表达',
  'usu. in negative sentence': '通常用于否定句',
  'usu. in negative sentences': '通常用于否定句',
  'usu. in negative contexts': '通常用于否定语境',
  'usu. negative connotation': '通常含否定语气',
  'usu. with a negative connotation': '通常含否定语气',
  'usu. with a negative nuance': '通常含否定语气',
  'usu. with a negative verb': '通常与否定形式的动词连用',
  'usu. with negative': '通常与否定表达连用',
  'usu. with negative verb': '通常与否定形式的动词连用',
  'usu. with negative verb forms': '通常与动词否定形式连用'
};

const INFLECTION_RELATIONS: Readonly<Record<string, string>> = {
  'attributive form': '连体形',
  'continuative form': '连用形',
  'imperative form': '命令形',
  'passive form': '被动形',
  'potential form': '可能形',
  'honorific form': '敬语形式',
  'colloquial form': '口语形式',
  'literary form': '文语形式',
  'conjectural form': '推量形',
  'presumptive form': '推量形',
  'neg. continuative form': '否定连用形',
  'old attributive form': '古连体形',
  'irregular -te form': '不规则て形',
  'irregular passive form': '不规则被动形',
  'irregular past-tense form': '不规则过去式',
  'non-standard て-form': '非规范て形'
};

const DIALECT_PLACES: Readonly<Record<string, string>> = {
  Akita: '秋田',
  Niigata: '新潟',
  Aichi: '爱知',
  Chūbu: '中部地区',
  Chūgoku: '中国地区',
  Ehime: '爱媛',
  Fukushima: '福岛',
  Gifu: '岐阜',
  Gunma: '群马',
  Hakata: '博多',
  Hiroshima: '广岛',
  'Izu Oshima': '伊豆大岛',
  Izumo: '出云',
  'Kagoshima/Miyazaki': '鹿儿岛／宫崎',
  Kanazawa: '金泽',
  Kōshū: '甲州',
  Mikawa: '三河',
  'Nagoya (and surrounding areas)': '名古屋及周边地区',
  Tochigi: '栃木',
  Tottori: '鸟取',
  Wakayama: '和歌山',
  Yamagata: '山形',
  Yamaguchi: '山口',
  Yamanashi: '山梨'
};

function hasBalancedDelimiters(value: string): boolean {
  const expected: string[] = [];
  for (const character of value) {
    if (character === '(') expected.push(')');
    else if (character === '（') expected.push('）');
    else if (character === '［') expected.push('］');
    else if (
      (character === ')' || character === '）' || character === '］')
      && expected.pop() !== character
    ) return false;
  }
  return expected.length === 0;
}

function hasSafePeriods(value: string): boolean {
  return [...value.matchAll(/\.+/g)].every(match => match[0] === '...');
}

function expression(value: string): string | null {
  if (
    value.length === 0
    || value !== value.trim()
    || !SAFE_EXPRESSION.test(value)
    || !JAPANESE_SCRIPT.test(value)
    || UNSAFE_EDGE_SEPARATOR.test(value)
    || !hasBalancedDelimiters(value)
    || !hasSafePeriods(value)
  ) return null;
  return value;
}

function displayExpression(value: string): string {
  return value
    .replaceAll('...', '…')
    .replaceAll('?', '？')
    .replace(/[,，]\s*/g, '、');
}

function quoteExpression(value: string): string {
  return `「${displayExpression(value)}」`;
}

function quoteListSegment(value: string): string | null {
  const whole = expression(value);
  if (!whole) return null;
  const items = whole.split(/[,，]\s*/);
  if (items.length === 1) return quoteExpression(whole);
  const validated = items.map(expression);
  return validated.some(item => item === null)
    ? null
    : validated.map(item => quoteExpression(item!)).join('、');
}

/**
 * Accept a literal expression or a closed list joined only by English
 * "and"/"or". Each member must independently satisfy the strict Japanese
 * expression grammar, so connectors cannot smuggle untranslated prose into
 * compile output.
 */
function quotedExpressionList(value: string): string | null {
  const single = expression(value);
  if (single) return quoteExpression(single);

  const parentheticalAlternative = /^(.+) \(or (.+)\)$/.exec(value);
  if (parentheticalAlternative) {
    const left = expression(parentheticalAlternative[1]!);
    const right = expression(parentheticalAlternative[2]!);
    return left && right ? `${quoteExpression(left)}或${quoteExpression(right)}` : null;
  }

  const pieces = value.split(/(\s+(?:and|or)\s+)/);
  if (pieces.length < 3 || pieces.length % 2 === 0) return null;
  let target = '';
  for (let index = 0; index < pieces.length; index++) {
    const piece = pieces[index]!;
    if (index % 2 === 0) {
      const item = quoteListSegment(piece);
      if (!item) return null;
      target += item;
    } else if (/^\s+and\s+$/.test(piece)) {
      target += '和';
    } else if (/^\s+or\s+$/.test(piece)) {
      target += '或';
    } else {
      return null;
    }
  }
  return target;
}

function result(
  rule: ZhHansSenseInfoPatternRuleId,
  target: string
): ZhHansSenseInfoPatternTranslation {
  return { policy: ZH_HANS_SENSE_INFO_PATTERN_POLICY, rule, target };
}

function captureQuoted(source: string, pattern: RegExp): string | null {
  const value = pattern.exec(source)?.[1];
  return value === undefined ? null : quotedExpressionList(value);
}

function directReading(
  source: string,
  verb: 'read' | 'pronounced'
): ZhHansSenseInfoPatternTranslation | null {
  const verbPattern = verb === 'read' ? 'read' : '(?:pronounced|pron\\.)';
  const qualified = new RegExp(
    `^(?:(also)|(usu\\.|usually)|(oft\\.|often)|(occ\\.|occasionally|sometimes))? ?${verbPattern}(?: as)? (.+?) (?:(?:as|when|only when|when used as|only when used as) a (suffix|prefix)|(in modern Japanese))$`
  ).exec(source);
  if (qualified) {
    const value = quotedExpressionList(qualified[5]!);
    if (!value) return null;
    const frequency = qualified[1] ? '也' : qualified[2] ? '通常'
      : qualified[3] ? '常' : qualified[4] ? '有时' : '';
    const context = qualified[6] === 'suffix' ? '用作后缀时'
      : qualified[6] === 'prefix' ? '用作前缀时' : '现代日语中';
    const rule = qualified[6] === 'suffix'
      ? (verb === 'read' ? 'suffix-read' : 'suffix-pronounced')
      : qualified[6] === 'prefix'
        ? (verb === 'read' ? 'prefix-read' : 'prefix-pronounced')
        : (verb === 'read' ? 'modern-read' : 'modern-pronounced');
    return result(rule, `${context}${frequency}读作${value}`);
  }

  const variants: readonly [RegExp, ZhHansSenseInfoPatternRuleId, string][] = verb === 'read'
    ? [
        [/^read(?: as)? (.+)$/, 'read', ''],
        [/^also read(?: as)? (.+)$/, 'also-read', '也'],
        [/^(?:usu\.|usually) read(?: as)? (.+)$/, 'usual-read', '通常'],
        [/^(?:oft\.|often) read(?: as)? (.+)$/, 'frequent-read', '常'],
        [/^(?:occ\.|occasionally|sometimes) read(?: as)? (.+)$/, 'occasional-read', '有时']
      ]
    : [
        [/^pronounced(?: as)? (.+)$/, 'pronounced', ''],
        [/^also pronounced(?: as)? (.+)$/, 'also-pronounced', '也'],
        [/^(?:usu\.|usually) pronounced(?: as)? (.+)$/, 'usual-pronounced', '通常'],
        [/^(?:oft\.|often) pronounced(?: as)? (.+)$/, 'frequent-pronounced', '常'],
        [/^(?:occ\.|occasionally|sometimes) pronounced(?: as)? (.+)$/, 'occasional-pronounced', '有时']
      ];
  for (const [pattern, rule, qualifier] of variants) {
    const value = captureQuoted(source, pattern);
    if (value) return result(rule, `${qualifier}读作${value}`);
  }
  return null;
}

function subjectReading(source: string): ZhHansSenseInfoPatternTranslation | null {
  const match = /^(.+) is (?:(also)|(usu\.|usually)|(oft\.|often)|(occ\.|occasionally|sometimes))? ?(?:read|pronounced)(?: as)? (.+)$/.exec(source);
  if (!match) return null;
  const subject = expression(match[1]!);
  const reading = expression(match[6]!);
  if (!subject || !reading) return null;
  const qualifier = match[2] ? '也' : match[3] ? '通常' : match[4] ? '常' : match[5] ? '有时' : '';
  return result(
    'subject-reading',
    `${quoteExpression(subject)}${qualifier}读作${quoteExpression(reading)}`
  );
}

function expressionRule(
  source: string,
  pattern: RegExp,
  rule: ZhHansSenseInfoPatternRuleId,
  render: (quotedValue: string) => string
): ZhHansSenseInfoPatternTranslation | null {
  const value = captureQuoted(source, pattern);
  return value ? result(rule, render(value)) : null;
}

function attachmentRule(source: string): ZhHansSenseInfoPatternTranslation | null {
  const match = /^(?:(usu\.|oft\.|often) )?(after|before|following|followed by) (.+)$/.exec(source);
  if (!match) return null;
  const term = ATTACHMENT_TERMS[match[3]!];
  if (!term) return null;
  const frequency = match[1] === 'usu.' ? '通常'
    : match[1] === 'oft.' || match[1] === 'often' ? '常' : '';
  const target = match[2] === 'before' ? `${frequency}用于${term}之前`
    : match[2] === 'followed by' ? `${frequency}后接${term}`
      : `${frequency}接在${term}之后`;
  return result('grammar-attachment', target);
}

function inflectionRule(source: string): ZhHansSenseInfoPatternTranslation | null {
  const match = /^(.+) of (?:the verb )?(.+)$/.exec(source);
  if (!match) return null;
  const relation = INFLECTION_RELATIONS[match[1]!];
  const value = quotedExpressionList(match[2]!);
  return relation && value ? result('inflection-form-of', `${value}的${relation}`) : null;
}

function variantRule(source: string): ZhHansSenseInfoPatternTranslation | null {
  const variants: readonly [RegExp, ZhHansSenseInfoPatternRuleId, string][] = [
    [/^(?:variant form|variant) of (.+)$/, 'variant-of', '变体'],
    [/^(?:non-standard variant|unorthodox variant|unorthodox version) of (.+)$/, 'nonstandard-variant-of', '非规范变体'],
    [/^incorrect variant of (.+)$/, 'incorrect-variant-of', '误用变体'],
    [/^(?:emphatic form|emphatic version|emph\. form|emph\. version|emphatic ver\.) of (.+)$/, 'emphatic-form-of', '强调形式']
  ];
  for (const [pattern, rule, label] of variants) {
    const value = captureQuoted(source, pattern);
    if (value) return result(rule, `${value}的${label}`);
  }
  const strongerVersion = captureQuoted(source, /^stronger version of (.+)$/);
  if (strongerVersion) {
    return result('stronger-version-of', `${strongerVersion}的强化形式`);
  }
  const stronger = captureQuoted(source, /^more emphatic than (.+)$/);
  return stronger ? result('more-emphatic-than', `比${stronger}语气更强`) : null;
}

function negativeContextRule(source: string): ZhHansSenseInfoPatternTranslation | null {
  const target = NEGATIVE_CONTEXTS[source];
  return target ? result('negative-context', target) : null;
}

function dialectRule(source: string): ZhHansSenseInfoPatternTranslation | null {
  const match = /^(.+) dialect$/.exec(source);
  const place = match ? DIALECT_PLACES[match[1]!] : undefined;
  return place ? result('place-dialect', `${place}方言`) : null;
}

/** Translate only a closed, auditable JMdict note grammar. */
export function translateZhHansSenseInfoPattern(
  source: string
): ZhHansSenseInfoPatternTranslation | null {
  if (source !== source.trim()) return null;
  if (source.startsWith('also formerly read as ')) {
    const value = captureQuoted(source, /^also formerly read as (.+)$/);
    if (value) return result('formerly-read', `过去也读作${value}`);
  }
  const reading = directReading(source, 'read') ?? directReading(source, 'pronounced');
  if (reading) return reading;
  const subject = subjectReading(source);
  if (subject) return subject;
  const structured = attachmentRule(source)
    ?? negativeContextRule(source)
    ?? inflectionRule(source)
    ?? variantRule(source)
    ?? dialectRule(source);
  if (structured) return structured;

  const rules: readonly [RegExp, ZhHansSenseInfoPatternRuleId, (value: string) => string][] = [
    [/^also written(?: as)? (.+)$/, 'also-written', value => `也写作${value}`],
    [/^(?:usu\.|usually) written(?: as)? (.+)$/, 'usual-written', value => `通常写作${value}`],
    [/^(?:oft\.|often) written(?: as)? (.+)$/, 'frequent-written', value => `常写作${value}`],
    [/^(?:occ\.|occasionally) written(?: as)? (.+)$/, 'occasional-written', value => `偶尔写作${value}`],
    [/^sometimes written(?: as)? (.+)$/, 'sometimes-written', value => `有时写作${value}`],
    [/^(?:esp\.|especially) adverbially as (.+)$/, 'adverbially-as', value => `尤以${value}作副词使用`],
    [/^(?:usu\.|usually) adverbially as (.+)$/, 'usual-adverbially-as', value => `通常以${value}作副词使用`],
    [/^(?:oft\.|[Oo]ften) adverbially as (.+)$/, 'frequent-adverbially-as', value => `常以${value}作副词使用`],
    [/^adverbially as (.+)$/, 'adverbially-as', value => `以${value}作副词使用`],
    [/^(?:esp\.|especially) as (.+)$/, 'especially-as', value => `尤作${value}`],
    [/^(?:usu\.|usually) as (.+)$/, 'usually-as', value => `通常作${value}`],
    [/^(?:oft\.|[Oo]ften) as (.+)$/, 'often-as', value => `常作${value}`],
    [/^as (.+)$/, 'as-expression', value => `作${value}`],
    [/^(?:esp\.|especially) (.+)$/, 'especially-expression', value => `尤作${value}`],
    [/^(?:usu\.|usually) (.+)$/, 'usually-expression', value => `通常作${value}`],
    [/^(?:oft\.|[Oo]ften) (.+)$/, 'often-expression', value => `常作${value}`],
    [/^(?:occ\.|occasionally) (.+)$/, 'occasional-expression', value => `偶尔作${value}`],
    [/^after (.+)$/, 'after-expression', value => `接在${value}之后`],
    [/^before (.+)$/, 'before-expression', value => `位于${value}之前`],
    [/^with (.+)$/, 'with-expression', value => `与${value}连用`],
    [/^(?:abbr\.|abbreviation) of (.+)$/, 'abbreviation-of', value => `${value}的缩写`],
    [/^short for (.+)$/, 'short-for', value => `${value}的简称`],
    [/^contraction of (.+)$/, 'contraction-of', value => `${value}的缩约形式`],
    [/^e\.g\. (.+)$/, 'example-expression', value => `例如${value}`],
    [/^(?:equiv\. of|equivalent (?:of|to)) (.+)$/, 'equivalent-expression', value => `相当于${value}`]
  ];
  for (const [pattern, rule, render] of rules) {
    const translated = expressionRule(source, pattern, rule, render);
    if (translated) return translated;
  }
  return null;
}
