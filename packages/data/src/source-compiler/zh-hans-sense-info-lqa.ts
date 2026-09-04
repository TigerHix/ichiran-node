import type { CanonicalEntry } from './model.js';
import type { ZhHansSenseInfoCatalog } from './zh-hans-sense-info.js';
import { translateZhHansSenseInfoPattern } from './zh-hans-sense-info-patterns.js';

export interface ZhHansSenseInfoRuleMatch {
  readonly source: string;
  readonly target: string;
  readonly ruleId: string;
}

/**
 * Replayable output from the closed deterministic pattern policy. This schema
 * intentionally cannot represent machine-translation drafts.
 */
export interface ZhHansSenseInfoRuleOutput {
  readonly formatVersion: 1;
  readonly locale: 'zh-Hans';
  readonly sourceLocale: 'en';
  readonly matches: readonly ZhHansSenseInfoRuleMatch[];
}

export interface ZhHansSenseInfoOccurrence {
  readonly seq: number;
  readonly sense: number;
  readonly info: number;
  readonly headwords: readonly string[];
  readonly englishGlosses: readonly string[];
}

export type ZhHansSenseInfoRisk = 'high' | 'medium' | 'low';
export type ZhHansSenseInfoResolution =
  | 'catalog'
  | 'deterministic-rule'
  | 'rule-collision'
  | 'unmatched';

export interface ZhHansSenseInfoRemainderItem {
  readonly source: string;
  readonly occurrenceCount: number;
  readonly clusterId: string;
  readonly signature: string;
  readonly resolution: Exclude<ZhHansSenseInfoResolution, 'catalog'>;
  readonly risk: ZhHansSenseInfoRisk;
  readonly riskReasons: readonly string[];
  readonly ruleSuggestions: readonly {
    readonly target: string;
    readonly ruleIds: readonly string[];
  }[];
  readonly occurrences: readonly ZhHansSenseInfoOccurrence[];
}

interface CoverageMeasure {
  readonly unique: number;
  readonly occurrences: number;
  readonly uniqueRatio: number;
  readonly occurrenceRatio: number;
}

export interface ZhHansSenseInfoLqaReport {
  readonly formatVersion: 1;
  readonly locale: 'zh-Hans';
  readonly sourceLocale: 'en';
  readonly inputPolicy: string;
  readonly coverage: {
    readonly source: { readonly unique: number; readonly occurrences: number };
    /** Exact reviewed translations that can ship. */
    readonly catalog: CoverageMeasure;
    /** Uncataloged sources resolved by one deterministic target. */
    readonly deterministicRules: CoverageMeasure;
    /** Exact catalog plus unambiguous deterministic-rule output. */
    readonly effectiveLocalized: CoverageMeasure;
    /** Uncataloged sources for which deterministic outputs disagree. */
    readonly ruleCollisions: CoverageMeasure;
    /** Uncataloged sources with no deterministic output. */
    readonly unmatched: CoverageMeasure;
    /** Rule collisions plus unmatched sources: the remaining translation queue. */
    readonly untranslatedRemainder: CoverageMeasure;
  };
  readonly clusters: readonly {
    readonly id: string;
    readonly label: string;
    readonly uniqueSourceCount: number;
    readonly occurrenceCount: number;
    readonly catalogUniqueCount: number;
    readonly deterministicRuleUniqueCount: number;
    readonly ruleCollisionUniqueCount: number;
    readonly unmatchedUniqueCount: number;
    readonly riskyUnmatchedUniqueCount: number;
    readonly patterns: readonly {
      readonly signature: string;
      readonly uniqueSourceCount: number;
      readonly occurrenceCount: number;
      readonly examples: readonly string[];
    }[];
  }[];
  /** Sources unresolved after exact-catalog precedence and deterministic rules. */
  readonly untranslatedRemainder: readonly ZhHansSenseInfoRemainderItem[];
  /** Formulaic-looking sources for which no deterministic rule produced a suggestion. */
  readonly riskyUnmatchedBoilerplate: readonly ZhHansSenseInfoRemainderItem[];
  readonly diagnostics: {
    readonly rules: readonly {
      readonly ruleId: string;
      readonly matchCount: number;
      readonly sourceCount: number;
      readonly sourceOccurrenceCount: number;
      readonly effectiveUncatalogedSourceCount: number;
      readonly collisionSourceCount: number;
      readonly catalogDisagreementCount: number;
      readonly staleSourceCount: number;
    }[];
    readonly ruleCollisions: readonly {
      readonly source: string;
      readonly occurrenceCount: number;
      readonly suggestions: readonly {
        readonly target: string;
        readonly ruleIds: readonly string[];
      }[];
    }[];
    readonly overlappingRules: readonly {
      readonly source: string;
      readonly target: string;
      readonly ruleIds: readonly string[];
    }[];
    readonly catalogRuleDisagreements: readonly {
      readonly source: string;
      readonly catalogTarget: string;
      readonly ruleSuggestions: readonly {
        readonly target: string;
        readonly ruleIds: readonly string[];
      }[];
    }[];
    readonly targetCollisions: readonly {
      readonly target: string;
      readonly sources: readonly {
        readonly source: string;
        readonly producers: readonly string[];
      }[];
    }[];
    readonly staleCatalogSources: readonly string[];
    readonly staleRuleSources: readonly {
      readonly source: string;
      readonly target: string;
      readonly ruleId: string;
    }[];
    readonly suspiciousTargets: readonly {
      readonly source: string;
      readonly target: string;
      readonly producer: string;
      readonly reasons: readonly string[];
    }[];
  };
  readonly agentQueues: {
    readonly translator: readonly {
      readonly priority: ZhHansSenseInfoRisk;
      readonly source: string;
      readonly occurrenceCount: number;
      readonly clusterId: string;
      readonly resolution: Exclude<ZhHansSenseInfoResolution, 'catalog'>;
      readonly suggestedTargets: readonly string[];
      readonly reason: string;
    }[];
    readonly reviewer: readonly {
      readonly priority: ZhHansSenseInfoRisk;
      readonly kind: string;
      readonly source: string;
      readonly detail: string;
    }[];
  };
}

function record(value: unknown, label: string): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
}

function exactKeys(
  value: Record<string, unknown>,
  expected: readonly string[],
  label: string
): void {
  const keys = Object.keys(value);
  const unknown = keys.filter(key => !expected.includes(key));
  const missing = expected.filter(key => !keys.includes(key));
  if (unknown.length > 0) throw new Error(`${label} has unknown fields: ${unknown.join(', ')}`);
  if (missing.length > 0) throw new Error(`${label} is missing fields: ${missing.join(', ')}`);
}

function nonemptyText(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.trim().length === 0 || value !== value.trim()) {
    throw new Error(`${label} must be trimmed, non-empty text`);
  }
  return value;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

export function parseZhHansSenseInfoRuleOutput(value: unknown): ZhHansSenseInfoRuleOutput {
  const output = record(value, 'zh-Hans sense-info deterministic-rule output');
  exactKeys(
    output,
    ['formatVersion', 'locale', 'sourceLocale', 'matches'],
    'zh-Hans sense-info deterministic-rule output'
  );
  if (output.formatVersion !== 1) {
    throw new Error('Unsupported zh-Hans sense-info deterministic-rule output format');
  }
  if (output.locale !== 'zh-Hans' || output.sourceLocale !== 'en') {
    throw new Error('Sense-info deterministic rules must translate en to zh-Hans');
  }
  if (!Array.isArray(output.matches)) {
    throw new Error('zh-Hans sense-info deterministic-rule matches must be an array');
  }
  let previousKey = '';
  const matches = output.matches.map((value, index) => {
    const match = record(value, `zh-Hans sense-info deterministic-rule match ${index}`);
    exactKeys(
      match,
      ['source', 'target', 'ruleId'],
      `zh-Hans sense-info deterministic-rule match ${index}`
    );
    const parsed = {
      source: nonemptyText(match.source, `deterministic-rule match ${index} source`),
      target: nonemptyText(match.target, `deterministic-rule match ${index} target`),
      ruleId: nonemptyText(match.ruleId, `deterministic-rule match ${index} ruleId`)
    };
    const key = `${parsed.source}\u0000${parsed.ruleId}\u0000${parsed.target}`;
    if (key <= previousKey) {
      throw new Error(
        'zh-Hans sense-info deterministic-rule matches must be unique and sorted by '
        + 'source, ruleId, and target'
      );
    }
    previousKey = key;
    return parsed;
  });
  return { formatVersion: 1, locale: 'zh-Hans', sourceLocale: 'en', matches };
}

const EMPTY_RULE_OUTPUT: ZhHansSenseInfoRuleOutput = {
  formatVersion: 1,
  locale: 'zh-Hans',
  sourceLocale: 'en',
  matches: []
};

const CLUSTERS = [
  { id: 'after', label: 'After/following constructions', test: /^(?:after|following)\b/i },
  { id: 'before', label: 'Before constructions', test: /^before\b/i },
  {
    id: 'usage',
    label: 'Usage instructions',
    test: /^(?:(?:not |may be |can be )?used\b|use of\b|also (?:written|pronounced|called|known)\b)/i
  },
  {
    id: 'restriction',
    label: 'Usually/only/especially restrictions',
    test: /^(?:(?:usually|only|chiefly|especially|mainly|primarily|often)\b|(?:usu|esp|oft)\.?(?:\s|$))/i
  },
  {
    id: 'form',
    label: 'Form and pattern descriptions',
    test: /^(?:in (?:the )?(?:form|pattern)\b|as\b|takes? (?:the )?(?:form|pattern)\b|e\.g\.(?:\s|$)|(?:contraction|corruption|emphatic|shortened) (?:form )?of\b)/i
  },
  {
    id: 'context',
    label: 'Contextual preposition/condition notes',
    test: /^(?:when|while|where|if|with|without|on|at|from|of|for|in)\b/i
  },
  {
    id: 'abbreviation',
    label: 'Abbreviation and expansion notes',
    test: /(?:\babbrev(?:iation|iated)?\b|\babbr\.(?:\s|$)|\bshort for\b|\bacronym\b)/i
  },
  {
    id: 'etymology',
    label: 'Origin, derivation, and wordplay notes',
    test: /^(?:orig\. meaning\b|original meaning\b|derived from\b|pun on\b|more emphatic than\b)/i
  },
  {
    id: 'register',
    label: 'Register, era, and jargon notes',
    test: /\b(?:slang|jargon|colloquial|honorific|polite|vulgar|derogatory|archaic|obsolete)\b/i
  },
  {
    id: 'named-entity',
    label: 'Name and proper-noun notes',
    test: /\b(?:name|surname|given name|proper noun|place name)\b/i
  }
] as const;

function clusterFor(source: string): { id: string; label: string } {
  return CLUSTERS.find(cluster => cluster.test.test(source)) ?? {
    id: 'freeform',
    label: 'Free-form semantic notes'
  };
}

function signatureFor(source: string): string {
  return source.normalize('NFKC').toLowerCase()
    .replace(/[“”][^“”]*[“”]/g, '<quoted>')
    .replace(/[‘’][^‘’]*[‘’]/g, '<quoted>')
    .replace(/"[^"]*"/g, '<quoted>')
    .replace(/'[^']*'/g, '<quoted>')
    .replace(/\([^()]*\)/g, '(<slot>)')
    .replace(/[\p{Script=Hiragana}\p{Script=Katakana}\p{Script=Han}々ー]+/gu, '<ja>')
    .replace(/\b\d+(?:[.,]\d+)*\b/g, '<num>')
    .replace(/\s+/g, ' ')
    .trim();
}

function balanced(source: string, open: string, close: string): boolean {
  let depth = 0;
  for (const character of source) {
    if (character === open) depth++;
    if (character === close && --depth < 0) return false;
  }
  return depth === 0;
}

function targetRiskReasons(source: string, target: string): string[] {
  const reasons: string[] = [];
  if (target === source) reasons.push('identity-translation');
  if (/^[\x00-\x7f]+$/.test(target)) reasons.push('ascii-only-target');
  const unexpectedLatin = target.match(/[A-Za-z]{4,}/g)?.some(token =>
    !new RegExp(
      `(^|[^A-Za-z])${token.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}([^A-Za-z]|$)`
    ).test(source)) ?? false;
  if (unexpectedLatin) reasons.push('long-latin-token');
  if (!balanced(target, '(', ')') || !balanced(target, '（', '）')) {
    reasons.push('unbalanced-target-delimiters');
  }
  if (/\{[^}]*$|^[^{]*\}/.test(target)) reasons.push('unbalanced-target-placeholder');
  return reasons;
}

function baseRisk(
  source: string,
  resolution: Exclude<ZhHansSenseInfoResolution, 'catalog'>,
  signatureFrequency: number
): { risk: ZhHansSenseInfoRisk; reasons: string[] } {
  const reasons: string[] = [];
  if (resolution === 'rule-collision') reasons.push('conflicting-rule-targets');
  if (resolution === 'deterministic-rule') reasons.push('deterministic-rule-output');
  const formulaic = /^(?:(?:after|following|before|used|not used|use of|may be used|can be used|usually|only|chiefly|especially|mainly|primarily|often|when|while|where|if|with|without|on|at|from|of|for|in|as|derived from|pun on|more emphatic than)\b|(?:usu|esp|oft|e\.g|abbr)\.?(?:\s|$)|also (?:written|pronounced|called|known)\b)/i.test(source);
  if (formulaic) reasons.push('formulaic-boilerplate');
  const parameterized = /["'“”‘’()]|[\p{Script=Hiragana}\p{Script=Katakana}\p{Script=Han}々ー]|\b\d+\b/u.test(source);
  if (parameterized) reasons.push('parameterized-source');
  if (signatureFrequency > 1) reasons.push('repeated-template-signature');
  if (!balanced(source, '(', ')')) reasons.push('unbalanced-source-delimiters');
  if (source.length > 160) reasons.push('long-source-note');
  const high = resolution === 'rule-collision'
    || reasons.includes('unbalanced-source-delimiters')
    || (formulaic && parameterized);
  return {
    risk: high ? 'high' : reasons.length > 0 ? 'medium' : 'low',
    reasons
  };
}

function inventory(entries: readonly CanonicalEntry[]): Map<string, {
  source: string;
  occurrenceCount: number;
  occurrences: ZhHansSenseInfoOccurrence[];
}> {
  const result = new Map<string, {
    source: string;
    occurrenceCount: number;
    occurrences: ZhHansSenseInfoOccurrence[];
  }>();
  for (const entry of entries) {
    const headwords = [...new Set([
      ...entry.kanji.map(form => form.text),
      ...entry.kana.map(form => form.text)
    ])];
    for (const sense of entry.senses) {
      for (const property of sense.properties) {
        if (property.tag !== 's_inf') continue;
        const item = result.get(property.text) ?? {
          source: property.text,
          occurrenceCount: 0,
          occurrences: []
        };
        item.occurrenceCount++;
        item.occurrences.push({
          seq: entry.seq,
          sense: sense.ordinal,
          info: property.ordinal,
          headwords,
          englishGlosses: sense.glosses
        });
        result.set(property.text, item);
      }
    }
  }
  for (const item of result.values()) {
    item.occurrences.sort((left, right) =>
      left.seq - right.seq || left.sense - right.sense || left.info - right.info);
  }
  return result;
}

/** Evaluate the current closed pattern grammar once per unique source note. */
export function buildZhHansSenseInfoRuleOutput(
  entries: readonly CanonicalEntry[]
): ZhHansSenseInfoRuleOutput {
  const matches = [...inventory(entries).keys()].sort(compareText).flatMap(source => {
    const translated = translateZhHansSenseInfoPattern(source);
    return translated === null ? [] : [{
      source,
      target: translated.target,
      ruleId: translated.rule
    }];
  });
  return {
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    matches
  };
}

function suggestionsFor(matches: readonly ZhHansSenseInfoRuleMatch[]): readonly {
  target: string;
  ruleIds: readonly string[];
}[] {
  const targets = new Map<string, Set<string>>();
  for (const match of matches) {
    const ids = targets.get(match.target) ?? new Set<string>();
    ids.add(match.ruleId);
    targets.set(match.target, ids);
  }
  return [...targets].map(([target, ids]) => ({
    target,
    ruleIds: [...ids].sort(compareText)
  })).sort((left, right) => compareText(left.target, right.target));
}

function ratio(value: number, total: number): number {
  return total === 0 ? 0 : Math.round((value / total) * 1_000_000) / 1_000_000;
}

function measure(
  items: readonly { readonly occurrenceCount: number }[],
  totalUnique: number,
  totalOccurrences: number
): CoverageMeasure {
  const occurrences = items.reduce((sum, item) => sum + item.occurrenceCount, 0);
  return {
    unique: items.length,
    occurrences,
    uniqueRatio: ratio(items.length, totalUnique),
    occurrenceRatio: ratio(occurrences, totalOccurrences)
  };
}

function riskRank(value: ZhHansSenseInfoRisk): number {
  return value === 'high' ? 0 : value === 'medium' ? 1 : 2;
}

/**
 * Build a deterministic LQA report from canonical English notes, the reviewed
 * exact catalog, and deterministic-rule output. Catalog matches take precedence;
 * unambiguous rule matches count as effective localized output, while this
 * analysis never mutates or promotes entries into the reviewed catalog.
 */
export function analyzeZhHansSenseInfoLqa(
  entries: readonly CanonicalEntry[],
  catalog: ZhHansSenseInfoCatalog,
  ruleOutput: ZhHansSenseInfoRuleOutput = EMPTY_RULE_OUTPUT
): ZhHansSenseInfoLqaReport {
  const sourceItems = inventory(entries);
  const catalogTargets = new Map(catalog.translations.map(value => [value.source, value.target]));
  const ruleMatches = [...ruleOutput.matches].sort((left, right) =>
    compareText(left.source, right.source)
    || compareText(left.ruleId, right.ruleId)
    || compareText(left.target, right.target));
  const matchesBySource = new Map<string, ZhHansSenseInfoRuleMatch[]>();
  for (const match of ruleMatches) {
    const values = matchesBySource.get(match.source) ?? [];
    values.push(match);
    matchesBySource.set(match.source, values);
  }

  const signatures = new Map<string, number>();
  for (const item of sourceItems.values()) {
    const signature = signatureFor(item.source);
    signatures.set(signature, (signatures.get(signature) ?? 0) + 1);
  }

  const all = [...sourceItems.values()].sort((left, right) => compareText(left.source, right.source));
  const cataloged = all.filter(item => catalogTargets.has(item.source));
  const unambiguous = all.filter(item =>
    !catalogTargets.has(item.source) && suggestionsFor(matchesBySource.get(item.source) ?? []).length === 1);
  const ambiguous = all.filter(item =>
    !catalogTargets.has(item.source) && suggestionsFor(matchesBySource.get(item.source) ?? []).length > 1);
  const unmatched = all.filter(item =>
    !catalogTargets.has(item.source) && suggestionsFor(matchesBySource.get(item.source) ?? []).length === 0);

  const uncataloged = all
    .filter(item => !catalogTargets.has(item.source))
    .map(item => {
      const suggestions = suggestionsFor(matchesBySource.get(item.source) ?? []);
      const resolution = suggestions.length === 0
        ? 'unmatched' as const
        : suggestions.length === 1 ? 'deterministic-rule' as const : 'rule-collision' as const;
      const signature = signatureFor(item.source);
      const risk = baseRisk(item.source, resolution, signatures.get(signature) ?? 1);
      return {
        source: item.source,
        occurrenceCount: item.occurrenceCount,
        clusterId: clusterFor(item.source).id,
        signature,
        resolution,
        risk: risk.risk,
        riskReasons: risk.reasons,
        ruleSuggestions: suggestions,
        occurrences: item.occurrences
      };
    }).sort((left, right) =>
      riskRank(left.risk) - riskRank(right.risk)
      || right.occurrenceCount - left.occurrenceCount
      || compareText(left.source, right.source));

  const remainder: ZhHansSenseInfoRemainderItem[] = uncataloged.filter(item =>
    item.resolution !== 'deterministic-rule');

  const riskyUnmatched = remainder.filter(item =>
    item.resolution === 'unmatched'
    && item.risk !== 'low'
    && item.riskReasons.some(reason => [
      'formulaic-boilerplate',
      'repeated-template-signature',
      'unbalanced-source-delimiters'
    ].includes(reason)));

  const clusterValues = new Map<string, {
    id: string;
    label: string;
    items: typeof all;
  }>();
  for (const item of all) {
    const cluster = clusterFor(item.source);
    const value = clusterValues.get(cluster.id) ?? { ...cluster, items: [] };
    value.items.push(item);
    clusterValues.set(cluster.id, value);
  }
  const uncatalogedBySource = new Map(uncataloged.map(item => [item.source, item]));
  const riskySources = new Set(riskyUnmatched.map(item => item.source));
  const clusters = [...clusterValues.values()].map(cluster => {
    const patterns = new Map<string, typeof all>();
    for (const item of cluster.items) {
      const signature = signatureFor(item.source);
      const values = patterns.get(signature) ?? [];
      values.push(item);
      patterns.set(signature, values);
    }
    const categorized = cluster.items.map(item =>
      uncatalogedBySource.get(item.source)?.resolution ?? 'catalog');
    return {
      id: cluster.id,
      label: cluster.label,
      uniqueSourceCount: cluster.items.length,
      occurrenceCount: cluster.items.reduce((sum, item) => sum + item.occurrenceCount, 0),
      catalogUniqueCount: categorized.filter(value => value === 'catalog').length,
      deterministicRuleUniqueCount: categorized.filter(
        value => value === 'deterministic-rule'
      ).length,
      ruleCollisionUniqueCount: categorized.filter(value => value === 'rule-collision').length,
      unmatchedUniqueCount: categorized.filter(value => value === 'unmatched').length,
      riskyUnmatchedUniqueCount: cluster.items.filter(item => riskySources.has(item.source)).length,
      patterns: [...patterns].map(([signature, values]) => ({
        signature,
        uniqueSourceCount: values.length,
        occurrenceCount: values.reduce((sum, item) => sum + item.occurrenceCount, 0),
        examples: values.map(item => item.source).sort(compareText).slice(0, 5)
      })).sort((left, right) =>
        right.occurrenceCount - left.occurrenceCount
        || right.uniqueSourceCount - left.uniqueSourceCount
        || compareText(left.signature, right.signature))
    };
  }).sort((left, right) =>
    right.occurrenceCount - left.occurrenceCount || compareText(left.id, right.id));

  const ruleCollisions = all.flatMap(item => {
    const suggestions = suggestionsFor(matchesBySource.get(item.source) ?? []);
    return suggestions.length > 1 ? [{
      source: item.source,
      occurrenceCount: item.occurrenceCount,
      suggestions
    }] : [];
  }).sort((left, right) =>
    right.occurrenceCount - left.occurrenceCount || compareText(left.source, right.source));

  const overlappingRules = all.flatMap(item =>
    suggestionsFor(matchesBySource.get(item.source) ?? []).flatMap(suggestion =>
      suggestion.ruleIds.length > 1 ? [{ source: item.source, ...suggestion }] : []))
    .sort((left, right) => compareText(left.source, right.source)
      || compareText(left.target, right.target));

  const catalogRuleDisagreements = all.flatMap(item => {
    const catalogTarget = catalogTargets.get(item.source);
    if (catalogTarget === undefined) return [];
    const ruleSuggestions = suggestionsFor(matchesBySource.get(item.source) ?? [])
      .filter(suggestion => suggestion.target !== catalogTarget);
    return ruleSuggestions.length > 0 ? [{ source: item.source, catalogTarget, ruleSuggestions }] : [];
  }).sort((left, right) => compareText(left.source, right.source));

  const staleCatalogSources = catalog.translations.map(value => value.source)
    .filter(source => !sourceItems.has(source)).sort(compareText);
  const staleRuleSources = ruleMatches.filter(match => !sourceItems.has(match.source));

  const rules = [...new Set(ruleMatches.map(match => match.ruleId))].sort(compareText).map(ruleId => {
    const matches = ruleMatches.filter(match => match.ruleId === ruleId);
    const sources = new Set(matches.map(match => match.source));
    return {
      ruleId,
      matchCount: matches.length,
      sourceCount: sources.size,
      sourceOccurrenceCount: [...sources].reduce(
        (sum, source) => sum + (sourceItems.get(source)?.occurrenceCount ?? 0),
        0
      ),
      effectiveUncatalogedSourceCount: [...sources].filter(source =>
        !catalogTargets.has(source)
        && suggestionsFor(matchesBySource.get(source) ?? []).length === 1).length,
      collisionSourceCount: [...sources].filter(source =>
        suggestionsFor(matchesBySource.get(source) ?? []).length > 1).length,
      catalogDisagreementCount: [...sources].filter(source => {
        const target = catalogTargets.get(source);
        return target !== undefined && matchesBySource.get(source)?.some(match => match.target !== target);
      }).length,
      staleSourceCount: [...sources].filter(source => !sourceItems.has(source)).length
    };
  });

  const targetSources = new Map<string, Map<string, Set<string>>>();
  for (const translation of catalog.translations) {
    const sources = targetSources.get(translation.target) ?? new Map<string, Set<string>>();
    const producers = sources.get(translation.source) ?? new Set<string>();
    producers.add('catalog');
    sources.set(translation.source, producers);
    targetSources.set(translation.target, sources);
  }
  for (const match of ruleMatches) {
    const sources = targetSources.get(match.target) ?? new Map<string, Set<string>>();
    const producers = sources.get(match.source) ?? new Set<string>();
    producers.add(`rule:${match.ruleId}`);
    sources.set(match.source, producers);
    targetSources.set(match.target, sources);
  }
  const targetCollisions = [...targetSources].flatMap(([target, sources]) =>
    sources.size > 1 ? [{
      target,
      sources: [...sources].map(([source, producers]) => ({
        source,
        producers: [...producers].sort(compareText)
      })).sort((left, right) => compareText(left.source, right.source))
    }] : []).sort((left, right) =>
    right.sources.length - left.sources.length || compareText(left.target, right.target));

  const suspiciousTargets = [
    ...catalog.translations.map(value => ({ ...value, producer: 'catalog' })),
    ...ruleMatches.map(value => ({
      source: value.source,
      target: value.target,
      producer: `rule:${value.ruleId}`
    }))
  ].flatMap(value => {
    const reasons = targetRiskReasons(value.source, value.target);
    return reasons.length > 0 ? [{ ...value, reasons }] : [];
  }).sort((left, right) => compareText(left.source, right.source)
    || compareText(left.producer, right.producer)
    || compareText(left.target, right.target));

  const reviewer = [
    ...ruleCollisions.map(value => ({
      priority: 'high' as const,
      kind: 'rule-collision',
      source: value.source,
      detail: value.suggestions.map(suggestion =>
        `${suggestion.target} <- ${suggestion.ruleIds.join(', ')}`).join(' | ')
    })),
    ...catalogRuleDisagreements.map(value => ({
      priority: 'high' as const,
      kind: 'catalog-rule-disagreement',
      source: value.source,
      detail: `catalog: ${value.catalogTarget}; rules: ${value.ruleSuggestions.map(
        suggestion => suggestion.target).join(' | ')}`
    })),
    ...overlappingRules.map(value => ({
      priority: 'medium' as const,
      kind: 'same-target-rule-overlap',
      source: value.source,
      detail: `${value.target} <- ${value.ruleIds.join(', ')}`
    })),
    ...suspiciousTargets.map(value => ({
      priority: value.reasons.includes('identity-translation') ? 'high' as const : 'medium' as const,
      kind: 'suspicious-target',
      source: value.source,
      detail: `${value.producer}: ${value.target} (${value.reasons.join(', ')})`
    })),
    ...staleCatalogSources.map(source => ({
      priority: 'low' as const,
      kind: 'stale-catalog-source',
      source,
      detail: 'Reviewed source is absent from the current English s_inf inventory'
    })),
    ...staleRuleSources.map(value => ({
      priority: 'low' as const,
      kind: 'stale-rule-source',
      source: value.source,
      detail: `${value.ruleId}: ${value.target}`
    })),
    ...targetCollisions.map(value => ({
      priority: 'low' as const,
      kind: 'target-reuse',
      source: value.sources.map(source => source.source).join(' | '),
      detail: `${value.target} is emitted for ${value.sources.length} English sources`
    }))
  ].sort((left, right) => riskRank(left.priority) - riskRank(right.priority)
    || compareText(left.kind, right.kind)
    || compareText(left.source, right.source)
    || compareText(left.detail, right.detail));

  const totalOccurrences = all.reduce((sum, item) => sum + item.occurrenceCount, 0);
  return {
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    inputPolicy: 'Canonical JMdict English s_inf, reviewed exact catalog, and closed deterministic-rule output only; no machine-translation or draft output',
    coverage: {
      source: { unique: all.length, occurrences: totalOccurrences },
      catalog: measure(cataloged, all.length, totalOccurrences),
      deterministicRules: measure(unambiguous, all.length, totalOccurrences),
      effectiveLocalized: measure([...cataloged, ...unambiguous], all.length, totalOccurrences),
      ruleCollisions: measure(ambiguous, all.length, totalOccurrences),
      unmatched: measure(unmatched, all.length, totalOccurrences),
      untranslatedRemainder: measure([...ambiguous, ...unmatched], all.length, totalOccurrences)
    },
    clusters,
    untranslatedRemainder: remainder,
    riskyUnmatchedBoilerplate: riskyUnmatched,
    diagnostics: {
      rules,
      ruleCollisions,
      overlappingRules,
      catalogRuleDisagreements,
      targetCollisions,
      staleCatalogSources,
      staleRuleSources,
      suspiciousTargets
    },
    agentQueues: {
      translator: remainder.map(item => ({
        priority: item.risk,
        source: item.source,
        occurrenceCount: item.occurrenceCount,
        clusterId: item.clusterId,
        resolution: item.resolution,
        suggestedTargets: item.ruleSuggestions.map(value => value.target),
        reason: item.resolution === 'unmatched'
          ? 'Translate from English source and occurrence context'
        : 'Resolve deterministic-rule disagreement before cataloging'
      })),
      reviewer
    }
  };
}
