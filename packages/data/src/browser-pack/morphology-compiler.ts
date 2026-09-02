import {
  constructConjugation,
  getConjRules,
  getPosIndex,
  loadAllConjugationRules,
  SECONDARY_CONJUGATION_TYPES,
  SECONDARY_CONJUGATION_TYPES_FROM,
  type ConjugationRule,
  type ConjugationRulePaths
} from '../data/conj-rules.js';
import {
  encodeMorphologyArtifact,
  type CompiledMorphologyArtifact,
  type CompiledMorphologyPatch,
  type CompiledMorphologyRootGroup,
  type CompiledMorphologyRootKey,
  type CompiledMorphologyRule,
  type CompiledMorphologyTemplate,
  type CompiledMorphologyTombstone,
  type MorphologyRoute
} from './morphology-format.js';

export interface MorphologyRootSource {
  seq: number;
  pos: string;
  route: MorphologyRoute;
  text: string;
  ord: number;
  common: number | null;
  counterpart: string | null;
}

export interface MorphologyRootFormSource {
  seq: number;
  text: string;
}

export interface MorphologyManualPatchSource {
  route: MorphologyRoute;
  surface: string;
  rootSeq: number;
  pos: string;
  conjType: number;
  negative: boolean | null;
  formal: boolean | null;
  sourceText: string;
  sourceCounterpart: string | null;
  targetCounterpart: string | null;
  ord: number;
  common: number | null;
}

export interface MorphologySource {
  readonly roots: readonly MorphologyRootSource[];
  readonly rootForms: readonly MorphologyRootFormSource[];
  readonly manualPatches: readonly MorphologyManualPatchSource[];
}

interface PendingTemplate {
  suffix: string;
  removed: string;
  firstRuleKey: string;
  secondRuleKey: string | null;
}

interface PendingPatch extends Omit<CompiledMorphologyPatch, 'firstRule' | 'secondRule'> {
  firstRuleKey: string;
  secondRuleKey: string | null;
}

export interface MorphologyCompileStats {
  bytes: number;
  positions: number;
  rules: number;
  directTemplates: number;
  secondaryTemplates: number;
  templates: number;
  suffixes: number;
  rootRows: number;
  rootKeys: number;
  rootGroups: number;
  rootForms: number;
  patches: number;
  tombstones: number;
}

export interface MorphologyCompileResult {
  bytes: Uint8Array;
  artifact: CompiledMorphologyArtifact;
  stats: MorphologyCompileStats;
}

const TOMBSTONE_SPECS = [
  {
    route: 'kana' as const, rootSeq: 2257550, sourceText: 'ない', surface: 'な',
    firstPos: 'adj-i', firstType: 51, secondPos: null, secondType: null
  },
  {
    route: 'kana' as const, rootSeq: 2684620, sourceText: 'しい', surface: 'し',
    firstPos: 'adj-i', firstType: 51, secondPos: null, secondType: null
  },
  {
    route: 'kana' as const, rootSeq: 1593170, sourceText: 'コケる', surface: 'コケさせ',
    firstPos: 'v1', firstType: 7, secondPos: 'v1', secondType: 13
  },
  {
    route: 'kana' as const, rootSeq: 1593170, sourceText: 'コケる', surface: 'コケさせ',
    firstPos: 'v1', firstType: 53, secondPos: 'v5s', secondType: 10
  }
] as const;

function ruleKey(rule: CompiledMorphologyRule): string {
  return JSON.stringify(rule);
}

function templateKey(template: PendingTemplate): string {
  return `${template.suffix}\u0000${template.removed}\u0000${template.firstRuleKey}\u0000${template.secondRuleKey ?? ''}`;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function longestCommonPrefix(left: string, right: string): number {
  const length = Math.min(left.length, right.length);
  let index = 0;
  while (index < length && left.charCodeAt(index) === right.charCodeAt(index)) index++;
  return index;
}

function effectiveRule(pos: string, rule: ConjugationRule, peers: readonly ConjugationRule[]): CompiledMorphologyRule {
  const sameType = peers.filter(peer => peer.conj === rule.conj);
  const ignoresNegative = !sameType.some(peer => peer.neg);
  const ignoresFormal = !sameType.some(peer => peer.fml);
  return {
    pos,
    type: rule.conj,
    negative: ignoresNegative ? null : rule.neg,
    formal: ignoresFormal ? null : rule.fml,
    ordinal: rule.onum,
    stem: rule.stem,
    okuri: rule.okuri,
    euphr: rule.euphr,
    euphk: rule.euphk
  };
}

function manualRule(row: MorphologyManualPatchSource, ordinal: number): CompiledMorphologyRule {
  return {
    pos: row.pos,
    type: row.conjType,
    negative: row.negative,
    formal: row.formal,
    ordinal,
    stem: 0,
    okuri: '',
    euphr: '',
    euphk: ''
  };
}

function makePendingTemplates(
  roots: readonly MorphologyRootSource[],
  rulesByKey: Map<string, CompiledMorphologyRule>
): { templates: PendingTemplate[]; direct: number; secondary: number } {
  const sourcesByPos = new Map<string, Set<string>>();
  for (const root of roots) {
    let sources = sourcesByPos.get(root.pos);
    if (!sources) {
      sources = new Set();
      sourcesByPos.set(root.pos, sources);
    }
    sources.add(root.text);
  }

  const templates = new Map<string, PendingTemplate>();
  for (const pos of [...sourcesByPos.keys()].sort()) {
    const posId = getPosIndex(pos);
    if (posId === undefined) throw new Error(`No conjugation POS ID for installed POS ${pos}`);
    const firstRules = getConjRules(posId);
    for (const source of [...sourcesByPos.get(pos)!].sort()) {
      for (const first of firstRules) {
        // The materializer deliberately excludes the irregular ある negative
        // stem even though the patched rule table contains it.
        if (pos === 'v5r-i' && first.conj === 52) continue;
        const firstCompiled = effectiveRule(pos, first, firstRules);
        const firstKey = ruleKey(firstCompiled);
        rulesByKey.set(firstKey, firstCompiled);
        const intermediate = constructConjugation(source, first);
        const directPrefix = longestCommonPrefix(source, intermediate);
        const direct: PendingTemplate = {
          suffix: intermediate.slice(directPrefix),
          removed: source.slice(directPrefix),
          firstRuleKey: firstKey,
          secondRuleKey: null
        };
        templates.set(templateKey(direct), direct);

        if (
          !SECONDARY_CONJUGATION_TYPES_FROM.includes(first.conj)
          || first.neg
          || first.fml
          || pos === 'vs-i'
          || pos === 'vs-s'
        ) continue;

        const secondPos = first.conj === 53 ? 'v5s' : 'v1';
        const secondPosId = getPosIndex(secondPos);
        if (secondPosId === undefined) throw new Error(`No secondary POS ID for ${secondPos}`);
        const secondRules = getConjRules(secondPosId);
        for (const second of secondRules) {
          if (!SECONDARY_CONJUGATION_TYPES.includes(second.conj)) continue;
          const secondCompiled = effectiveRule(secondPos, second, secondRules);
          const secondKey = ruleKey(secondCompiled);
          rulesByKey.set(secondKey, secondCompiled);
          const generated = constructConjugation(intermediate, second);
          const prefix = longestCommonPrefix(source, generated);
          const secondary: PendingTemplate = {
            suffix: generated.slice(prefix),
            removed: source.slice(prefix),
            firstRuleKey: firstKey,
            secondRuleKey: secondKey
          };
          templates.set(templateKey(secondary), secondary);
        }
      }
    }
  }

  const values = [...templates.values()];
  const direct = values.filter(template => template.secondRuleKey === null).length;
  return { templates: values, direct, secondary: values.length - direct };
}

function canonicalizeRules(rulesByKey: Map<string, CompiledMorphologyRule>): {
  rules: CompiledMorphologyRule[];
  ids: Map<string, number>;
} {
  const rules = [...rulesByKey.values()].sort((left, right) => compareText(ruleKey(left), ruleKey(right)));
  return { rules, ids: new Map(rules.map((rule, index) => [ruleKey(rule), index])) };
}

function canonicalizeTemplates(pending: readonly PendingTemplate[], ruleIds: Map<string, number>): CompiledMorphologyTemplate[] {
  const templates = pending.map(template => {
    const firstRule = ruleIds.get(template.firstRuleKey);
    const secondRule = template.secondRuleKey === null ? null : ruleIds.get(template.secondRuleKey);
    if (firstRule === undefined || secondRule === undefined) throw new Error('Template references an uncompiled rule');
    return { suffix: template.suffix, removed: template.removed, firstRule, secondRule };
  });
  templates.sort((left, right) => compareText(
    `${left.suffix}\u0000${left.removed}\u0000${left.firstRule.toString().padStart(8, '0')}\u0000${String(left.secondRule ?? 0xffff_ffff).padStart(10, '0')}`,
    `${right.suffix}\u0000${right.removed}\u0000${right.firstRule.toString().padStart(8, '0')}\u0000${String(right.secondRule ?? 0xffff_ffff).padStart(10, '0')}`
  ));
  return templates;
}

function compileRootGroups(forms: readonly MorphologyRootFormSource[]): {
  groups: CompiledMorphologyRootGroup[];
  groupBySeq: Map<number, number>;
} {
  const formsBySeq = new Map<number, Set<string>>();
  for (const row of forms) {
    let values = formsBySeq.get(row.seq);
    if (!values) {
      values = new Set();
      formsBySeq.set(row.seq, values);
    }
    values.add(row.text);
  }
  const groups = [...formsBySeq].map(([seq, values]) => ({ seq, forms: [...values].sort() }));
  groups.sort((left, right) => left.seq - right.seq);
  return { groups, groupBySeq: new Map(groups.map((group, index) => [group.seq, index])) };
}

function compileRootKeys(rows: readonly MorphologyRootSource[], groupBySeq: Map<number, number>): CompiledMorphologyRootKey[] {
  const keys = new Map<string, CompiledMorphologyRootKey>();
  for (const row of rows) {
    const keyText = `${row.route}\u0000${row.pos}\u0000${row.text}`;
    let key = keys.get(keyText);
    if (!key) {
      key = { route: row.route, pos: row.pos, sourceText: row.text, records: [] };
      keys.set(keyText, key);
    }
    const rootGroup = groupBySeq.get(row.seq);
    if (rootGroup === undefined) throw new Error(`Root ${row.seq} has no root-form group`);
    const sourceForm = row.route === 'kanji' ? row.text : row.counterpart ?? row.text;
    const sourceReading = row.route === 'kana' ? row.text : row.counterpart ?? row.text;
    const record = { rootGroup, sourceForm, sourceReading, ord: row.ord, common: row.common };
    if (!key.records.some(existing => JSON.stringify(existing) === JSON.stringify(record))) key.records.push(record);
  }

  const routeOrder = (route: MorphologyRoute): number => route === 'kana' ? 0 : 1;
  const values = [...keys.values()];
  for (const key of values) {
    key.records.sort((left, right) =>
      left.rootGroup - right.rootGroup
      || left.ord - right.ord
      || (left.common ?? 0xff) - (right.common ?? 0xff)
      || compareText(left.sourceForm, right.sourceForm)
      || compareText(left.sourceReading, right.sourceReading)
    );
  }
  values.sort((left, right) =>
    routeOrder(left.route) - routeOrder(right.route)
    || compareText(left.pos, right.pos)
    || compareText(left.sourceText, right.sourceText)
  );
  return values;
}

function manualOrdinal(row: MorphologyManualPatchSource): number {
  const posId = getPosIndex(row.pos);
  if (posId === undefined) return 0;
  const ordinals = getConjRules(posId)
    .filter(rule =>
      rule.conj === row.conjType
      && rule.neg === (row.negative ?? false)
      && rule.fml === (row.formal ?? false)
    )
    .map(rule => rule.onum);
  return ordinals.length === 0 ? 0 : Math.min(...ordinals);
}

function makePendingPatches(
  rows: readonly MorphologyManualPatchSource[],
  rulesByKey: Map<string, CompiledMorphologyRule>
): PendingPatch[] {
  const patches: PendingPatch[] = [];
  const rowKey = (row: MorphologyManualPatchSource, sourceText: string): string => JSON.stringify([
    row.route,
    row.rootSeq,
    row.pos,
    row.conjType,
    row.negative,
    row.formal,
    sourceText
  ]);
  const bySource = new Map(rows.map(row => [rowKey(row, row.sourceText), row]));
  for (const row of rows) {
    const rule = manualRule(row, manualOrdinal(row));
    const firstRuleKey = ruleKey(rule);
    rulesByKey.set(firstRuleKey, rule);
    const sourceForm = row.route === 'kanji' ? row.sourceText : row.sourceCounterpart ?? row.sourceText;
    const sourceReading = row.route === 'kana' ? row.sourceText : row.sourceCounterpart ?? row.sourceText;
    const counterpartRoute: MorphologyRoute = row.route === 'kana' ? 'kanji' : 'kana';
    const counterpartRow = row.sourceCounterpart === null
      ? undefined
      : bySource.get(rowKey({ ...row, route: counterpartRoute }, row.sourceCounterpart));
    const form = row.route === 'kanji' ? row.surface : counterpartRow?.surface ?? row.targetCounterpart ?? row.surface;
    const reading = row.route === 'kana' ? row.surface : counterpartRow?.surface ?? row.targetCounterpart ?? row.surface;
    patches.push({
      route: row.route,
      surface: row.surface,
      rootSeq: row.rootSeq,
      sourceText: row.sourceText,
      sourceForm,
      sourceReading,
      form,
      reading,
      firstRuleKey,
      secondRuleKey: null,
      intermediate: null,
      ord: row.ord,
      common: row.common
    });
  }
  return patches;
}

function canonicalizePatches(pending: readonly PendingPatch[], ruleIds: Map<string, number>): CompiledMorphologyPatch[] {
  const routeOrder = (route: MorphologyRoute): number => route === 'kana' ? 0 : 1;
  const patches = pending.map(patch => {
    const firstRule = ruleIds.get(patch.firstRuleKey);
    const secondRule = patch.secondRuleKey === null ? null : ruleIds.get(patch.secondRuleKey);
    if (firstRule === undefined || secondRule === undefined) throw new Error('Patch references an uncompiled rule');
    const { firstRuleKey: _firstRuleKey, secondRuleKey: _secondRuleKey, ...rest } = patch;
    void _firstRuleKey;
    void _secondRuleKey;
    return { ...rest, firstRule, secondRule };
  });
  patches.sort((left, right) =>
    routeOrder(left.route) - routeOrder(right.route)
    || compareText(left.surface, right.surface)
    || left.rootSeq - right.rootSeq
    || compareText(left.sourceText, right.sourceText)
    || left.firstRule - right.firstRule
    || (left.secondRule ?? 0xffff_ffff) - (right.secondRule ?? 0xffff_ffff)
  );
  return patches;
}

function compileTombstones(
  templates: readonly CompiledMorphologyTemplate[],
  rules: readonly CompiledMorphologyRule[]
): CompiledMorphologyTombstone[] {
  const tombstones: CompiledMorphologyTombstone[] = [];
  for (const spec of TOMBSTONE_SPECS) {
    const matchingRules = new Map<string, readonly [number, number | null]>();
    templates.forEach(template => {
      if ((template.secondRule === null) !== (spec.secondType === null)) return;
      const firstRule = rules[template.firstRule]!;
      const secondRule = template.secondRule === null ? null : rules[template.secondRule]!;
      const intermediate = constructFromCompiled(spec.sourceText, firstRule);
      const generated = secondRule === null ? intermediate : constructFromCompiled(intermediate, secondRule);
      if (
        firstRule.pos === spec.firstPos
        && firstRule.type === spec.firstType
        && secondRule?.pos === (spec.secondPos ?? undefined)
        && secondRule?.type === (spec.secondType ?? undefined)
        && generated === spec.surface
      ) matchingRules.set(
        `${template.firstRule}\u0000${template.secondRule ?? ''}`,
        [template.firstRule, template.secondRule]
      );
    });
    if (matchingRules.size === 0) throw new Error(`Tombstone ${spec.rootSeq}/${spec.surface} matches no rule`);
    for (const [firstRule, secondRule] of matchingRules.values()) {
      tombstones.push({
        route: spec.route,
        surface: spec.surface,
        rootSeq: spec.rootSeq,
        firstRule,
        secondRule
      });
    }
  }
  tombstones.sort((left, right) =>
    (left.route === 'kana' ? 0 : 1) - (right.route === 'kana' ? 0 : 1)
    || compareText(left.surface, right.surface)
    || left.rootSeq - right.rootSeq
    || left.firstRule - right.firstRule
  );
  return tombstones;
}

function constructFromCompiled(word: string, rule: CompiledMorphologyRule): string {
  const kana = /^[ァ-ヺヽヾーぁ-ゔゝゞー]+$/.test(word.slice(Math.max(0, word.length - 2)));
  const euphony = kana ? rule.euphr : rule.euphk;
  return word.slice(0, word.length - rule.stem - (euphony.length > 0 ? 1 : 0)) + euphony + rule.okuri;
}

/** Builds format-v1 morphology directly from compiler-owned semantic input. */
export function buildMorphology(
  source: MorphologySource,
  options: {
    readonly dataPath?: string;
    readonly conjugationRules?: ConjugationRulePaths;
  } = {}
): MorphologyCompileResult {
  loadAllConjugationRules(options.conjugationRules ?? options.dataPath ?? 'data');
  const rulesByKey = new Map<string, CompiledMorphologyRule>();
  const pendingTemplates = makePendingTemplates(source.roots, rulesByKey);
  const pendingPatches = makePendingPatches(source.manualPatches, rulesByKey);
  const { rules, ids: ruleIds } = canonicalizeRules(rulesByKey);
  const templates = canonicalizeTemplates(pendingTemplates.templates, ruleIds);
  const { groups: rootGroups, groupBySeq } = compileRootGroups(source.rootForms);
  const rootKeys = compileRootKeys(source.roots, groupBySeq);
  const patches = canonicalizePatches(pendingPatches, ruleIds);
  const tombstones = compileTombstones(templates, rules);
  const positions = [...new Set([
    ...rules.map(rule => rule.pos),
    ...rootKeys.map(key => key.pos)
  ])].sort();

  const artifact: CompiledMorphologyArtifact = {
    positions,
    rules,
    templates,
    rootKeys,
    rootGroups,
    patches,
    tombstones
  };
  const bytes = encodeMorphologyArtifact(artifact);
  return {
    bytes,
    artifact,
    stats: {
      bytes: bytes.byteLength,
      positions: positions.length,
      rules: rules.length,
      directTemplates: pendingTemplates.direct,
      secondaryTemplates: pendingTemplates.secondary,
      templates: templates.length,
      suffixes: new Set(templates.map(template => template.suffix)).size,
      rootRows: source.roots.length,
      rootKeys: rootKeys.length,
      rootGroups: rootGroups.length,
      rootForms: source.rootForms.length,
      patches: patches.length,
      tombstones: tombstones.length
    }
  };
}
