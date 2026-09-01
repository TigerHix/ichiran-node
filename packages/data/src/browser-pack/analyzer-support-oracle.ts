import type postgres from 'postgres';
import type { AsyncSplitFunction, HintFunction } from '@ichiran/reference-postgres/src/dict/splitMaps.js';
import type { KanaText, Reading } from '@ichiran/reference-postgres/src/types.js';

import {
  ANALYZER_SUPPORT_COUNTER_CLASSES,
  analyzerSupportCollisionKey,
  analyzerSupportHintKey,
  analyzerSupportSplitKey,
  AnalyzerSupportEncodingError,
  type AnalyzerSupportCollisionSource,
  type AnalyzerSupportCompileIssue,
  type AnalyzerSupportConjugationSource,
  type AnalyzerSupportCounterClass,
  type AnalyzerSupportCounterSource,
  type AnalyzerSupportHintSource,
  type AnalyzerSupportRoute,
  type AnalyzerSupportSource,
  type AnalyzerSupportSplitConjugationSource,
  type AnalyzerSupportSplitPartSource,
  type AnalyzerSupportSplitSource,
  type AnalyzerSupportSuffixFormSource,
  type AnalyzerSupportSuffixSource
} from './analyzer-support.js';
import { loadAnalyzerGeneratedSource } from './analyzer-generated-oracle.js';
import { compileMorphology } from './morphology-compiler-oracle.js';
import type {
  CompiledMorphologyArtifact,
  CompiledMorphologyRule
} from './morphology-format.js';

const NONE = 0xffff_ffff;

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

async function loadAnalyzerSupportOracleRuntime() {
  await import('@ichiran/reference-postgres/src/dict/splitDefinitions.js');
  const [
    connection,
    characters,
    errata,
    counters,
    splitMaps,
    suffixCache,
    upstream
  ] = await Promise.all([
    import('@ichiran/reference-postgres/src/conn.js'),
    import('@ichiran/reference-postgres/src/characters.js'),
    import('@ichiran/reference-postgres/src/dict/errata.js'),
    import('@ichiran/reference-postgres/src/dict/counters.js'),
    import('@ichiran/reference-postgres/src/dict/splitMaps.js'),
    import('@ichiran/reference-postgres/src/grammar/suffixCache.js'),
    import('./analyzer-upstream-260118.js')
  ]);
  return {
    connection,
    characters,
    errata,
    counters,
    splitMaps,
    suffixCache,
    upstream
  };
}

type AnalyzerSupportOracleRuntime = Awaited<ReturnType<typeof loadAnalyzerSupportOracleRuntime>>;

interface DirectFormRow {
  seq: number;
  route: AnalyzerSupportRoute;
  text: string;
  ord: number;
  common: number | null;
  commonTags: string;
  conjugatable: boolean;
  nokanji: boolean;
  best: string | null;
}

interface AnnotationCandidate {
  rootSeq: number;
  route: AnalyzerSupportRoute;
  surface: string;
  form: string;
  reading: string;
  ord: number;
  common: number | null;
  ruleIds: readonly [number] | readonly [number, number] | null;
}

interface CollisionPathRow {
  collisionSeq: number;
  rootSeq: number;
  via: number | null;
  pos: string;
  conjType: number;
  negative: boolean | null;
  formal: boolean | null;
  sourceText: string;
  surface: string;
}

interface CollisionEntryRow {
  seq: number;
  nKanji: number;
  nKana: number;
  primaryNokanji: boolean;
  archived: boolean;
  preferKana: boolean;
  preferKanaOnOrdinalZero: boolean;
  pos: string[] | null;
}

interface RawSuffixFormSource extends Omit<AnalyzerSupportSuffixFormSource, 'conjugations'> {
  readonly conjugations: ':root' | readonly number[] | null;
}

interface RawSuffixSource extends Omit<AnalyzerSupportSuffixSource, 'values'> {
  readonly values: readonly {
    readonly keyword: string;
    readonly form: RawSuffixFormSource | null;
  }[];
}

interface SuffixConjugationRow extends AnalyzerSupportConjugationSource {
  conjugationId: number;
  propertyId: number;
  surface: string;
}

function applyMorphologyRule(word: string, rule: CompiledMorphologyRule): string {
  const kana = /^[ァ-ヺヽヾーぁ-ゔゝゞー]+$/.test(word.slice(Math.max(0, word.length - 2)));
  const euphony = kana ? rule.euphr : rule.euphk;
  return word.slice(0, word.length - rule.stem - (euphony.length > 0 ? 1 : 0)) + euphony + rule.okuri;
}

function annotationCandidateKey(value: AnnotationCandidate): string {
  return JSON.stringify([
    value.rootSeq, value.route, value.surface, value.form, value.reading, value.ruleIds
  ]);
}

function enumerateMorphologyCandidates(
  artifact: CompiledMorphologyArtifact,
  selectedRoots: ReadonlySet<number>
): AnnotationCandidate[] {
  const templatesByPos = new Map<string, typeof artifact.templates>();
  for (const template of artifact.templates) {
    const pos = artifact.rules[template.firstRule]!.pos;
    let values = templatesByPos.get(pos);
    if (!values) {
      values = [];
      templatesByPos.set(pos, values);
    }
    (values as typeof artifact.templates[number][]).push(template);
  }
  const rootForms = new Map(artifact.rootGroups.map(group => [group.seq, new Set(group.forms)]));
  const tombstones = new Set(artifact.tombstones.map(value => JSON.stringify([
    value.route, value.surface, value.rootSeq, value.firstRule, value.secondRule
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
        const surface = second ? applyMorphologyRule(intermediateSurface, second) : intermediateSurface;
        if (rootForms.get(group.seq)?.has(surface)) continue;
        if (tombstones.has(JSON.stringify([
          key.route, surface, group.seq, template.firstRule, template.secondRule
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
        candidates.set(annotationCandidateKey(value), value);
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
    candidates.set(annotationCandidateKey(value), value);
  }
  return [...candidates.values()].sort((left, right) =>
    compareText(annotationCandidateKey(left), annotationCandidateKey(right)));
}

function ruleMatches(
  rule: CompiledMorphologyRule,
  row: Pick<CollisionPathRow, 'pos' | 'conjType' | 'negative' | 'formal' | 'sourceText' | 'surface'>
): boolean {
  return rule.pos === row.pos
    && rule.type === row.conjType
    && (rule.negative === null || rule.negative === row.negative)
    && (rule.formal === null || rule.formal === row.formal)
    && applyMorphologyRule(row.sourceText, rule) === row.surface;
}

async function loadCollisionSources(
  sql: postgres.Sql,
  artifact: CompiledMorphologyArtifact,
  runtime: AnalyzerSupportOracleRuntime
): Promise<AnalyzerSupportCollisionSource[]> {
  const pathRows = await sql<CollisionPathRow[]>`
    SELECT c.seq AS collision_seq, c."from" AS root_seq, c.via,
           cp.pos, cp.conj_type, cp.neg AS negative, cp.fml AS formal,
           csr.source_text, csr.text AS surface
    FROM conjugation c
    JOIN entry target ON target.seq = c.seq AND target.root_p
    JOIN conj_prop cp ON cp.conj_id = c.id
    JOIN conj_source_reading csr ON csr.conj_id = c.id
    ORDER BY c."from", c.seq, c.id, cp.id, csr.source_text COLLATE "C", csr.text COLLATE "C"
  `;
  const collisionSeqs = [...new Set(pathRows.map(row => row.collisionSeq))];
  if (collisionSeqs.length === 0) return [];
  const entryRows = await sql<CollisionEntryRow[]>`
    WITH archived AS (
      SELECT sense.seq
      FROM sense
      LEFT JOIN sense_prop sp ON sp.sense_id = sense.id
        AND sp.tag = 'misc' AND sp.text IN ('arch', 'obsc', 'rare')
      WHERE sense.seq = ANY(${collisionSeqs})
      GROUP BY sense.seq
      HAVING EVERY(sp.id IS NOT NULL)
    ), facts AS (
      SELECT e.seq, e.n_kanji, e.n_kana, e.primary_nokanji,
             EXISTS (SELECT 1 FROM archived a WHERE a.seq = e.seq) AS archived,
             EXISTS (
               SELECT 1 FROM sense_prop sp
               WHERE sp.seq = e.seq AND sp.tag = 'misc' AND sp.text = 'uk'
             ) AS prefer_kana,
             EXISTS (
               SELECT 1 FROM sense_prop sp JOIN sense s ON s.id = sp.sense_id
               WHERE sp.seq = e.seq AND sp.tag = 'misc' AND sp.text = 'uk' AND s.ord = 0
             ) AS prefer_kana_on_ordinal_zero,
             ARRAY(
               SELECT selected.text FROM (
                 SELECT DISTINCT sp1.text
                 FROM sense_prop sp1
                 LEFT JOIN sense_prop sp2 ON sp1.sense_id = sp2.sense_id
                   AND sp2.tag = 'misc' AND sp2.text IN ('arch', 'obsc', 'rare')
                 WHERE sp1.seq = e.seq AND sp1.tag = 'pos' AND sp2.id IS NULL
               ) selected ORDER BY selected.text COLLATE "C"
             ) AS pos
      FROM entry e WHERE e.seq = ANY(${collisionSeqs})
    )
    SELECT * FROM facts ORDER BY seq
  `;
  const entries = new Map(entryRows.map(row => [row.seq, row]));

  const direct = new Map<string, { target: number; via: number | null }>();
  const byIntermediate = new Map<number, CollisionPathRow[]>();
  for (const row of pathRows) {
    if (row.via === null) {
      artifact.rules.forEach((rule, ruleId) => {
        if (ruleMatches(rule, row)) {
          const key = JSON.stringify([row.rootSeq, row.collisionSeq, row.surface, ruleId, NONE]);
          direct.set(key, { target: row.collisionSeq, via: null });
        }
      });
    } else {
      let values = byIntermediate.get(row.via);
      if (!values) {
        values = [];
        byIntermediate.set(row.via, values);
      }
      values.push(row);
    }
  }

  if (byIntermediate.size > 0) {
    const viaSeqs = [...byIntermediate.keys()];
    const firstRows = await sql<CollisionPathRow[]>`
      SELECT c.seq AS collision_seq, c."from" AS root_seq, c.via,
             cp.pos, cp.conj_type, cp.neg AS negative, cp.fml AS formal,
             csr.source_text, csr.text AS surface
      FROM conjugation c
      JOIN conj_prop cp ON cp.conj_id = c.id
      JOIN conj_source_reading csr ON csr.conj_id = c.id
      WHERE c.seq = ANY(${viaSeqs})
      ORDER BY c."from", c.seq, c.id, cp.id, csr.source_text COLLATE "C", csr.text COLLATE "C"
    `;
    const firstByVia = new Map<number, CollisionPathRow[]>();
    for (const row of firstRows) {
      let values = firstByVia.get(row.collisionSeq);
      if (!values) {
        values = [];
        firstByVia.set(row.collisionSeq, values);
      }
      values.push(row);
    }
    for (const [via, finalRows] of byIntermediate) {
      for (const finalRow of finalRows) {
        for (const firstRow of firstByVia.get(via) ?? []) {
          if (firstRow.rootSeq !== finalRow.rootSeq || firstRow.surface !== finalRow.sourceText) continue;
          artifact.rules.forEach((firstRule, firstRuleId) => {
            if (!ruleMatches(firstRule, firstRow)) return;
            artifact.rules.forEach((secondRule, secondRuleId) => {
              if (!ruleMatches(secondRule, finalRow)) return;
              const key = JSON.stringify([
                finalRow.rootSeq, finalRow.collisionSeq, finalRow.surface, firstRuleId, secondRuleId
              ]);
              direct.set(key, { target: finalRow.collisionSeq, via });
            });
          });
        }
      }
    }
  }

  // Manual compatibility patches use an explicit rule and may not be
  // reproducible by applying that rule to the irregular source mapping.
  const pathByRootSurface = new Map<string, Array<{ target: number; via: number | null }>>();
  for (const row of pathRows) {
    const key = JSON.stringify([row.rootSeq, row.surface]);
    let values = pathByRootSurface.get(key);
    if (!values) {
      values = [];
      pathByRootSurface.set(key, values);
    }
    if (!values.some(value => value.target === row.collisionSeq && value.via === row.via)) {
      values.push({ target: row.collisionSeq, via: row.via });
    }
  }
  for (const patch of artifact.patches) {
    const targets = pathByRootSurface.get(JSON.stringify([patch.rootSeq, patch.surface]));
    if (!targets) continue;
    for (const target of targets) {
      direct.set(JSON.stringify([
        patch.rootSeq, target.target, patch.surface, patch.firstRule, patch.secondRule ?? NONE
      ]), target);
    }
  }

  const collisionRoots = new Set(pathRows.map(row => row.rootSeq));
  const emitted = enumerateMorphologyCandidates(artifact, collisionRoots);
  const output = new Map<string, AnalyzerSupportCollisionSource>();
  for (const candidate of emitted) {
    if (!candidate.ruleIds) continue;
    const first = candidate.ruleIds[0];
    const second = candidate.ruleIds[1] ?? NONE;
    const possible = pathRows.filter(row =>
      row.rootSeq === candidate.rootSeq && row.surface === candidate.surface);
    for (const row of possible) {
      const match = direct.get(JSON.stringify([
        candidate.rootSeq, row.collisionSeq, candidate.surface, first, second
      ]));
      if (match === undefined) continue;
      const target = match.target;
      const entry = entries.get(target);
      if (!entry) throw new AnalyzerSupportEncodingError(`Collision ${target} has no entry facts`);
      const value: AnalyzerSupportCollisionSource = {
        rootSeq: candidate.rootSeq,
        collisionSeq: target,
        viaSeq: match.via,
        route: candidate.route,
        surface: candidate.surface,
        ruleIds: candidate.ruleIds,
        nKanji: entry.nKanji,
        nKana: entry.nKana,
        primaryNokanji: entry.primaryNokanji,
        archived: entry.archived,
        preferKana: entry.preferKana,
        preferKanaOnOrdinalZero: entry.preferKanaOnOrdinalZero,
        pos: entry.pos ?? [],
        skipWord: target === runtime.upstream.UPSTREAM_260118_SKIP_WORD_ADDED
          || (target !== runtime.upstream.UPSTREAM_260118_SKIP_WORD_REMOVED
            && runtime.errata.SKIP_WORDS.includes(target)),
        finalParticle: runtime.errata.FINAL_PRT.includes(target),
        semiFinalParticle: runtime.errata.SEMI_FINAL_PRT.includes(target),
        nonFinalParticle: runtime.errata.NON_FINAL_PRT.includes(target),
        copula: runtime.errata.COPULAE.includes(target),
        noKanjiBreakPenalty: runtime.errata.NO_KANJI_BREAK_PENALTY.includes(target)
      };
      const key = analyzerSupportCollisionKey(value);
      const prior = output.get(key);
      if (prior && JSON.stringify(prior) !== JSON.stringify(value)) {
        throw new AnalyzerSupportEncodingError(`Conflicting collision facts for ${key}`);
      }
      output.set(key, value);
    }
  }
  return [...output.values()].sort((left, right) => compareText(analyzerSupportCollisionKey(left), analyzerSupportCollisionKey(right)));
}

function rawSuffixForm(form: KanaText): RawSuffixFormSource {
  return {
    seq: form.seq,
    text: form.text,
    bestKanji: form.bestKanji,
    commonTags: form.commonTags,
    ord: form.ord,
    common: form.common,
    conjugatable: form.conjugateP,
    nokanji: form.nokanji,
    conjugations: form.conjugations ?? null
  };
}

async function suffixSources(runtime: AnalyzerSupportOracleRuntime): Promise<{
  suffixes: RawSuffixSource[];
  suffixClasses: Array<{ seq: number; keyword: string }>;
}> {
  const cache = runtime.suffixCache.getSuffixCache();
  const classes = runtime.suffixCache.getSuffixClass();
  if (!cache || !classes) throw new AnalyzerSupportEncodingError('Suffix cache was not initialized');
  const suffixes = new Map<string, RawSuffixSource>();
  for (const [text, entry] of cache) {
    const rawValues = Array.isArray(entry[0]) ? entry as Array<[string, KanaText | null]> : [entry as [string, KanaText | null]];
    suffixes.set(text, {
      text,
      values: rawValues.map(([keyword, form]) => ({
        keyword,
        form: form === null ? null : rawSuffixForm(form)
      }))
    });
  }

  // These are compiler-owned overlays, equivalent to upstream's load-conjs
  // and load-abbr calls, without mutating the frozen reference suffix cache.
  const suffixClasses = new Map<number, string>(classes);
  for (const form of await runtime.upstream.loadUpstream260118GataiForms()) {
    suffixes.set(form.text, {
      text: form.text,
      values: [{ keyword: runtime.upstream.UPSTREAM_260118_GATAI_KEYWORD, form: rawSuffixForm(form) }]
    });
    suffixClasses.set(form.seq, runtime.upstream.UPSTREAM_260118_GATAI_CLASS);
  }
  suffixes.set(runtime.upstream.UPSTREAM_260118_NEBA_ABBREVIATION.text, {
    text: runtime.upstream.UPSTREAM_260118_NEBA_ABBREVIATION.text,
    values: [{ keyword: runtime.upstream.UPSTREAM_260118_NEBA_ABBREVIATION.keyword, form: null }]
  });

  return {
    suffixes: [...suffixes.values()],
    suffixClasses: [...suffixClasses].map(([seq, keyword]) => ({ seq, keyword }))
  };
}

async function hydrateSuffixConjugations(
  sql: postgres.Sql,
  suffixes: readonly RawSuffixSource[]
): Promise<AnalyzerSupportSuffixSource[]> {
  const forms = suffixes.flatMap(suffix => suffix.values.flatMap(value =>
    value.form === null ? [] : [value.form]));
  const seqs = [...new Set(forms
    .filter(form => form.conjugations !== ':root')
    .map(form => form.seq))];
  const rows = seqs.length === 0 ? [] : await sql<SuffixConjugationRow[]>`
    SELECT c.id AS conjugation_id, cp.id AS property_id,
           c.seq, c."from", c.via, cp.pos, cp.conj_type AS type,
           cp.neg AS negative, cp.fml AS formal, csr.text AS surface
    FROM conjugation c
    JOIN conj_prop cp ON cp.conj_id = c.id
    JOIN conj_source_reading csr ON csr.conj_id = c.id
    WHERE c.seq = ANY(${seqs})
    ORDER BY c.id, cp.id, csr.text COLLATE "C"
  `;
  const rowsByForm = new Map<string, SuffixConjugationRow[]>();
  const seen = new Map<string, Set<string>>();
  for (const row of rows) {
    const key = JSON.stringify([row.seq, row.surface]);
    const rowKey = `${row.conjugationId}\u0000${row.propertyId}`;
    let rowSeen = seen.get(key);
    if (!rowSeen) {
      rowSeen = new Set();
      seen.set(key, rowSeen);
    }
    if (rowSeen.has(rowKey)) continue;
    rowSeen.add(rowKey);
    const values = rowsByForm.get(key) ?? [];
    values.push(row);
    rowsByForm.set(key, values);
  }
  return suffixes.map(suffix => ({
    text: suffix.text,
    values: suffix.values.map(value => {
      const raw = value.form;
      if (raw === null) return { keyword: value.keyword, form: null };
      if (raw.conjugations === ':root') {
        return { keyword: value.keyword, form: { ...raw, conjugations: ':root' } };
      }
      const selectedIds = raw.conjugations && raw.conjugations.length > 0
        ? new Set(raw.conjugations)
        : null;
      const conjugations = (rowsByForm.get(JSON.stringify([raw.seq, raw.text])) ?? [])
        .filter(row => selectedIds === null || selectedIds.has(row.conjugationId))
        .map(({ conjugationId: _conjugationId, propertyId: _propertyId, surface: _surface, ...row }) => row);
      return {
        keyword: value.keyword,
        form: { ...raw, conjugations: conjugations.length === 0 ? null : conjugations }
      };
    })
  }));
}

async function counterSources(
  runtime: AnalyzerSupportOracleRuntime
): Promise<AnalyzerSupportCounterSource[]> {
  const cache = await runtime.counters.ensureCounterCache();
  const output: AnalyzerSupportCounterSource[] = [];
  for (const [key, variants] of cache) {
    for (let order = 0; order < variants.length; order++) {
      const [counterClass, options] = variants[order]!;
      const className = counterClass.name as AnalyzerSupportCounterClass;
      if (!ANALYZER_SUPPORT_COUNTER_CLASSES.includes(className)) {
        throw new AnalyzerSupportEncodingError(`Unsupported counter class ${counterClass.name}`);
      }
      const source = options.source ?? null;
      output.push({
        key,
        order,
        className,
        text: options.text,
        kana: options.kana,
        suffix: options.suffix ?? null,
        source: source === null ? null : {
          seq: source.seq,
          route: runtime.characters.testWord(source.text, 'kana') ? 'kana' : 'kanji',
          text: source.text,
          ord: source.ord
        },
        ordinal: options.ordinalp ?? false,
        foreign: options.foreign ?? false,
        common: options.common ?? null,
        suffixDescriptions: options.suffixDescriptions ?? [],
        digitOptions: (options.digitOpts ?? []).map(option => {
          const [digit, ...tokens] = option;
          if (digit !== ':off' && typeof digit !== 'number') {
            throw new AnalyzerSupportEncodingError(`Unsupported counter digit ${JSON.stringify(digit)}`);
          }
          return [digit, ...tokens] as readonly [number | ':off', ...string[]];
        }),
        digitSet: options.digitSet ?? [],
        allowed: options.allowed ?? []
      });
    }
  }
  return output;
}

async function loadDirectForms(sql: postgres.Sql, seqs: readonly number[]): Promise<DirectFormRow[]> {
  if (seqs.length === 0) return [];
  return sql<DirectFormRow[]>`
    SELECT * FROM (
      SELECT k.seq, 'kanji'::text AS route, k.text, k.ord, k.common,
             k.common_tags, k.conjugate_p AS conjugatable, k.nokanji, k.best_kana AS best
      FROM kanji_text k JOIN entry e USING (seq)
      WHERE e.root_p AND k.seq = ANY(${seqs})
      UNION ALL
      SELECT r.seq, 'kana'::text AS route, r.text, r.ord, r.common,
             r.common_tags, r.conjugate_p AS conjugatable, r.nokanji, r.best_kanji AS best
      FROM kana_text r JOIN entry e USING (seq)
      WHERE e.root_p AND r.seq = ANY(${seqs})
    ) forms
    ORDER BY seq, route COLLATE "C", text COLLATE "C", ord
  `;
}

function directCandidate(row: DirectFormRow): AnnotationCandidate {
  return {
    rootSeq: row.seq,
    route: row.route,
    surface: row.text,
    form: row.route === 'kanji' ? row.text : row.best ?? row.text,
    reading: row.route === 'kana' ? row.text : row.best ?? row.text,
    ord: row.ord,
    common: row.common,
    ruleIds: null
  };
}

function readingFor(candidate: AnnotationCandidate, definitionSeq: number): Reading {
  const common = candidate.common;
  if (candidate.route === 'kana') {
    return {
      id: 0,
      seq: definitionSeq,
      text: candidate.surface,
      ord: candidate.ord,
      common,
      commonTags: '',
      conjugateP: false,
      nokanji: false,
      bestKanji: candidate.form === candidate.reading ? null : candidate.form,
      hintedp: true
    };
  }
  return {
    id: 0,
    seq: definitionSeq,
    text: candidate.surface,
    ord: candidate.ord,
    common,
    commonTags: '',
    conjugateP: false,
    nokanji: false,
    bestKana: candidate.reading,
    hintedp: true
  };
}

function splitPartSource(
  part: unknown,
  runtime: AnalyzerSupportOracleRuntime
): AnalyzerSupportSplitPartSource {
  if (part === ':score' || part === ':pscore') return part;
  if (!part || typeof part !== 'object' || !('text' in part) || !('seq' in part)) {
    throw new AnalyzerSupportEncodingError(`Unsupported split part ${JSON.stringify(part)}`);
  }
  const word = part as {
    seq: number; text: string; ord: number; common: number | null; commonTags: string;
    conjugateP: boolean; nokanji: boolean; bestKana?: string | null; bestKanji?: string | null;
  };
  const route: AnalyzerSupportRoute = runtime.characters.testWord(word.text, 'kana') ? 'kana' : 'kanji';
  return {
    seq: word.seq,
    route,
    text: word.text,
    best: route === 'kana' ? word.bestKanji ?? null : word.bestKana ?? null,
    ord: word.ord,
    common: word.common,
    commonTags: word.commonTags,
    conjugatable: word.conjugateP,
    nokanji: word.nokanji,
    generated: null
  };
}

interface SplitConjugationRow extends AnalyzerSupportSplitConjugationSource {
  readonly seq: number;
  readonly viaSeq: number | null;
}

async function splitGeneratedLocators(
  sql: postgres.Sql,
  splits: readonly AnalyzerSupportSplitSource[]
): Promise<ReadonlyMap<number, readonly AnalyzerSupportSplitConjugationSource[]>> {
  const seqs = [...new Set(splits.flatMap(split => split.parts.flatMap(part =>
    typeof part === 'string' ? [] : [part.seq])))];
  if (seqs.length === 0) return new Map();
  const rows = await sql<SplitConjugationRow[]>`
    SELECT c.seq, c."from", c.via AS via_seq,
           cp.pos, cp.conj_type AS type,
           cp.neg AS negative, cp.fml AS formal
    FROM conjugation c
    JOIN conj_prop cp ON cp.conj_id = c.id
    WHERE c.seq = ANY(${seqs})
    ORDER BY c.seq, c."from", c.via NULLS FIRST,
             cp.pos COLLATE "C", cp.conj_type,
             cp.neg NULLS FIRST, cp.fml NULLS FIRST
  `;
  const output = new Map<number, AnalyzerSupportSplitConjugationSource[]>();
  const seen = new Map<number, Set<string>>();
  for (const row of rows) {
    const key = JSON.stringify([
      row.from, row.viaSeq !== null, row.pos, row.type, row.negative, row.formal
    ]);
    const seqSeen = seen.get(row.seq) ?? new Set<string>();
    if (seqSeen.has(key)) continue;
    seqSeen.add(key);
    seen.set(row.seq, seqSeen);
    const values = output.get(row.seq) ?? [];
    values.push({
      from: row.from,
      via: row.viaSeq !== null,
      pos: row.pos,
      type: row.type,
      negative: row.negative,
      formal: row.formal
    });
    output.set(row.seq, values);
  }
  return output;
}

async function annotationSources(
  sql: postgres.Sql,
  candidates: readonly AnnotationCandidate[],
  collisions: readonly AnalyzerSupportCollisionSource[],
  activeSplitMap: ReadonlyMap<number, AsyncSplitFunction>,
  activeSegsplitMap: ReadonlyMap<number, AsyncSplitFunction>,
  activeHintMap: ReadonlyMap<number, HintFunction>,
  runtime: AnalyzerSupportOracleRuntime
): Promise<{
  splits: AnalyzerSupportSplitSource[];
  hints: AnalyzerSupportHintSource[];
  issues: AnalyzerSupportCompileIssue[];
}> {
  const collisionMap = new Map(collisions.map(value => [analyzerSupportCollisionKey(value), value]));
  const splitOutput = new Map<string, AnalyzerSupportSplitSource>();
  const hintOutput = new Map<string, AnalyzerSupportHintSource>();
  const issueOutput = new Map<string, AnalyzerSupportCompileIssue>();

  const collisionFor = (candidate: AnnotationCandidate): AnalyzerSupportCollisionSource | null => {
    if (!candidate.ruleIds) return null;
    const key = analyzerSupportCollisionKey({
      rootSeq: candidate.rootSeq,
      ruleIds: candidate.ruleIds,
      route: candidate.route,
      surface: candidate.surface
    });
    return collisionMap.get(key) ?? null;
  };

  for (const candidate of candidates) {
    const collision = collisionFor(candidate);
    for (const [kind, map] of [
      ['split', activeSplitMap],
      ['segsplit', activeSegsplitMap]
    ] as const) {
      const definitionSeq = collision && map.has(collision.collisionSeq)
        ? collision.collisionSeq
        : map.has(candidate.rootSeq) ? candidate.rootSeq : null;
      if (definitionSeq === null) continue;
      const result = await map.get(definitionSeq)!(readingFor(candidate, definitionSeq));
      if (!result || result[0].some(part => part === null)) continue;
      const attrs = result[1];
      const value: AnalyzerSupportSplitSource = {
        definitionSeq,
        route: candidate.route,
        surface: candidate.surface,
        kind,
        parts: result[0].map(part => splitPartSource(part, runtime)),
        score: typeof attrs === 'number' ? attrs : attrs.score,
        primary: typeof attrs === 'number' ? 0 : attrs.primary ?? 0,
        connector: typeof attrs === 'number' ? ' ' : attrs.connector ?? ' ',
        root: typeof attrs === 'number' ? [] : attrs.root ?? []
      };
      const key = analyzerSupportSplitKey(value);
      const prior = splitOutput.get(key);
      if (prior && JSON.stringify(prior) !== JSON.stringify(value)) {
        throw new AnalyzerSupportEncodingError(`Split output depends on unkeyed state for ${key}`);
      }
      splitOutput.set(key, value);
    }

    const definitionSeq = collision && activeHintMap.has(collision.collisionSeq)
      ? collision.collisionSeq
      : activeHintMap.has(candidate.rootSeq) ? candidate.rootSeq : null;
    if (definitionSeq !== null) {
      let hint: string | null;
      try {
        hint = await activeHintMap.get(definitionSeq)!(readingFor(candidate, definitionSeq));
      } catch (error) {
        const issue: AnalyzerSupportCompileIssue = {
          kind: 'hint-runtime-error',
          definitionSeq,
          route: candidate.route,
          surface: candidate.surface,
          reading: candidate.reading,
          message: error instanceof Error ? error.message : String(error)
        };
        issueOutput.set(JSON.stringify(issue), issue);
        continue;
      }
      if (hint !== null) {
        const value: AnalyzerSupportHintSource = {
          definitionSeq,
          route: candidate.route,
          surface: candidate.surface,
          reading: candidate.reading,
          hint
        };
        const key = analyzerSupportHintKey(value);
        const prior = hintOutput.get(key);
        if (prior && prior.hint !== hint) {
          throw new AnalyzerSupportEncodingError(`Hint output depends on unkeyed state for ${key}`);
        }
        hintOutput.set(key, value);
      }
    }
  }
  const splits = [...splitOutput.values()];
  const generatedLocators = await splitGeneratedLocators(sql, splits);
  return {
    splits: splits.map(split => ({
      ...split,
      parts: split.parts.map(part => {
        if (typeof part === 'string') return part;
        const generated = generatedLocators.get(part.seq);
        return generated ? { ...part, generated } : part;
      })
    })),
    hints: [...hintOutput.values()],
    issues: [...issueOutput.values()]
  };
}

/**
 * Resolve every analyzer-only database/cache dependency into a pinned source.
 * The returned object is deliberately plain data and can be encoded without a
 * live core runtime.
 */
export async function loadAnalyzerSupportSource(sql: postgres.Sql): Promise<AnalyzerSupportSource> {
  const runtime = await loadAnalyzerSupportOracleRuntime();
  return runtime.connection.withConnectionOverride(sql, async () => {
    // Counter/no-conjugation/archive caches are connection-owned in the legacy
    // runtime. A release build must never reuse values from an earlier DB.
    runtime.connection.resetAllCaches();
    await runtime.suffixCache.initSuffixes({ blocking: true, reset: true });
    const [morphology, counters] = await Promise.all([
      compileMorphology({ sql }),
      counterSources(runtime)
    ]);
    const collisions = await loadCollisionSources(sql, morphology.artifact, runtime);
    const generated = await loadAnalyzerGeneratedSource(sql, morphology.artifact);
    const activeSplitMap = new Map<number, AsyncSplitFunction>(runtime.splitMaps.splitMap);
    for (const [seq, split] of runtime.upstream.upstream260118SplitMap) {
      activeSplitMap.set(seq, split);
    }
    const activeSegsplitMap = new Map<number, AsyncSplitFunction>(runtime.splitMaps.segsplitMap);
    const activeHintMap = new Map<number, HintFunction>(runtime.splitMaps.hintMap);
    for (const [seq, hint] of runtime.upstream.upstream260118HintMap) {
      activeHintMap.set(seq, hint);
    }
    const roots = new Set<number>([
      ...activeSplitMap.keys(), ...activeSegsplitMap.keys(), ...activeHintMap.keys(),
      ...collisions
        .filter(value =>
          activeSplitMap.has(value.collisionSeq)
          || activeSegsplitMap.has(value.collisionSeq)
          || activeHintMap.has(value.collisionSeq))
        .map(value => value.rootSeq)
    ]);
    const directForms = await loadDirectForms(sql, [...roots]);
    const candidates = new Map<string, AnnotationCandidate>();
    for (const row of directForms) {
      const value = directCandidate(row);
      candidates.set(annotationCandidateKey(value), value);
    }
    for (const value of enumerateMorphologyCandidates(morphology.artifact, roots)) {
      candidates.set(annotationCandidateKey(value), value);
    }
    const annotations = await annotationSources(
      sql,
      [...candidates.values()],
      collisions,
      activeSplitMap,
      activeSegsplitMap,
      activeHintMap,
      runtime
    );
    const suffix = await suffixSources(runtime);
    const suffixes = await hydrateSuffixConjugations(sql, suffix.suffixes);
    return {
      suffixes,
      suffixClasses: suffix.suffixClasses,
      counters,
      splits: annotations.splits,
      hints: annotations.hints,
      collisions,
      generated,
      issues: annotations.issues
    };
  });
}

// Kept local to the compiler so the browser reader remains Postgres-free.
export type AnalyzerSupportSql = postgres.Sql;
