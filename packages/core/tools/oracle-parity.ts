#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { execFile as execFileCallback } from 'node:child_process';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { promisify } from 'node:util';
import { gunzipSync } from 'node:zlib';
import postgres, { type Sql } from 'postgres';
import { withConnectionOverride } from '../../reference-postgres/src/conn.js';
import { resetCalcScoreCache } from '../../reference-postgres/src/dict/cache.js';
import {
  constructConjugation,
  getConjRules,
  getPosIndex
} from '../../data/src/data/conj-rules.js';
import {
  verifyBrowserAlphaDatabase
} from '../../data/src/browser-pack/database-identity.js';
import {
  sha256Bytes,
  verifyBrowserAlphaOracleCore,
  verifyBrowserAlphaSources,
  type BrowserAlphaSourceLock
} from '../../data/src/browser-pack/release-orchestration.js';
import {
  parseAnalyzerReleaseManifest,
  type AnalyzerReleaseAsset,
  type AnalyzerReleaseManifest
} from '../src/release-manifest.js';

import {
  ANALYZER_ANNOTATIONS_SECTION_ID,
  AnalyzerAnnotationNotLoadedError,
  AnalyzerAnnotationsReader,
  analyzerAnnotationsMemorySource
} from '../src/analyzer-annotations.js';
import {
  PortableAnalyzer,
  type PortableAnalysisResult,
  type PortableAnalyzeOptions
} from '../src/analyzer.js';
import { normalize as normalizePortable } from '../src/characters.js';
import { ANALYZER_SUPPORT_SECTION_ID, openAnalyzerSupport } from '../src/analyzer-support.js';
import { memoryDetailSource, openDetailStore } from '../src/details.js';
import { MORPHOLOGY_SECTION_ID, openMorphology } from '../src/morphology.js';
import { openPack } from '../src/pack.js';
import { ROOT_PAYLOAD_SECTION_ID, openRootPayload } from '../src/root-payload.js';
import { SURFACE_INDEX_SECTION_ID, openSurfaceIndex } from '../src/surface-index.js';
import {
  firstCanonicalDifference,
  legacyPathSkeleton,
  normalizeLegacyIdentities,
  projectCoreCleanAnalysis,
  projectPortableCleanAnalysis,
  type CanonicalDifference,
  type CleanAnalysisInflection,
  type CleanAnalysisRoot,
  type CoreResolvedWord,
  type CoreWordLike,
  type IdentityResolver,
  type IdentitySource
} from './parity-canonical.js';
import {
  compareDetailedAuthority,
  normalizeSegmentationExpectation,
  releaseGateFailureCount
} from './oracle-authority.js';
import {
  fixtureKey,
  loadAnalyzerParityCorpus,
  type AnalyzerParityCorpus,
  type AnalyzerEntityFixture,
  type AnalyzerFixtureRequest
} from './parity-corpus.js';

interface Options {
  readonly repository: string;
  readonly release: string;
  readonly database: string;
  readonly out: string | null;
  readonly smoke: boolean;
  readonly allowFailures: boolean;
  readonly samples: number;
}

const execFile = promisify(execFileCallback);

interface CoreReference {
  readonly core: {
    romanize(input: string, options: Record<string, unknown>): Promise<{
      readonly romanized: string;
    }>;
    romanizeStar(input: string, options: Record<string, unknown>): Promise<unknown>;
    basicSplit(input: string): readonly {
      readonly type: string;
      readonly text: string;
    }[];
    simpleSegment(input: string): Promise<readonly {
      readonly type: string;
      readonly text: string;
    }[]>;
    normalize(input: string, context?: 'kana', skipPunctuation?: boolean): string;
    transformRomanizeStarResult(input: unknown): Promise<unknown>;
  };
  readonly sql: Sql;
  readonly identity: PostgresIdentityResolver;
  withOracle<T>(fn: () => Promise<T>): Promise<T>;
  close(): Promise<void>;
}

interface DatabaseSpec {
  readonly database: string;
  readonly user: string;
  readonly password: string;
  readonly host: string;
  readonly port?: number;
  readonly ssl?: boolean;
}

function morphologyOrdinal(
  pos: string,
  type: number,
  negative: boolean | null,
  formal: boolean | null,
  source: string,
  target: string,
  manualCompatibility = false
): number {
  const posId = getPosIndex(pos);
  if (posId === undefined) throw new Error(`No conjugation-rule position for ${pos}`);
  const sameType = getConjRules(posId).filter(rule => rule.conj === type);
  const ignoresNegative = !sameType.some(rule => rule.neg);
  const ignoresFormal = !sameType.some(rule => rule.fml);
  const sameProperty = sameType.filter(rule =>
    (ignoresNegative ? null : rule.neg) === negative
    && (ignoresFormal ? null : rule.fml) === formal);
  const forward = sameProperty.filter(rule => constructConjugation(source, rule) === target);
  if (forward.length === 0 && !manualCompatibility) {
    throw new Error(
      `No forward pinned morphology rule for ${pos}/${type}/${String(negative)}/${String(formal)} ${source} -> ${target}`
    );
  }
  const candidates = forward.length > 0 ? forward : sameProperty;
  if (candidates.length === 0) throw new Error(`No manual compatibility property for ${pos}/${type}`);
  return Math.min(...candidates.map(rule => rule.onum));
}

type FailureClass = 'analyzer' | 'presentation' | 'error';

interface SuiteStats {
  total: number;
  exact: number;
  pathExact: number;
  analyzer: number;
  presentation: number;
  errors: number;
}

interface FailureSample {
  readonly suite: string;
  readonly request: string;
  readonly classification: FailureClass;
  readonly pathDifference?: DifferencePreview;
  readonly cleanDifference?: DifferencePreview;
  readonly detailedDifference?: DifferencePreview;
  readonly multipleRoots?: Readonly<Record<string, readonly number[]>>;
  readonly error?: string;
}

interface DifferencePreview {
  readonly path: string;
  readonly kind: CanonicalDifference['kind'];
  readonly expected: string;
  readonly actual: string;
}

interface HistoricalDifference {
  readonly request: string;
  readonly difference: DifferencePreview;
}

interface SuiteRun {
  readonly stats: SuiteStats;
  readonly referenceStats: SuiteStats;
  readonly cleanStats: SuiteStats;
  readonly referenceExact: number;
  readonly referenceFailed: number;
  readonly referenceDifferences: readonly HistoricalDifference[];
}

interface Runtime {
  readonly analyzer: PortableAnalyzer;
  readonly annotations: ReturnType<AnalyzerAnnotationsReader['createPreloaded']>;
  readonly details: Awaited<ReturnType<typeof openDetailStore>>;
}

function usage(message?: string): never {
  if (message) console.error(`error: ${message}\n`);
  console.error(`usage: bun packages/core/tools/oracle-parity.ts \\
  --release <directory> --database <url> [--repository <directory>] \\
  [--out <report.json>] [--smoke] [--allow-failures] [--samples <count>]

Without --smoke this always runs all 534 segmentation, 252 CLI, 149 hard,
200 counter, 54 entity, deterministic analyzer probes, and 5 standalone
romanization fixtures. Without
--allow-failures, any divergence
exits non-zero so the command is suitable as the exact release gate.`);
  process.exit(2);
}

function positiveInteger(value: string, label: string): number {
  if (!/^[1-9][0-9]*$/.test(value)) usage(`${label} must be a positive integer`);
  return Number(value);
}

function parseArgs(argv: readonly string[]): Options {
  let repository = process.cwd();
  let release: string | null = null;
  let database = process.env.ICHIRAN_DB_URL ?? '';
  let out: string | null = null;
  let smoke = false;
  let allowFailures = false;
  let samples = 30;
  for (let index = 0; index < argv.length; index++) {
    const argument = argv[index]!;
    const next = (): string => {
      const value = argv[++index];
      if (!value) usage(`${argument} requires a value`);
      return value;
    };
    if (argument === '--repository') repository = next();
    else if (argument === '--release') release = next();
    else if (argument === '--database') database = next();
    else if (argument === '--out') out = next();
    else if (argument === '--smoke') smoke = true;
    else if (argument === '--allow-failures') allowFailures = true;
    else if (argument === '--samples') samples = positiveInteger(next(), '--samples');
    else if (argument === '--help' || argument === '-h') usage();
    else usage(`unknown argument ${argument}`);
  }
  if (!release) usage('--release is required');
  if (!database) usage('--database or ICHIRAN_DB_URL is required');
  return {
    repository: resolve(repository),
    release: resolve(release),
    database,
    out: out ? resolve(out) : null,
    smoke,
    allowFailures,
    samples
  };
}

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function decodeGzip(bytes: Uint8Array, expectedBytes: number): Promise<Uint8Array> {
  const decoded = new Uint8Array(gunzipSync(bytes));
  if (decoded.byteLength !== expectedBytes) {
    throw new Error(`Decoded ${decoded.byteLength} bytes; expected ${expectedBytes}`);
  }
  return Promise.resolve(decoded);
}

async function releaseAsset(directory: string, asset: AnalyzerReleaseAsset): Promise<Uint8Array> {
  const download = new Uint8Array(await readFile(resolve(directory, asset.file)));
  if (download.byteLength !== asset.downloadBytes || sha256(download) !== asset.downloadSha256) {
    throw new Error(`${asset.file} download length or digest does not match its manifest`);
  }
  const installed = asset.encoding === 'gzip'
    ? new Uint8Array(gunzipSync(download))
    : download.slice();
  if (installed.byteLength !== asset.installedBytes || sha256(installed) !== asset.installedSha256) {
    throw new Error(`${asset.file} installed length or digest does not match its manifest`);
  }
  return installed;
}

async function openRuntime(
  directory: string,
  expected: { readonly sourceCommit: string; readonly sourcesLockSha256: string }
): Promise<Runtime> {
  const manifest = JSON.parse(
    await readFile(resolve(directory, 'manifest.json'), 'utf8')
  );
  const verifiedManifest: AnalyzerReleaseManifest = parseAnalyzerReleaseManifest(
    manifest,
    text => createHash('sha256').update(text).digest('hex')
  );
  if (verifiedManifest.sourceCommit !== expected.sourceCommit) {
    throw new Error(
      `Release source commit ${verifiedManifest.sourceCommit} does not match repository HEAD ${expected.sourceCommit}`
    );
  }
  if (verifiedManifest.sourcesLockSha256 !== expected.sourcesLockSha256) {
    throw new Error('Release sources-lock digest does not match the verified repository lock');
  }
  const [hot, detailsBytes] = await Promise.all([
    releaseAsset(directory, verifiedManifest.hot),
    releaseAsset(directory, verifiedManifest.details)
  ]);
  const pack = openPack(hot);
  pack.verifyAll();
  const annotationsStore = await AnalyzerAnnotationsReader.open(
    analyzerAnnotationsMemorySource(pack.getSection(ANALYZER_ANNOTATIONS_SECTION_ID)),
    decodeGzip
  );
  const annotations = annotationsStore.createPreloaded();
  const surface = openSurfaceIndex(pack.getSection(SURFACE_INDEX_SECTION_ID));
  const roots = openRootPayload(pack.getSection(ROOT_PAYLOAD_SECTION_ID));
  const analyzer = new PortableAnalyzer({
    surface,
    roots,
    morphology: openMorphology(pack.getSection(MORPHOLOGY_SECTION_ID)),
    support: openAnalyzerSupport(pack.getSection(ANALYZER_SUPPORT_SECTION_ID)),
    annotations
  });
  const details = await openDetailStore(memoryDetailSource(detailsBytes), decodeGzip);
  return { analyzer, annotations, details };
}

function parseDatabase(value: string): DatabaseSpec {
  const normalized = value.replace(/^postgresql:\/\//, 'postgres://');
  const url = new URL(normalized);
  const database = decodeURIComponent(url.pathname.replace(/^\//, ''));
  if (!database) throw new Error('Database URL has no database name');
  const hostParameter = url.searchParams.get('host');
  const host = hostParameter ? decodeURIComponent(hostParameter) : (url.hostname || 'localhost');
  const portText = url.port || url.searchParams.get('port');
  const sslMode = url.searchParams.get('sslmode');
  return {
    database,
    host,
    user: url.username ? decodeURIComponent(url.username) : '',
    password: url.password ? decodeURIComponent(url.password) : '',
    port: portText ? Number(portText) : undefined,
    ssl: sslMode ? sslMode !== 'disable' : undefined
  };
}

class PostgresIdentityResolver implements IdentityResolver {
  readonly #sql: Sql;
  readonly #cache = new Map<string, Promise<readonly number[]>>();
  readonly #wordCache = new Map<string, Promise<CoreResolvedWord>>();

  constructor(sql: Sql) {
    this.#sql = sql;
  }

  roots(
    seq: number,
    surface?: string,
    sources: readonly IdentitySource[] = []
  ): Promise<readonly number[]> {
    const key = JSON.stringify([seq, surface ?? null, sources]);
    let cached = this.#cache.get(key);
    if (!cached) {
      cached = this.#load(seq, surface, sources);
      this.#cache.set(key, cached);
    }
    return cached;
  }

  resolveWord(word: CoreWordLike): Promise<CoreResolvedWord> {
    const key = JSON.stringify([
      word.type, word.text, word.trueText ?? null, word.seq ?? null,
      word.conjugations ?? null, word.kana
    ]);
    let cached = this.#wordCache.get(key);
    if (!cached) {
      cached = this.#resolveWord(word);
      this.#wordCache.set(key, cached);
    }
    return cached;
  }

  async #resolveWord(word: CoreWordLike): Promise<CoreResolvedWord> {
    if (typeof word.seq !== 'number' || word.type.toLowerCase() === 'gap') {
      return { root: null, inflection: [] };
    }
    const seq = word.seq;
    const route = word.type.toLowerCase() === 'kana' ? 'kana' : 'kanji';
    const surface = word.trueText ?? word.text;
    const [entry] = await this.#sql<{ rootP: boolean }[]>`
      SELECT root_p AS "rootP" FROM entry WHERE seq = ${seq}
    `;
    if (word.conjugations === ':root' || entry?.rootP) {
      return { root: await this.#directRoot(seq, route, surface, word), inflection: [] };
    }
    const ids = Array.isArray(word.conjugations)
      ? word.conjugations.filter((value): value is number => typeof value === 'number')
      : [];
    const selectedIds = ids.length > 0
      ? this.#sql`AND final.id IN ${this.#sql(ids)}`
      : this.#sql``;
    interface GeneratedRow {
      readonly rootSeq: number;
      readonly sourceText: string;
      readonly sourceForm: string;
      readonly sourceReading: string;
      readonly intermediate: string;
      readonly prefixPos: string | null;
      readonly prefixType: number | null;
      readonly prefixNegative: boolean | null;
      readonly prefixFormal: boolean | null;
      readonly finalPos: string;
      readonly finalType: number;
      readonly finalNegative: boolean | null;
      readonly finalFormal: boolean | null;
    }
    // Physical member and property IDs own the exact ordering used by the
    // pinned generated projection. Rule ordinal is intentionally unavailable.
    const rows = await this.#sql<GeneratedRow[]>`
      WITH matched AS (
        SELECT final.id AS final_id, final."from" AS root_seq, final.via,
               final_source.source_text AS intermediate,
               final_prop.id AS final_prop_id,
               final_prop.pos AS final_pos,
               final_prop.conj_type AS final_type,
               final_prop.neg AS final_negative,
               final_prop.fml AS final_formal
        FROM conjugation final
        JOIN conj_source_reading final_source ON final_source.conj_id = final.id
          AND final_source.text = ${surface}
        JOIN conj_prop final_prop ON final_prop.conj_id = final.id
        WHERE final.seq = ${seq} ${selectedIds}
      ), relation AS (
        SELECT matched.root_seq, source.text AS source_text,
               matched.intermediate AS intermediate,
               source.text AS source_form,
               COALESCE(source.best_kana, source.text) AS source_reading,
               source.ord AS source_ord, source.common AS source_common,
               NULL::integer AS prefix_id, NULL::integer AS prefix_prop_id,
               NULL::text AS prefix_pos, NULL::integer AS prefix_type,
               NULL::boolean AS prefix_negative, NULL::boolean AS prefix_formal,
               matched.final_id, matched.final_prop_id, matched.final_pos,
               matched.final_type, matched.final_negative, matched.final_formal
        FROM matched
        JOIN kanji_text source ON ${route} = 'kanji'
          AND source.seq = matched.root_seq AND source.text = matched.intermediate
        WHERE matched.via IS NULL

        UNION ALL
        SELECT matched.root_seq, source.text, matched.intermediate,
               source.text,
               COALESCE(source.best_kana, source.text),
               source.ord, source.common,
               prefix.id, prefix_prop.id, prefix_prop.pos, prefix_prop.conj_type,
               prefix_prop.neg, prefix_prop.fml,
               matched.final_id, matched.final_prop_id, matched.final_pos,
               matched.final_type, matched.final_negative, matched.final_formal
        FROM matched
        JOIN conjugation prefix ON prefix.seq = matched.via
          AND prefix."from" = matched.root_seq AND prefix.via IS NULL
        JOIN conj_source_reading prefix_source ON prefix_source.conj_id = prefix.id
          AND prefix_source.text = matched.intermediate
        JOIN conj_prop prefix_prop ON prefix_prop.conj_id = prefix.id
        JOIN kanji_text source ON ${route} = 'kanji'
          AND source.seq = matched.root_seq AND source.text = prefix_source.source_text
        WHERE matched.via IS NOT NULL

        UNION ALL
        SELECT matched.root_seq, source.text, matched.intermediate,
               COALESCE(source.best_kanji, source.text),
               source.text,
               source.ord, source.common,
               NULL::integer, NULL::integer, NULL::text, NULL::integer,
               NULL::boolean, NULL::boolean,
               matched.final_id, matched.final_prop_id, matched.final_pos,
               matched.final_type, matched.final_negative, matched.final_formal
        FROM matched
        JOIN kana_text source ON ${route} = 'kana'
          AND source.seq = matched.root_seq AND source.text = matched.intermediate
        WHERE matched.via IS NULL

        UNION ALL
        SELECT matched.root_seq, source.text, matched.intermediate,
               COALESCE(source.best_kanji, source.text),
               source.text,
               source.ord, source.common,
               prefix.id, prefix_prop.id, prefix_prop.pos, prefix_prop.conj_type,
               prefix_prop.neg, prefix_prop.fml,
               matched.final_id, matched.final_prop_id, matched.final_pos,
               matched.final_type, matched.final_negative, matched.final_formal
        FROM matched
        JOIN conjugation prefix ON prefix.seq = matched.via
          AND prefix."from" = matched.root_seq AND prefix.via IS NULL
        JOIN conj_source_reading prefix_source ON prefix_source.conj_id = prefix.id
          AND prefix_source.text = matched.intermediate
        JOIN conj_prop prefix_prop ON prefix_prop.conj_id = prefix.id
        JOIN kana_text source ON ${route} = 'kana'
          AND source.seq = matched.root_seq AND source.text = prefix_source.source_text
        WHERE matched.via IS NOT NULL
      )
      SELECT root_seq AS "rootSeq", source_text AS "sourceText",
             source_form AS "sourceForm",
             source_reading AS "sourceReading", prefix_pos AS "prefixPos",
             prefix_type AS "prefixType", prefix_negative AS "prefixNegative",
             prefix_formal AS "prefixFormal", final_pos AS "finalPos",
             final_type AS "finalType", final_negative AS "finalNegative",
             final_formal AS "finalFormal", intermediate
      FROM relation
      ORDER BY final_id, final_prop_id, prefix_id NULLS FIRST,
               prefix_prop_id NULLS FIRST, source_ord, source_common NULLS LAST,
               source_form COLLATE "C", source_reading COLLATE "C",
               source_text COLLATE "C"
    `;
    const row = rows[0];
    if (!row) {
      return { root: await this.#directRoot(seq, route, surface, word), inflection: [] };
    }
    const inflection: CleanAnalysisInflection[] = [];
    if (row.prefixPos !== null && row.prefixType !== null) {
      inflection.push({
        pos: row.prefixPos,
        type: row.prefixType,
        negative: row.prefixNegative,
        formal: row.prefixFormal,
        ordinal: morphologyOrdinal(
          row.prefixPos,
          row.prefixType,
          row.prefixNegative,
          row.prefixFormal,
          row.sourceText,
          row.intermediate
        )
      });
    }
    inflection.push({
      pos: row.finalPos,
      type: row.finalType,
      negative: row.finalNegative,
      formal: row.finalFormal,
      ordinal: morphologyOrdinal(
        row.finalPos,
        row.finalType,
        row.finalNegative,
        row.finalFormal,
        row.prefixPos === null ? row.sourceText : row.intermediate,
        surface,
        row.prefixPos === null && (
          (row.rootSeq === 2089020 && surface.startsWith('じゃ'))
          || ([1612690, 2253080].includes(row.rootSeq) && row.finalPos === 'exp')
        )
      )
    });
    return {
      root: { seq: row.rootSeq, form: row.sourceForm, reading: row.sourceReading },
      inflection
    };
  }

  async #directRoot(
    seq: number,
    route: 'kanji' | 'kana',
    surface: string,
    word: CoreWordLike
  ): Promise<CleanAnalysisRoot> {
    const [row] = await this.#sql<{ form: string; reading: string }[]>`
      SELECT text AS form, COALESCE(best_kana, text) AS reading
      FROM kanji_text WHERE ${route} = 'kanji' AND seq = ${seq} AND text = ${surface}
      UNION ALL
      SELECT COALESCE(best_kanji, text) AS form, text AS reading
      FROM kana_text WHERE ${route} = 'kana' AND seq = ${seq} AND text = ${surface}
      LIMIT 1
    `;
    const fallbackReading = Array.isArray(word.kana) ? word.kana[0] ?? surface : word.kana;
    return {
      seq,
      form: row?.form ?? (route === 'kanji' ? surface : fallbackReading),
      reading: row?.reading ?? fallbackReading
    };
  }

  async #load(
    seq: number,
    surface?: string,
    sources: readonly IdentitySource[] = []
  ): Promise<readonly number[]> {
    const [entry] = await this.#sql<{ rootP: boolean }[]>`
      SELECT root_p AS "rootP" FROM entry WHERE seq = ${seq}
    `;
    if (entry?.rootP) return [seq];

    // Follow only conjugation rows that can produce this exact displayed
    // surface. For a two-stage path, the intermediate row must retain the
    // outer row's semantic root. A blind target graph overstates identities
    // (for example it invents a third root for やらせられ).
    const rows = surface === undefined ? [] : await this.#sql<{ seq: number }[]>`
      WITH RECURSIVE lineage(target_seq, surface, required_root, path) AS (
        SELECT ${seq}::integer, ${surface}::text, NULL::integer,
               ARRAY[${seq}::integer]
        UNION ALL
        SELECT c.via, csr.source_text, c."from",
               lineage.path || c.via
        FROM lineage
        JOIN conjugation c ON c.seq = lineage.target_seq
          AND (lineage.required_root IS NULL OR c."from" = lineage.required_root)
          AND c.via IS NOT NULL
        JOIN conj_source_reading csr ON csr.conj_id = c.id
          AND csr.text = lineage.surface
        WHERE NOT c.via = ANY(lineage.path)
      )
      SELECT DISTINCT c."from" AS seq
      FROM lineage
      JOIN conjugation c ON c.seq = lineage.target_seq
        AND (lineage.required_root IS NULL OR c."from" = lineage.required_root)
        AND c.via IS NULL
      JOIN conj_source_reading csr ON csr.conj_id = c.id
        AND csr.text = lineage.surface
      JOIN entry root ON root.seq = c."from" AND root.root_p
      WHERE EXISTS (
        SELECT 1 FROM kana_text k
        WHERE k.seq = c."from" AND k.text = csr.source_text
        UNION ALL
        SELECT 1 FROM kanji_text k
        WHERE k.seq = c."from" AND k.text = csr.source_text
      )
      ORDER BY c."from"
    `;
    if (rows.length > 0) return rows.map(row => row.seq);
    if (sources.length === 0) return [seq];

    // Some legacy suffix rewrites retain the generated target identity while
    // changing the displayed surface (for example おもわない -> おもわざる).
    // The detailed conjugation still exposes its lexical source reading. Use
    // that semantic lineage to resolve the root without making the generated
    // target seq part of the accepted projection.
    interface SourceRow {
      readonly seq: number;
      readonly route: 'kanji' | 'kana';
      readonly text: string;
    }
    const sourceRows = await this.#sql<SourceRow[]>`
      WITH candidates AS (
        SELECT DISTINCT c."from" AS seq
        FROM conjugation c
        JOIN entry root ON root.seq = c."from" AND root.root_p
        WHERE c.seq = ${seq}
      ), sources AS (
        SELECT candidates.seq, 'kanji'::text AS route, k.text
        FROM candidates JOIN kanji_text k ON k.seq = candidates.seq
        UNION ALL
        SELECT candidates.seq, 'kana'::text AS route, k.text
        FROM candidates JOIN kana_text k ON k.seq = candidates.seq
      )
      SELECT seq, route, text FROM sources
      ORDER BY seq, route, text COLLATE "C"
    `;
    const forms = new Map<number, Set<string>>();
    const readings = new Map<number, Set<string>>();
    for (const row of sourceRows) {
      const target = row.route === 'kanji' ? forms : readings;
      let texts = target.get(row.seq);
      if (!texts) {
        texts = new Set<string>();
        target.set(row.seq, texts);
      }
      texts.add(row.text);
    }
    const roots = [...new Set(sourceRows.map(row => row.seq))].filter(rootSeq =>
      sources.some(source => {
        const form = source.form;
        const reading = source.reading;
        if (form !== null && reading !== null && form !== reading) {
          return forms.get(rootSeq)?.has(form) === true
            && readings.get(rootSeq)?.has(reading) === true;
        }
        const text = form ?? reading;
        return text !== null && (
          forms.get(rootSeq)?.has(text) === true
          || readings.get(rootSeq)?.has(text) === true
        );
      })
    ).sort((left, right) => left - right);
    return roots.length > 0 ? roots : [seq];
  }
}

async function openReference(
  database: string,
  expected: BrowserAlphaSourceLock['database']
): Promise<CoreReference> {
  const spec = parseDatabase(database);
  const core = await import('../../reference-postgres/src/index.ts') as unknown as CoreReference['core'];
  const pool = postgres({
    host: spec.host,
    port: spec.port ?? 5432,
    database: spec.database,
    user: spec.user,
    password: spec.password,
    ssl: spec.ssl ? 'require' : false,
    max: 1,
    prepare: false,
    transform: postgres.camel,
    idle_timeout: 5
  });
  const sql = await pool.reserve();
  try {
    await sql.unsafe('BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY');
    await verifyBrowserAlphaDatabase(sql, database, expected);
  } catch (error) {
    sql.release();
    await pool.end();
    throw error;
  }
  return {
    core,
    sql,
    identity: new PostgresIdentityResolver(sql),
    withOracle: fn => withConnectionOverride(sql, fn),
    async close(): Promise<void> {
      try {
        await sql.unsafe('ROLLBACK');
      } finally {
        sql.release();
        await pool.end();
      }
    }
  };
}

async function repositoryHead(repository: string): Promise<string> {
  const { stdout } = await execFile('git', ['-C', repository, 'rev-parse', 'HEAD'], {
    encoding: 'utf8'
  });
  const commit = stdout.trim();
  if (!/^[0-9a-f]{40}$/.test(commit)) throw new Error('Repository HEAD is not a full Git object ID');
  return commit;
}

interface ReferenceAnalysis {
  readonly raw: unknown;
  readonly detailed: unknown;
  readonly normalized: string;
  readonly segments: readonly { readonly type: string; readonly text: string }[];
}

interface PortableAnalysis {
  readonly result: PortableAnalysisResult;
  readonly detailed: unknown;
}

async function referenceAnalysis(
  reference: CoreReference,
  request: AnalyzerFixtureRequest,
  entities?: AnalyzerEntityFixture['entities']
): Promise<ReferenceAnalysis> {
  // The legacy data cache retains text-filtered conjugation rows across calls.
  // Reset request-scoped score/conjugation data so corpus order cannot change
  // the supposedly fresh oracle (observed on させてもらえなかったりしてこまりきる).
  resetCalcScoreCache();
  const raw = await reference.core.romanizeStar(request.text, {
    limit: request.limit,
    normalizePunctuation: request.normalizePunctuation ?? true,
    entities: entities ? [...entities] : undefined
  });
  const normalized = reference.core.normalize(
    request.text,
    undefined,
    !(request.normalizePunctuation ?? true)
  );
  return {
    raw,
    detailed: await reference.core.transformRomanizeStarResult(raw),
    normalized,
    segments: reference.core.basicSplit(normalized)
  };
}

async function portableAnalysis(
  runtime: Runtime,
  request: AnalyzerFixtureRequest,
  options: PortableAnalyzeOptions = {}
): Promise<PortableAnalysis> {
  const loaded = new Set<string>();
  try {
    for (;;) {
      try {
        const result = runtime.analyzer.analyze(request.text, {
          limit: request.limit,
          normalizePunctuation: request.normalizePunctuation ?? true,
          ...options
        });
        return {
          result,
          detailed: await runtime.analyzer.serializeLegacyDetailed(result, runtime.details)
        };
      } catch (error) {
        if (!(error instanceof AnalyzerAnnotationNotLoadedError)) throw error;
        const missing = `${error.kind}:${error.definitionSeq}`;
        if (loaded.has(missing)) {
          throw new Error(`${missing} was still missing after preload`);
        }
        loaded.add(missing);
        await runtime.annotations.preloadMissing(error);
      }
    }
  } finally {
    runtime.annotations.clear();
  }
}

function emptyStats(): SuiteStats {
  return {
    total: 0,
    exact: 0,
    pathExact: 0,
    analyzer: 0,
    presentation: 0,
    errors: 0
  };
}

function recordDetailedComparison(
  stats: SuiteStats,
  comparison: {
    readonly pathDifference: CanonicalDifference | null;
    readonly detailedDifference: CanonicalDifference | null;
  }
): void {
  if (!comparison.pathDifference) stats.pathExact++;
  if (!comparison.detailedDifference) stats.exact++;
  else if (comparison.pathDifference) stats.analyzer++;
  else stats.presentation++;
}

function recordCleanComparison(stats: SuiteStats, difference: CanonicalDifference | null): void {
  if (!difference) {
    stats.exact++;
    stats.pathExact++;
  } else {
    stats.analyzer++;
  }
}

function preview(value: unknown): string {
  const text = JSON.stringify(value);
  if (text === undefined) return 'undefined';
  return text.length <= 600 ? text : `${text.slice(0, 600)}…`;
}

function differencePreview(value: CanonicalDifference | null): DifferencePreview | undefined {
  return value ? {
    path: value.path,
    kind: value.kind,
    expected: preview(value.expected),
    actual: preview(value.actual)
  } : undefined;
}

function select<T>(values: readonly T[], smoke: boolean, count: number): readonly T[] {
  return smoke ? values.slice(0, count) : values;
}

function progress(label: string, index: number, total: number): void {
  if ((index + 1) % 25 === 0 || index + 1 === total) {
    console.error(`${label}: ${index + 1}/${total}`);
  }
}

async function compareSuite(
  suite: string,
  cases: readonly { readonly request: AnalyzerFixtureRequest; readonly entities?: AnalyzerEntityFixture['entities']; readonly currentLisp?: string }[],
  runtime: Runtime,
  reference: CoreReference,
  samples: FailureSample[],
  maxSamples: number
): Promise<SuiteRun> {
  const stats = emptyStats();
  const referenceStats = emptyStats();
  const cleanStats = emptyStats();
  let referenceExact = 0;
  let referenceFailed = 0;
  const referenceDifferences: HistoricalDifference[] = [];
  for (let index = 0; index < cases.length; index++) {
    const fixture = cases[index]!;
    stats.total++;
    referenceStats.total++;
    cleanStats.total++;
    let actual: Awaited<ReturnType<typeof portableAnalysis>>;
    let currentLisp: unknown | null = null;
    try {
      currentLisp = fixture.currentLisp === undefined
        ? null
        : JSON.parse(fixture.currentLisp) as unknown;
      actual = await portableAnalysis(runtime, fixture.request, {
        entities: fixture.entities ? [...fixture.entities] : undefined
      });
      if (currentLisp !== null) {
        const authority = compareDetailedAuthority(currentLisp, null, actual.detailed);
        recordDetailedComparison(stats, authority);
        if (authority.detailedDifference && samples.length < maxSamples) {
          samples.push({
            suite,
            request: fixtureKey(fixture.request),
            classification: authority.pathDifference ? 'analyzer' : 'presentation',
            pathDifference: differencePreview(authority.pathDifference),
            detailedDifference: differencePreview(authority.detailedDifference)
          });
        }
      }
    } catch (error) {
      stats.errors++;
      referenceStats.errors++;
      cleanStats.errors++;
      if (samples.length < maxSamples) {
        samples.push({
          suite,
          request: fixtureKey(fixture.request),
          classification: 'error',
          error: error instanceof Error ? `${error.name}: ${error.message}` : String(error)
        });
      }
      progress(suite, index, cases.length);
      continue;
    }

    try {
      const expected = await referenceAnalysis(reference, fixture.request, fixture.entities);
      const expectedIdentity = await normalizeLegacyIdentities(expected.detailed, reference.identity);
      const expectedClean = await projectCoreCleanAnalysis({
        input: fixture.request.text,
        normalized: expected.normalized,
        limit: fixture.request.limit,
        segments: expected.segments,
        raw: expected.raw,
        resolveWord: word => reference.identity.resolveWord(word)
      });
      const actualClean = projectPortableCleanAnalysis(actual.result);
      const cleanDifference = firstCanonicalDifference(expectedClean, actualClean);
      const referencePathDifference = firstCanonicalDifference(
        legacyPathSkeleton(expectedIdentity.value),
        legacyPathSkeleton(actual.detailed)
      );
      const referenceDetailedDifference = firstCanonicalDifference(
        expectedIdentity.value,
        actual.detailed
      );
      recordCleanComparison(cleanStats, cleanDifference);
      recordDetailedComparison(referenceStats, {
        pathDifference: referencePathDifference,
        detailedDifference: referenceDetailedDifference
      });
      if (currentLisp !== null) {
        const difference = firstCanonicalDifference(currentLisp, expectedIdentity.value);
        if (difference) {
          referenceFailed++;
          referenceDifferences.push({
            request: fixture.request.text,
            difference: differencePreview(difference)!
          });
        } else referenceExact++;
      } else {
        const authority = compareDetailedAuthority(null, expectedIdentity.value, actual.detailed);
        recordDetailedComparison(stats, authority);
        if ((cleanDifference || authority.detailedDifference) && samples.length < maxSamples) {
          const classification: FailureClass = cleanDifference || authority.pathDifference
            ? 'analyzer'
            : 'presentation';
          samples.push({
            suite,
            request: fixtureKey(fixture.request),
            classification,
            pathDifference: differencePreview(authority.pathDifference),
            cleanDifference: differencePreview(cleanDifference),
            detailedDifference: differencePreview(authority.detailedDifference),
            multipleRoots: Object.keys(expectedIdentity.multipleRoots).length > 0
              ? expectedIdentity.multipleRoots
              : undefined
          });
        }
      }
    } catch (error) {
      referenceStats.errors++;
      cleanStats.errors++;
      if (currentLisp === null) {
        stats.errors++;
      }
      if (currentLisp === null && samples.length < maxSamples) {
        samples.push({
          suite,
          request: fixtureKey(fixture.request),
          classification: 'error',
          error: error instanceof Error ? `${error.name}: ${error.message}` : String(error)
        });
      }
    }
    progress(suite, index, cases.length);
  }
  return {
    stats,
    referenceStats,
    cleanStats,
    referenceExact,
    referenceFailed,
    referenceDifferences
  };
}

async function segmentationSuite(
  fixtures: readonly { readonly input: string; readonly expected: readonly string[] }[],
  runtime: Runtime,
  reference: CoreReference,
  samples: FailureSample[],
  maxSamples: number
): Promise<SuiteRun> {
  const stats = emptyStats();
  const referenceStats = emptyStats();
  const cleanStats = emptyStats();
  let referenceExact = 0;
  let referenceFailed = 0;
  const referenceDifferences: HistoricalDifference[] = [];
  for (let index = 0; index < fixtures.length; index++) {
    const fixture = fixtures[index]!;
    stats.total++;
    referenceStats.total++;
    const normalized = normalizePortable(fixture.input, undefined, true);
    const expected = normalizeSegmentationExpectation(
      fixture.expected,
      value => normalizePortable(value, undefined, true)
    );
    let actual: string[] | null = null;
    try {
      let result;
      const loaded = new Set<string>();
      for (;;) {
        try {
          result = runtime.analyzer.analyze(fixture.input, { limit: 1 });
          break;
        } catch (error) {
          if (!(error instanceof AnalyzerAnnotationNotLoadedError)) throw error;
          const missing = `${error.kind}:${error.definitionSeq}`;
          if (loaded.has(missing)) throw error;
          loaded.add(missing);
          await runtime.annotations.preloadMissing(error);
        }
      }
      actual = result.paths[0]?.tokens
        .map(token => token.route === 'gap' ? ':gap' : token.text) ?? [];
      const difference = firstCanonicalDifference(expected, actual);
      if (!difference) {
        stats.exact++;
        stats.pathExact++;
      } else {
        stats.analyzer++;
        if (samples.length < maxSamples) {
          samples.push({
            suite: 'segmentation',
            request: fixture.input,
            classification: 'analyzer',
            pathDifference: differencePreview(difference),
            detailedDifference: differencePreview(difference)
          });
        }
      }
    } catch (error) {
      stats.errors++;
      referenceStats.errors++;
      if (samples.length < maxSamples) {
        samples.push({
          suite: 'segmentation',
          request: fixture.input,
          classification: 'error',
          error: error instanceof Error ? `${error.name}: ${error.message}` : String(error)
        });
      }
    } finally {
      runtime.annotations.clear();
    }
    if (actual !== null) {
      try {
        resetCalcScoreCache();
        const current = (await reference.core.simpleSegment(normalized))
          .map(word => word.type.toLowerCase() === 'gap' ? ':gap' : word.text);
        const referenceDifference = firstCanonicalDifference(expected, current);
        if (referenceDifference) {
          referenceFailed++;
          referenceDifferences.push({
            request: fixture.input,
            difference: differencePreview(referenceDifference)!
          });
        } else referenceExact++;
        recordCleanComparison(
          referenceStats,
          firstCanonicalDifference(current, actual)
        );
      } catch {
        referenceStats.errors++;
      }
    }
    progress('segmentation', index, fixtures.length);
  }
  return {
    stats,
    referenceStats,
    cleanStats,
    referenceExact,
    referenceFailed,
    referenceDifferences
  };
}

async function romanizationSuite(
  inputs: readonly string[],
  currentLispOutputs: Readonly<Record<string, string>>,
  runtime: Runtime,
  reference: CoreReference,
  samples: FailureSample[],
  maxSamples: number
): Promise<SuiteRun> {
  const stats = emptyStats();
  const referenceStats = emptyStats();
  const cleanStats = emptyStats();
  let referenceExact = 0;
  let referenceFailed = 0;
  const referenceDifferences: HistoricalDifference[] = [];
  for (let index = 0; index < inputs.length; index++) {
    const input = inputs[index]!;
    stats.total++;
    referenceStats.total++;
    const currentLisp = currentLispOutputs[input];
    let actual: string | null = null;
    try {
      if (currentLisp === undefined) {
        throw new Error(`Current Lisp romanization output is missing ${JSON.stringify(input)}`);
      }
      const loaded = new Set<string>();
      for (;;) {
        try {
          actual = runtime.analyzer.romanize(input, { normalizePunctuation: true });
          break;
        } catch (error) {
          if (!(error instanceof AnalyzerAnnotationNotLoadedError)) throw error;
          const missing = `${error.kind}:${error.definitionSeq}`;
          if (loaded.has(missing)) throw error;
          loaded.add(missing);
          await runtime.annotations.preloadMissing(error);
        }
      }
      const difference = firstCanonicalDifference(currentLisp, actual);
      if (!difference) {
        stats.exact++;
        stats.pathExact++;
      } else {
        stats.analyzer++;
        if (samples.length < maxSamples) {
          samples.push({
            suite: 'romanization',
            request: input,
            classification: 'analyzer',
            pathDifference: differencePreview(difference),
            detailedDifference: differencePreview(difference)
          });
        }
      }
    } catch (error) {
      stats.errors++;
      referenceStats.errors++;
      if (samples.length < maxSamples) {
        samples.push({
          suite: 'romanization',
          request: input,
          classification: 'error',
          error: error instanceof Error ? `${error.name}: ${error.message}` : String(error)
        });
      }
    } finally {
      runtime.annotations.clear();
    }
    if (actual !== null && currentLisp !== undefined) {
      try {
        resetCalcScoreCache();
        const expected = (await reference.core.romanize(input, {
          withInfo: true,
          normalizePunctuation: true
        })).romanized;
        const referenceDifference = firstCanonicalDifference(currentLisp, expected);
        if (referenceDifference) {
          referenceFailed++;
          referenceDifferences.push({
            request: input,
            difference: differencePreview(referenceDifference)!
          });
        } else referenceExact++;
        recordCleanComparison(
          referenceStats,
          firstCanonicalDifference(expected, actual)
        );
      } catch {
        referenceStats.errors++;
      }
    }
  }
  return {
    stats,
    referenceStats,
    cleanStats,
    referenceExact,
    referenceFailed,
    referenceDifferences
  };
}

function failed(stats: SuiteStats): number {
  return stats.total - stats.exact;
}

function combineStats(values: readonly SuiteStats[]): SuiteStats {
  return values.reduce((total, value) => ({
    total: total.total + value.total,
    exact: total.exact + value.exact,
    pathExact: total.pathExact + value.pathExact,
    analyzer: total.analyzer + value.analyzer,
    presentation: total.presentation + value.presentation,
    errors: total.errors + value.errors
  }), emptyStats());
}

function exactTotals(stats: SuiteStats): Record<string, number> {
  return {
    operations: stats.total,
    exact: stats.exact,
    divergent: failed(stats),
    pathExact: stats.pathExact,
    analyzerDivergent: stats.analyzer,
    presentationDivergent: stats.presentation,
    errors: stats.errors
  };
}

function intersectionSize(left: readonly string[], right: readonly string[]): number {
  const leftSet = new Set(left);
  return new Set(right.filter(value => leftSet.has(value))).size;
}

function corpusAccounting(corpus: AnalyzerParityCorpus): Record<string, unknown> {
  const segmentation = corpus.segmentation.map(value => value.input);
  const cli = corpus.cli.map(value => value.text);
  const hard = corpus.hard.map(value => value.text);
  const counters = corpus.counters.map(value => value.text);
  const entities = corpus.entities.map(value => value.text);
  const probes = corpus.probes.map(value => value.request.text);
  const detailed = [...cli, ...hard, ...counters, ...entities, ...probes];
  const all = [...segmentation, ...corpus.romanization, ...detailed];
  const withoutRomanization = [...segmentation, ...detailed];
  const count = (values: readonly string[]) => ({
    operations: values.length,
    uniqueTexts: new Set(values).size
  });
  return {
    categoriesAreIndependent: true,
    categories: {
      segmentation: count(segmentation),
      standaloneRomanization: count(corpus.romanization),
      cliDetailed: count(cli),
      hardDetailed: count(hard),
      countersDetailed: count(counters),
      entitiesDetailed: count(entities),
      probesDetailed: count(probes),
      probesByCategory: Object.fromEntries([
        ...new Set(corpus.probes.map(value => value.category))
      ].map(category => [
        category,
        count(corpus.probes.filter(value => value.category === category)
          .map(value => value.request.text))
      ])),
      detailedLegacyTotal: count(detailed),
      allComparisons: count(all)
    },
    overlap: {
      cliAndHardUniqueTexts: intersectionSize(cli, hard),
      segmentationAndCliUniqueTexts: intersectionSize(segmentation, cli),
      duplicateEntityRequestsByText: entities.length - new Set(entities).size,
      romanizationTextsAlreadyCoveredElsewhere:
        intersectionSize(corpus.romanization, withoutRomanization)
    },
    notSummedIntoThisDifferential: {
      legacyCliInfoStrings: 3,
      coreJsonConsistencyRoundTrips: 17,
      reason:
        'CLI info formatting is not a browser API; core round trips assert its own object/JSON consistency rather than a second analyzer oracle.'
    }
  };
}

async function main(): Promise<void> {
  const options = parseArgs(process.argv.slice(2));
  const source = await verifyBrowserAlphaSources(options.repository);
  await verifyBrowserAlphaOracleCore(
    options.repository,
    source.lock.postgresReference.repositoryCommit
  );
  const head = await repositoryHead(options.repository);
  const [corpus, runtime] = await Promise.all([
    loadAnalyzerParityCorpus(options.repository),
    openRuntime(options.release, {
      sourceCommit: head,
      sourcesLockSha256: source.lockSha256
    })
  ]);
  const reference = await openReference(options.database, source.lock.database);
  const samples: FailureSample[] = [];
  try {
    await reference.withOracle(async () => {
    const segmentation = await segmentationSuite(
      select(corpus.segmentation, options.smoke, 30), runtime, reference, samples, options.samples
    );
    const romanization = await romanizationSuite(
      corpus.romanization,
      corpus.currentLispRomanization,
      runtime,
      reference,
      samples,
      options.samples
    );
    const cliCases = select(corpus.cli, options.smoke, 15).map(request => ({
      request,
      currentLisp: corpus.currentLispCli[fixtureKey(request)]
    }));
    const hardCases = select(corpus.hard, options.smoke, 15).map(request => ({
      request,
      currentLisp: corpus.currentLispHard[fixtureKey(request)]
    }));
    const counterCases = select(corpus.counters, options.smoke, 20).map(request => ({ request }));
    const entityCases = select(corpus.entities, options.smoke, 15).map(fixture => ({
      request: { text: fixture.text, limit: 1 },
      entities: fixture.entities
    }));
    const cli = await compareSuite('cli', cliCases, runtime, reference, samples, options.samples);
    const hard = await compareSuite('hard', hardCases, runtime, reference, samples, options.samples);
    const counters = await compareSuite('counters', counterCases, runtime, reference, samples, options.samples);
    const entities = await compareSuite('entities', entityCases, runtime, reference, samples, options.samples);
    const probes = await compareSuite(
      'probes',
      corpus.probes.map(probe => ({ request: probe.request })),
      runtime,
      reference,
      samples,
      options.samples
    );
    const detailedStats = combineStats([
      cli.stats, hard.stats, counters.stats, entities.stats, probes.stats
    ]);
    const cleanDetailedStats = combineStats([
      cli.cleanStats, hard.cleanStats, counters.cleanStats,
      entities.cleanStats, probes.cleanStats
    ]);
    const authoritativeStats = [
      segmentation.stats, romanization.stats, cli.stats, hard.stats,
      counters.stats, entities.stats, probes.stats
    ];
    const frozenReferenceStats = [
      segmentation.referenceStats, romanization.referenceStats,
      cli.referenceStats, hard.referenceStats, counters.referenceStats,
      entities.referenceStats, probes.referenceStats
    ];
    const report = {
      formatVersion: 3,
      generatedAt: new Date().toISOString(),
      completeCorpus: !options.smoke,
      corpus: {
        segmentation: corpus.segmentation.length,
        romanization: corpus.romanization.length,
        cli: corpus.cli.length,
        hard: corpus.hard.length,
        counters: corpus.counters.length,
        entities: corpus.entities.length,
        probes: corpus.probes.length
      },
      accounting: corpusAccounting(corpus),
      authoritativeOracleTotals: {
        policy:
          'Pinned current-Lisp snapshots are authoritative where present; the frozen PostgreSQL reference is the fallback for unsnapshotted suites.',
        segmentation: exactTotals(segmentation.stats),
        detailedLegacy: exactTotals(detailedStats),
        standaloneRomanization: exactTotals(romanization.stats),
        allComparisons: exactTotals(combineStats(authoritativeStats)),
        frozenFallbackCleanSemantic: exactTotals(combineStats([
          counters.cleanStats, entities.cleanStats, probes.cleanStats
        ]))
      },
      results: {
        segmentation: segmentation.stats,
        romanization: romanization.stats,
        cli: cli.stats,
        hard: hard.stats,
        counters: counters.stats,
        entities: entities.stats,
        probes: probes.stats,
        frozenFallbackCleanSemantic: {
          counters: counters.cleanStats,
          entities: entities.cleanStats,
          probes: probes.cleanStats
        }
      },
      frozenPostgresDiagnostics: {
        role: {
          snapshotCoveredSuites: 'diagnostic-only',
          unsnapshottedSuites: 'authoritative fallback'
        },
        portableComparison: {
          segmentation: segmentation.referenceStats,
          romanization: romanization.referenceStats,
          cli: cli.referenceStats,
          hard: hard.referenceStats,
          counters: counters.referenceStats,
          entities: entities.referenceStats,
          probes: probes.referenceStats,
          allComparisons: exactTotals(combineStats(frozenReferenceStats)),
          cleanSemantic: exactTotals(cleanDetailedStats)
        },
        currentLispDrift: {
          segmentationExact: segmentation.referenceExact,
          segmentationFailed: segmentation.referenceFailed,
          romanizationExact: romanization.referenceExact,
          romanizationFailed: romanization.referenceFailed,
          cliExact: cli.referenceExact,
          cliFailed: cli.referenceFailed,
          hardExact: hard.referenceExact,
          hardFailed: hard.referenceFailed,
          differences: {
            segmentation: segmentation.referenceDifferences,
            romanization: romanization.referenceDifferences,
            cli: cli.referenceDifferences,
            hard: hard.referenceDifferences
          }
        }
      },
      gate: {
        currentOracleAllowlist: [],
        currentLispSnapshotsAreAuthoritative: true,
        frozenPostgresIsFallbackAndDiagnostic: true,
        rule:
          'Exit nonzero for any current-Lisp divergence in snapshot-covered suites, or any frozen-reference legacy or clean-semantic divergence in unsnapshotted suites, unless --allow-failures is explicitly passed.'
      },
      samples
    };
    const reportText = `${JSON.stringify(report, null, 2)}\n`;
    if (options.out) {
      await mkdir(dirname(options.out), { recursive: true });
      await writeFile(options.out, reportText);
    }
    process.stdout.write(reportText);
    const totalFailures = releaseGateFailureCount({
      currentLisp: [segmentation.stats, romanization.stats, cli.stats, hard.stats],
      frozenFallback: [
        { detailed: counters.stats, clean: counters.cleanStats },
        { detailed: entities.stats, clean: entities.cleanStats },
        { detailed: probes.stats, clean: probes.cleanStats }
      ]
    });
    if (totalFailures > 0 && !options.allowFailures) process.exitCode = 1;
    });
  } finally {
    await reference.close();
  }
}

await main();
