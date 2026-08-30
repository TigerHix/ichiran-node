#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { readFile, rename, writeFile } from 'node:fs/promises';
import { basename, dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

import postgres, { type Sql } from 'postgres';

import {
  normalizeLegacyIdentities,
  type IdentityResolver,
  type IdentitySource
} from '../packages/core/tools/parity-canonical.js';
import {
  deterministicJson,
  verifyBrowserAlphaSources,
  type BrowserAlphaSourceLock
} from '../packages/data/src/browser-pack/release-orchestration.js';

interface RawOutputFile {
  readonly fullJson: Readonly<Record<string, string>>;
}

interface DatabaseIdentity {
  readonly name: string;
  readonly postgresServerVersion: string;
  readonly encoding: string;
  readonly collation: string;
  readonly ctype: string;
  readonly readOnly: boolean;
}

interface FixtureSpec {
  readonly rawPath: string;
  readonly canonicalPath: string;
}

interface GeneratedFixture {
  readonly path: string;
  readonly bytes: Uint8Array;
}

const REPOSITORY = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const IDENTITY_POLICY = 'terminal-root-v1';
const FIXTURES: readonly FixtureSpec[] = [
  {
    rawPath: 'packages/cli/tests/data/cli-lisp-outputs.json',
    canonicalPath: 'packages/cli/tests/data/cli-canonical-outputs.json'
  },
  {
    rawPath: 'packages/cli/tests/data/hard-cli-lisp-outputs.json',
    canonicalPath: 'packages/cli/tests/data/hard-cli-canonical-outputs.json'
  }
];

function usage(message?: string): never {
  if (message) console.error(message);
  console.error(
    'usage: bun scripts/canonicalize-lisp-fixtures.ts '
    + '--database <postgres-url>\n'
    + 'ICHIRAN_DB_URL may be used instead of --database.'
  );
  process.exit(message ? 2 : 0);
}

function databaseArgument(arguments_: readonly string[]): string {
  let database = process.env.ICHIRAN_DB_URL ?? '';
  for (let index = 0; index < arguments_.length; index++) {
    const argument = arguments_[index]!;
    if (argument === '--database') {
      const value = arguments_[++index];
      if (!value) usage('--database requires a value');
      database = value;
    } else if (argument === '--help' || argument === '-h') {
      usage();
    } else {
      usage(`unknown argument ${argument}`);
    }
  }
  if (!database) usage('--database or ICHIRAN_DB_URL is required');
  return database;
}

function sha256(bytes: Uint8Array | string): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function connection(database: string): Sql {
  const normalized = database.replace(/^postgresql:/, 'postgres:');
  if (!/^postgres:\/\//.test(normalized)) {
    throw new Error('Database must be a postgres:// or postgresql:// URL');
  }
  const url = new URL(normalized);
  const databaseName = decodeURIComponent(url.pathname.replace(/^\//, ''));
  if (!databaseName) throw new Error('Database URL is missing its database name');
  const queryHost = url.searchParams.get('host');
  const host = queryHost ? decodeURIComponent(queryHost) : url.hostname || undefined;
  const portText = url.port || url.searchParams.get('port');
  const port = portText ? Number(portText) : undefined;
  if (port !== undefined && (!Number.isSafeInteger(port) || port < 1 || port > 65_535)) {
    throw new Error('Database URL has an invalid port');
  }
  const sslMode = url.searchParams.get('sslmode');
  const ssl = sslMode === 'disable' ? false
    : sslMode === 'require' || sslMode === 'verify-ca' || sslMode === 'verify-full'
      ? 'require'
      : undefined;
  return postgres({
    database: databaseName,
    ...(host === undefined ? {} : { host }),
    ...(port === undefined ? {} : { port }),
    ...(url.username ? { user: decodeURIComponent(url.username) } : {}),
    ...(url.password ? { password: decodeURIComponent(url.password) } : {}),
    ...(ssl === undefined ? {} : { ssl }),
    max: 1,
    prepare: false,
    transform: postgres.camel,
    connection: {
      application_name: 'ichiran-canonical-fixture-generator',
      default_transaction_read_only: true
    }
  });
}

async function databaseIdentity(sql: Sql): Promise<DatabaseIdentity> {
  const rows = await sql.unsafe<DatabaseIdentity[]>(`
    SELECT current_database() AS name,
           current_setting('server_version') AS "postgresServerVersion",
           pg_encoding_to_char(d.encoding) AS encoding,
           d.datcollate AS collation,
           d.datctype AS ctype,
           current_setting('transaction_read_only') = 'on' AS "readOnly"
    FROM pg_database d
    WHERE d.datname = current_database()
  `);
  const identity = rows[0];
  if (!identity || rows.length !== 1) throw new Error('Could not read database identity');
  return identity;
}

function assertDatabaseIdentity(actual: DatabaseIdentity, lock: BrowserAlphaSourceLock): void {
  if (!actual.readOnly) throw new Error('Fixture generator database transaction is not read-only');
  for (const [label, expected, found] of [
    ['name', lock.database.name, actual.name],
    ['server version', lock.database.postgresServerVersion, actual.postgresServerVersion],
    ['encoding', lock.database.encoding, actual.encoding],
    ['collation', lock.database.collation, actual.collation],
    ['character classification', lock.database.ctype, actual.ctype]
  ] as const) {
    if (expected !== found) {
      throw new Error(`Database ${label} ${found}; sources lock requires ${expected}`);
    }
  }
}

/** Compiler-only resolver for the legacy physical-target identity contract. */
class PostgresFixtureIdentityResolver implements IdentityResolver {
  readonly #sql: Sql;
  readonly #cache = new Map<string, Promise<readonly number[]>>();
  readonly #unresolved = new Set<string>();

  constructor(sql: Sql) {
    this.#sql = sql;
  }

  roots(
    seq: number,
    surface?: string,
    sources: readonly IdentitySource[] = []
  ): Promise<readonly number[]> {
    const key = JSON.stringify([seq, surface ?? null, sources]);
    let value = this.#cache.get(key);
    if (!value) {
      value = this.#load(seq, surface, sources);
      this.#cache.set(key, value);
    }
    return value;
  }

  assertFullyResolved(): void {
    if (this.#unresolved.size === 0) return;
    const examples = [...this.#unresolved].sort().slice(0, 8).join(', ');
    throw new Error(
      `Canonical fixture identity resolver left ${this.#unresolved.size} non-root target(s): ${examples}`
    );
  }

  #unresolvedIdentity(
    seq: number,
    surface: string | undefined,
    sources: readonly IdentitySource[]
  ): readonly number[] {
    this.#unresolved.add(JSON.stringify([seq, surface ?? null, sources]));
    return [seq];
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

    // Resolve the exact displayed path. A shared physical target may represent
    // different roots for different surfaces or two-stage source lineages.
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
    if (sources.length === 0) return this.#unresolvedIdentity(seq, surface, sources);

    // Suffix rewrites can change the displayed surface while retaining the
    // generated target. The detailed Lisp result still carries root readings.
    const sourceRows = await this.#sql<{
      seq: number;
      route: 'kanji' | 'kana';
      text: string;
    }[]>`
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
      const values = target.get(row.seq) ?? new Set<string>();
      values.add(row.text);
      target.set(row.seq, values);
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
    return roots.length > 0 ? roots : this.#unresolvedIdentity(seq, surface, sources);
  }
}

async function canonicalFixture(
  spec: FixtureSpec,
  resolver: IdentityResolver,
  source: Awaited<ReturnType<typeof verifyBrowserAlphaSources>>
): Promise<GeneratedFixture> {
  const rawBytes = new Uint8Array(await readFile(join(REPOSITORY, spec.rawPath)));
  const raw = JSON.parse(new TextDecoder().decode(rawBytes)) as RawOutputFile;
  const fullJson: Record<string, string> = {};
  let rewrittenSeqFields = 0;
  let multipleRootIdentityKeys = 0;
  for (const [key, value] of Object.entries(raw.fullJson)) {
    const normalized = await normalizeLegacyIdentities(JSON.parse(value) as unknown, resolver);
    fullJson[key] = JSON.stringify(normalized.value);
    rewrittenSeqFields += normalized.rewritten;
    multipleRootIdentityKeys += Object.keys(normalized.multipleRoots).length;
  }
  const outputsSha256 = sha256(JSON.stringify(fullJson));
  const output = {
    formatVersion: 1,
    identityPolicy: IDENTITY_POLICY,
    source: {
      path: spec.rawPath,
      sha256: sha256(rawBytes)
    },
    oracle: {
      sourcesLockSha256: source.lockSha256,
      upstreamIchiranCommit: source.lock.upstreamIchiran.commit,
      dataReleaseTag: source.lock.upstreamIchiran.dataReleaseTag,
      postgresReferenceCommit: source.lock.postgresReference.repositoryCommit,
      databaseDumpSha256: source.lock.databaseDump.sha256,
      databaseSchemaSha256: source.lock.database.schemaSha256
    },
    stats: {
      requests: Object.keys(fullJson).length,
      rewrittenSeqFields,
      multipleRootIdentityKeys,
      outputsSha256
    },
    fullJson
  };
  return { path: spec.canonicalPath, bytes: deterministicJson(output) };
}

async function publish(fixtures: readonly GeneratedFixture[]): Promise<void> {
  const temporary: string[] = [];
  try {
    for (const fixture of fixtures) {
      const destination = join(REPOSITORY, fixture.path);
      const staging = join(dirname(destination), `.${basename(destination)}.${process.pid}.tmp`);
      await writeFile(staging, fixture.bytes, { flag: 'wx' });
      temporary.push(staging);
    }
    for (let index = 0; index < fixtures.length; index++) {
      await rename(temporary[index]!, join(REPOSITORY, fixtures[index]!.path));
    }
  } catch (error) {
    await Promise.all(temporary.map(path => Bun.file(path).delete().catch(() => {})));
    throw error;
  }
}

async function main(): Promise<void> {
  const database = databaseArgument(process.argv.slice(2));
  const source = await verifyBrowserAlphaSources(REPOSITORY);
  const pool = connection(database);
  const sql = await pool.reserve();
  try {
    await sql.unsafe('BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY');
    assertDatabaseIdentity(await databaseIdentity(sql), source.lock);
    const resolver = new PostgresFixtureIdentityResolver(sql);
    const generated: GeneratedFixture[] = [];
    for (const spec of FIXTURES) {
      const fixture = await canonicalFixture(spec, resolver, source);
      generated.push(fixture);
      console.error(`Canonicalized ${spec.rawPath}`);
    }
    resolver.assertFullyResolved();
    await publish(generated);
    for (const fixture of generated) {
      console.log(`${fixture.path}: ${fixture.bytes.byteLength} bytes, sha256 ${sha256(fixture.bytes)}`);
    }
  } finally {
    try {
      await sql.unsafe('ROLLBACK');
    } finally {
      sql.release();
      await pool.end();
    }
  }
}

await main();
