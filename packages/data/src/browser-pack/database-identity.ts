import { execFile as execFileCallback } from 'node:child_process';
import { promisify } from 'node:util';
import type { Sql } from 'postgres';

import {
  BROWSER_ALPHA_SCHEMA_NORMALIZATION,
  sha256Bytes,
  type BrowserAlphaSourceLock
} from './release-orchestration.js';

const execFile = promisify(execFileCallback);

/**
 * pg_dump 16 emits a random session key on these two lines. Removing only
 * those complete lines is the v1 normalization used by the locked oracle.
 */
export interface BrowserAlphaDatabaseIdentity {
  readonly name: string;
  readonly postgresServerVersion: string;
  readonly encoding: string;
  readonly collation: string;
  readonly ctype: string;
  readonly readOnly: boolean;
  readonly schemaSha256: string;
  readonly schemaNormalization: typeof BROWSER_ALPHA_SCHEMA_NORMALIZATION;
}

export function normalizePgDump16Schema(text: string): Uint8Array {
  const normalized = text
    .split('\n')
    .filter(line => !/^\\(?:un)?restrict [^\r\n]+\r?$/.test(line))
    .join('\n');
  return new TextEncoder().encode(normalized);
}

export function pgDumpSchemaInvocation(database: string): {
  readonly args: string[];
  readonly env: NodeJS.ProcessEnv;
} {
  let connection: URL;
  try {
    connection = new URL(database);
  } catch {
    throw new Error('Analyzer database must be a postgresql:// connection URL');
  }
  if (connection.protocol !== 'postgresql:' && connection.protocol !== 'postgres:') {
    throw new Error('Analyzer database must be a postgresql:// connection URL');
  }
  if (connection.searchParams.has('sslpassword')) {
    throw new Error('Analyzer database URLs do not support sslpassword');
  }
  const queryPassword = connection.searchParams.get('password');
  const password = connection.password
    ? decodeURIComponent(connection.password)
    : queryPassword;
  connection.password = '';
  connection.searchParams.delete('password');
  const env = { ...process.env };
  if (password !== null && password !== '') env.PGPASSWORD = password;
  return {
    args: [
      '--schema-only',
      '--no-owner',
      '--no-privileges',
      '--dbname',
      connection.toString()
    ],
    // Keep the password out of pg_dump argv and execFile diagnostics.
    env
  };
}

export async function measurePgDump16SchemaSha256(database: string): Promise<string> {
  const invocation = pgDumpSchemaInvocation(database);
  const { stdout } = await execFile('pg_dump', invocation.args, {
    encoding: 'utf8',
    env: invocation.env,
    maxBuffer: 16 * 1024 * 1024
  });
  return sha256Bytes(normalizePgDump16Schema(stdout));
}

async function readDatabaseIdentity(sql: Sql): Promise<Omit<
  BrowserAlphaDatabaseIdentity,
  'schemaSha256' | 'schemaNormalization'
>> {
  const rows = await sql.unsafe<Array<{
    name: string;
    postgresServerVersion: string;
    encoding: string;
    collation: string;
    ctype: string;
    readOnly: boolean;
  }>>(`
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
  if (!identity.readOnly) throw new Error('Analyzer database transaction is not read-only');
  return identity;
}

export function assertBrowserAlphaDatabaseIdentity(
  actual: BrowserAlphaDatabaseIdentity,
  expected: BrowserAlphaSourceLock['database']
): void {
  for (const [label, wanted, found] of [
    ['name', expected.name, actual.name],
    ['server version', expected.postgresServerVersion, actual.postgresServerVersion],
    ['encoding', expected.encoding, actual.encoding],
    ['collation', expected.collation, actual.collation],
    ['character classification', expected.ctype, actual.ctype],
    ['schema normalization', expected.schemaNormalization, actual.schemaNormalization],
    ['schema digest', expected.schemaSha256, actual.schemaSha256]
  ] as const) {
    if (wanted !== found) {
      throw new Error(`Database ${label} ${found}; sources lock requires ${wanted}`);
    }
  }
}

/** Measure and verify the exact read-only database before it can act as an oracle. */
export async function verifyBrowserAlphaDatabase(
  sql: Sql,
  database: string,
  expected?: BrowserAlphaSourceLock['database']
): Promise<BrowserAlphaDatabaseIdentity> {
  const [identity, schemaSha256] = await Promise.all([
    readDatabaseIdentity(sql),
    measurePgDump16SchemaSha256(database)
  ]);
  const complete: BrowserAlphaDatabaseIdentity = {
    ...identity,
    schemaSha256,
    schemaNormalization: BROWSER_ALPHA_SCHEMA_NORMALIZATION
  };
  if (expected) assertBrowserAlphaDatabaseIdentity(complete, expected);
  return complete;
}
