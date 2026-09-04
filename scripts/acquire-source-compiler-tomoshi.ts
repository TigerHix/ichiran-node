#!/usr/bin/env bun

import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';
import { constants, createReadStream } from 'node:fs';
import { copyFile, mkdir, mkdtemp, readFile, rm, stat } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';

import { parseSourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';

function command(executable: string, args: readonly string[]): Promise<void> {
  return new Promise((resolvePromise, reject) => {
    const child = spawn(executable, args, { stdio: 'inherit' });
    child.once('error', reject);
    child.once('close', (code, signal) => {
      if (code === 0) resolvePromise();
      else reject(new Error(`${executable} failed (${signal ?? code})`));
    });
  });
}

async function fileSha256(path: string): Promise<string> {
  const hash = createHash('sha256');
  for await (const chunk of createReadStream(path)) hash.update(chunk as Buffer);
  return hash.digest('hex');
}

async function verify(
  label: string,
  path: string,
  expectedBytes: number,
  expectedSha256: string
): Promise<void> {
  const [info, actualSha256] = await Promise.all([stat(path), fileSha256(path)]);
  if (!info.isFile() || info.size !== expectedBytes || actualSha256 !== expectedSha256) {
    throw new Error(
      `${label} is ${info.size} bytes ${actualSha256}; `
      + `expected ${expectedBytes} bytes ${expectedSha256}`
    );
  }
}

const repository = resolve(import.meta.dir, '..');
const lockPath = process.argv[2];
if (!lockPath) {
  throw new Error(
    'Usage: bun scripts/acquire-source-compiler-tomoshi.ts <source-lock> [output]'
  );
}
const lock = parseSourceCompilerLock(JSON.parse(
  await readFile(resolve(repository, lockPath), 'utf8')
));
const source = lock.sources.find(item => item.kind === 'tomoshi-dict');
if (!source) throw new Error('Source lock does not contain a Tomoshi dictionary source');

const output = resolve(repository, process.argv[3] ?? source.file.path);
const temporary = await mkdtemp(join(tmpdir(), 'ichiran-tomoshi-acquire-'));
try {
  const archive = join(temporary, 'tomoshi-dict-open.db.zst');
  const database = join(temporary, 'tomoshi-dict-open.db');
  await command('curl', ['--fail', '--location', '--output', archive, source.authoritativeUrl]);
  await verify('Tomoshi release archive', archive, source.archiveBytes, source.archiveSha256);
  await command('zstd', ['--decompress', '--quiet', archive, '--output', database]);
  await verify('Tomoshi SQLite database', database, source.file.bytes, source.file.sha256);
  await mkdir(dirname(output), { recursive: true });
  await copyFile(database, output, constants.COPYFILE_EXCL);
  process.stdout.write(`${output} ${source.file.bytes} ${source.file.sha256}\n`);
} finally {
  await rm(temporary, { recursive: true, force: true });
}
