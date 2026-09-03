#!/usr/bin/env bun

import { execFile as execFileCallback, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdtemp, mkdir, readFile, readdir, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join, relative, resolve } from 'node:path';
import { promisify } from 'node:util';
import { brotliDecompressSync } from 'node:zlib';

import { parseSourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';

const execFile = promisify(execFileCallback);

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function verify(
  label: string,
  bytes: Uint8Array,
  expectedBytes: number,
  expectedSha256: string
): void {
  const actualSha256 = sha256(bytes);
  if (bytes.byteLength !== expectedBytes || actualSha256 !== expectedSha256) {
    throw new Error(
      `${label} is ${bytes.byteLength} bytes ${actualSha256}; `
      + `expected ${expectedBytes} bytes ${expectedSha256}`
    );
  }
}

async function filesBelow(directory: string): Promise<string[]> {
  const entries = await readdir(directory, { withFileTypes: true });
  const files: string[] = [];
  for (const entry of entries) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) files.push(...await filesBelow(path));
    else files.push(path);
  }
  return files;
}

function deterministicGzip(bytes: Uint8Array): Buffer {
  const result = spawnSync('gzip', ['-n', '-9', '-c'], {
    input: bytes,
    maxBuffer: 32 * 1024 * 1024
  });
  if (result.status !== 0) throw new Error(`gzip failed: ${String(result.stderr)}`);
  return result.stdout;
}

const repository = resolve(import.meta.dir, '..');
const lockPath = process.argv[2];
if (!lockPath) {
  throw new Error('Usage: bun scripts/acquire-source-compiler-jmdict.ts <source-lock> [output]');
}

const lockAbsolute = resolve(repository, lockPath);
const lock = parseSourceCompilerLock(JSON.parse(await readFile(lockAbsolute, 'utf8')));
const source = lock.sources.find(item => item.kind === 'jmdict');
if (!source || !('archivePatch' in source)) {
  throw new Error('JMdict acquisition requires an archive-backed transition lock');
}
if (lock.transition?.acquisitionScript !== 'scripts/acquire-source-compiler-jmdict.ts') {
  throw new Error('Transition lock does not name this acquisition command');
}

const output = resolve(repository, process.argv[3] ?? source.file.path);
const temporaryBase = process.platform === 'linux' ? '/tmp' : tmpdir();
const temporary = await mkdtemp(join(temporaryBase, 'ichiran-jmdict-acquire-'));
try {
  const archive = join(temporary, 'archive');
  await execFile('git', [
    'clone', '--filter=blob:none', '--no-checkout', source.archiveRepository, archive
  ]);
  await execFile('git', ['-C', archive, 'sparse-checkout', 'init', '--cone']);
  const archiveDirectory = source.archivePatch.split('/')[0]!;
  await execFile('git', [
    '-C', archive, 'sparse-checkout', 'set', archiveDirectory, 'LICENSE', 'README.md'
  ]);
  await execFile('git', ['-C', archive, 'checkout', '--detach', source.archiveCommit]);

  const targetPatch = join(archive, source.archivePatch);
  verify(
    'JMdict target archive patch',
    await readFile(targetPatch),
    source.archivePatchBytes,
    source.archivePatchSha256
  );

  const sourceDirectory = join(archive, archiveDirectory);
  const xmlPath = join(temporary, 'JMdict_e');
  await writeFile(
    xmlPath,
    brotliDecompressSync(await readFile(join(sourceDirectory, `${archiveDirectory}.br`)))
  );

  const patchRoot = join(sourceDirectory, 'patches');
  let appliedTarget = false;
  for (const patchPath of (await filesBelow(patchRoot)).sort()) {
    const relativePatch = relative(archive, patchPath);
    if (relativePatch > source.archivePatch) break;
    const applied = spawnSync('patch', ['--quiet', xmlPath], {
      input: brotliDecompressSync(await readFile(patchPath))
    });
    if (applied.status !== 0) {
      throw new Error(`JMdict patch failed at ${relativePatch}: ${String(applied.stderr)}`);
    }
    if (relativePatch === source.archivePatch) appliedTarget = true;
  }
  if (!appliedTarget) throw new Error(`JMdict target patch was not applied: ${source.archivePatch}`);

  const xml = await readFile(xmlPath);
  verify('JMdict XML', xml, source.uncompressedBytes, source.uncompressedSha256);
  if (!xml.subarray(0, 64 * 1024).includes(`<!-- ${source.upstreamIdentity} -->`)) {
    throw new Error(`JMdict XML is missing ${source.upstreamIdentity}`);
  }

  const gzip = deterministicGzip(xml);
  verify('JMdict deterministic gzip', gzip, source.file.bytes, source.file.sha256);
  await mkdir(dirname(output), { recursive: true });
  await writeFile(output, gzip, { flag: 'wx' });
  process.stdout.write(`${output} ${gzip.byteLength} ${sha256(gzip)}\n`);
} finally {
  await rm(temporary, { recursive: true, force: true });
}
