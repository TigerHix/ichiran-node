#!/usr/bin/env bun

import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { brotliDecompressSync, gunzipSync } from 'node:zlib';

const ARCHIVE_COMMIT = 'fbc4afb4c786b7f4c304c173a475553279bbb528';
const PATCH_URL = `https://raw.githubusercontent.com/Jitendex/edrdg-dictionary-archive/${ARCHIVE_COMMIT}/JMdict_e/patches/2026/01/02.patch.br`;
const PATCH_BYTES = 3_086;
const PATCH_SHA256 = 'f0aae6a10273bc2ea569ea1aeead3a81b5268716c5e7347cf26fb5ca2666ca77';
const PATCH_PAYLOAD_BYTES = 13_911;
const PATCH_PAYLOAD_SHA256 = '62fbc4c7f14cb3cb1635ac8f6a253a99b2fe353bfeb5ef8b8cb53cf73469a86d';
const BASELINE = 'packages/data/JMdict_e.gz';
const OUTPUT = 'work/m6-transition/JMdict_e-2026-01-02.gz';
const BASELINE_GZIP_BYTES = 10_260_701;
const BASELINE_GZIP_SHA256 = '92eb77d60e5b949585e41a777ff3857c412bc97ea75444d14497a5156b6264b7';
const XML_BYTES = 61_500_527;
const XML_SHA256 = '9f125b6f574102e37279660fa022de63fdff28f99c3f5d47ba69c80c3c999f34';
const GZIP_BYTES = 10_261_624;
const GZIP_SHA256 = '34cc33abe2ae37a8572a9a45ce68c5e7fb6ccccd55c021366eb4fa6c49f6c90c';
const HEADER = '<!-- JMdict created: 2026-01-02 -->';

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

function deterministicGzip(bytes: Uint8Array): Buffer {
  const result = spawnSync('gzip', ['-n', '-9', '-c'], {
    input: bytes,
    maxBuffer: 20 * 1024 * 1024
  });
  if (result.status !== 0) throw new Error(`gzip failed: ${String(result.stderr)}`);
  return result.stdout;
}

const response = await fetch(PATCH_URL);
if (!response.ok) throw new Error(`JMdict archive patch download failed: HTTP ${response.status}`);
const compressedPatch = new Uint8Array(await response.arrayBuffer());
verify('JMdict archive patch', compressedPatch, PATCH_BYTES, PATCH_SHA256);
const patch = brotliDecompressSync(compressedPatch);
verify('JMdict archive patch payload', patch, PATCH_PAYLOAD_BYTES, PATCH_PAYLOAD_SHA256);

const repository = resolve(import.meta.dir, '..');
const temporary = await mkdtemp(join(tmpdir(), 'ichiran-jmdict-2026-01-02-'));
try {
  const baseline = await readFile(join(repository, BASELINE));
  verify('January 1 baseline gzip', baseline, BASELINE_GZIP_BYTES, BASELINE_GZIP_SHA256);
  const xmlPath = join(temporary, 'JMdict_e');
  await writeFile(xmlPath, gunzipSync(baseline));
  const applied = spawnSync('patch', ['--quiet', xmlPath], { input: patch });
  if (applied.status !== 0) throw new Error(`JMdict patch failed: ${String(applied.stderr)}`);
  const xml = await readFile(xmlPath);
  verify('JMdict_e.xml', xml, XML_BYTES, XML_SHA256);
  if (!new TextDecoder().decode(xml.subarray(0, 32_768)).includes(HEADER)) {
    throw new Error(`JMdict source is missing ${HEADER}`);
  }
  const gzip = deterministicGzip(xml);
  verify('JMdict_e.gz', gzip, GZIP_BYTES, GZIP_SHA256);
  const output = resolve(process.argv[2] ?? OUTPUT);
  await mkdir(dirname(output), { recursive: true });
  await writeFile(output, gzip, { flag: 'wx' });
  process.stdout.write(`${output} ${gzip.byteLength} ${GZIP_SHA256}\n`);
} finally {
  await rm(temporary, { recursive: true, force: true });
}
