#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { mkdir, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';

const URL = 'https://www.edrdg.org/pub/Nihongo/JMdict_e.gz';
const OUTPUT = 'work/m6-transition/JMdict_e-2026-09-01.gz';
const GZIP_BYTES = 10_564_910;
const GZIP_SHA256 = 'a2cce17805c392712a9569c515076ae84a0091281b54542753de1060add8c55e';
const XML_BYTES = 63_074_995;
const XML_SHA256 = '0401f8ef8e5bd9888ca20a9c11a3eaf066ff16d143ac641591ae4c0f8e6a47ab';
const HEADER = '<!-- JMdict created: 2026-09-01 -->';

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function verify(label: string, bytes: Uint8Array, expectedBytes: number, expectedSha256: string): void {
  const actualSha256 = sha256(bytes);
  if (bytes.byteLength !== expectedBytes || actualSha256 !== expectedSha256) {
    throw new Error(
      `${label} is ${bytes.byteLength} bytes ${actualSha256}; `
      + `expected ${expectedBytes} bytes ${expectedSha256}`
    );
  }
}

const response = await fetch(URL);
if (!response.ok) throw new Error(`JMdict download failed: HTTP ${response.status}`);
const compressed = new Uint8Array(await response.arrayBuffer());
verify('JMdict_e.gz', compressed, GZIP_BYTES, GZIP_SHA256);
const xml = new Uint8Array(gunzipSync(compressed));
verify('JMdict_e.xml', xml, XML_BYTES, XML_SHA256);
if (!new TextDecoder().decode(xml.subarray(0, 32_768)).includes(HEADER)) {
  throw new Error(`JMdict source is missing ${HEADER}`);
}

const output = resolve(process.argv[2] ?? OUTPUT);
await mkdir(dirname(output), { recursive: true });
await writeFile(output, compressed, { flag: 'wx' });
process.stdout.write(`${output} ${compressed.byteLength} ${GZIP_SHA256}\n`);
