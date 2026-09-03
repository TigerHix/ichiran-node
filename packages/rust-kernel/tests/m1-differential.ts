import { createHash } from 'node:crypto';
import { resolve } from 'node:path';

import type { AnalysisResult } from '../../core/dist/index.js';
import { TypeScriptOracleRuntime } from '../../core/dist/qualification.js';

const PACK_SHA256 = '61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0';
const directory = process.env.ICHIRAN_M1_PACK_DIR;
if (!directory) throw new Error('ICHIRAN_M1_PACK_DIR must name portable-core-260118-baseline');

const hot = new Uint8Array(await Bun.file(resolve(directory, 'hot.bin')).arrayBuffer());
const digest = createHash('sha256').update(hot).digest('hex');
if (digest !== PACK_SHA256) throw new Error(`qualified hot.bin digest mismatch: ${digest}`);

const details = Bun.file(resolve(directory, 'details.bin'));
const oracle = await TypeScriptOracleRuntime.open({
  hot,
  details: {
    byteLength: details.size,
    read: async (offset, byteLength) => new Uint8Array(
      await details.slice(offset, offset + byteLength).arrayBuffer()
    )
  },
  decodeGzip: async (compressed, expectedByteLength) => {
    const decoded = new Uint8Array(Bun.gunzipSync(compressed));
    if (decoded.byteLength !== expectedByteLength) {
      throw new Error(`gzip decoded ${decoded.byteLength}; expected ${expectedByteLength}`);
    }
    return decoded;
  }
});

const build = Bun.spawnSync({
  cmd: ['cargo', 'build', '--release', '--example', 'm1_probe'],
  cwd: resolve(import.meta.dirname, '..'),
  stdout: 'inherit',
  stderr: 'inherit'
});
if (build.exitCode !== 0) throw new Error(`Rust fixture probe build exited ${build.exitCode}`);

const probe = Bun.spawnSync({
  cmd: [resolve(import.meta.dirname, '../target/release/examples/m1_probe'), directory, '--fixtures'],
  stdout: 'pipe',
  stderr: 'inherit'
});
if (probe.exitCode !== 0) throw new Error(`Rust fixture probe exited ${probe.exitCode}`);
const actual = JSON.parse(probe.stdout.toString()) as readonly {
  readonly name: string;
  readonly result: AnalysisResult;
}[];

const fixtures = [
  ['direct', '猫'],
  ['morphology', '食べた'],
  ['generated', '忘れた'],
  ['astral', '😀'],
  ['high-surrogate', String.fromCharCode(0xd83d)],
  ['low-surrogate', String.fromCharCode(0xde00)]
] as const;

if (actual.length !== fixtures.length) {
  throw new Error(`Rust returned ${actual.length} fixtures; expected ${fixtures.length}`);
}
for (let index = 0; index < fixtures.length; index++) {
  const [name, text] = fixtures[index]!;
  const expected = { ...await oracle.analyze(text, { limit: 1 }), computeMs: 0 };
  const observed = actual[index]!;
  if (observed.name !== name) throw new Error(`fixture order mismatch at ${index}`);
  if (JSON.stringify(observed.result) !== JSON.stringify(expected)) {
    throw new Error(`${name} TypeScript/Rust differential mismatch\nexpected=${JSON.stringify(expected)}\nactual=${JSON.stringify(observed.result)}`);
  }
}

console.log(`M1 live TypeScript/Rust differential passed: ${fixtures.length}/${fixtures.length} exact`);
