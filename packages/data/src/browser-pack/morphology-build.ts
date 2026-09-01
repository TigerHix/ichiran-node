import { gzipSync } from 'node:zlib';
import { compileMorphology } from './morphology-compiler-oracle.js';

function outputPath(argv: readonly string[]): string {
  const index = argv.indexOf('--out');
  if (index < 0 || !argv[index + 1]) {
    throw new Error('Usage: bun morphology-build.ts --out <morphology.bin> [--data <data-directory>]');
  }
  return argv[index + 1]!;
}

function dataPath(argv: readonly string[]): string | undefined {
  const index = argv.indexOf('--data');
  return index < 0 ? undefined : argv[index + 1];
}

const out = outputPath(process.argv.slice(2));
const result = await compileMorphology({ dataPath: dataPath(process.argv.slice(2)) });
await Bun.write(out, result.bytes);

console.log(JSON.stringify({
  output: out,
  ...result.stats,
  gzipBytes: gzipSync(result.bytes, { level: 9 }).byteLength
}, null, 2));
