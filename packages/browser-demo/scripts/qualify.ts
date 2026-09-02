import { spawn } from 'node:child_process';
import { resolve } from 'node:path';
import { verifyAnalyzerRelease } from './release-files.js';

const packageRoot = resolve(import.meta.dir, '..');
const repositoryRoot = resolve(packageRoot, '..', '..');
let release = resolve(repositoryRoot, 'dist', 'browser-alpha');
let skipE2e = false;
for (let index = 2; index < process.argv.length; index++) {
  const argument = process.argv[index];
  if (argument === '--release') {
    const value = process.argv[++index];
    if (!value) throw new Error('--release requires a directory');
    release = resolve(repositoryRoot, value);
  } else if (argument === '--skip-e2e') {
    skipE2e = true;
  } else {
    throw new Error(`Unknown qualification argument: ${argument}`);
  }
}

async function run(
  command: string,
  args: readonly string[],
  cwd: string,
  capture = false
): Promise<string> {
  const child = spawn(command, args, {
    cwd,
    stdio: capture ? ['ignore', 'pipe', 'inherit'] : 'inherit'
  });
  let stdout = '';
  if (capture && child.stdout) {
    child.stdout.setEncoding('utf8');
    child.stdout.on('data', chunk => { stdout += chunk; });
  }
  const code = await new Promise<number | null>((resolveExit, reject) => {
    child.once('error', reject);
    child.once('exit', resolveExit);
  });
  if (code !== 0) throw new Error(`${command} ${args.join(' ')} exited ${code}`);
  return stdout.trim();
}

const verifiedRelease = await verifyAnalyzerRelease(
  release,
  repositoryRoot,
  process.env.ICHIRAN_QUALIFIED_ARTIFACT
);
await run('bun', ['scripts/stage-analyzer.ts', release], packageRoot);
await run('bun', ['run', 'build'], packageRoot);
await run('bun', [
  'scripts/audit-build.ts', '--require-analyzer', '--release', release
], packageRoot);
const shellBytes = Number(await run('bun', ['scripts/measure-shell.ts'], packageRoot, true));
if (!Number.isSafeInteger(shellBytes) || shellBytes < 1) {
  throw new Error(`Invalid production shell byte count: ${shellBytes}`);
}
const releaseDownloadBytes = verifiedRelease.manifestBytes.byteLength
  + verifiedRelease.hotBytes.byteLength
  + verifiedRelease.detailsBytes.byteLength;
const firstInstallBytes = releaseDownloadBytes + shellBytes;
const firstInstallLimit = 25 * 1024 * 1024;
if (firstInstallBytes > firstInstallLimit) {
  throw new Error(
    `First-install bytes ${firstInstallBytes} exceed the ${firstInstallLimit}-byte limit`
  );
}
if (!skipE2e) await run('bun', ['run', 'test:e2e'], packageRoot);

console.log(
  `Browser qualification passed for ${release}: ${releaseDownloadBytes} release bytes + `
  + `${shellBytes} shell bytes = ${firstInstallBytes} first-install bytes`
  + (skipE2e ? ' (E2E skipped)' : '')
);
