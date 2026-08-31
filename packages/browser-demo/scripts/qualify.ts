import { spawn } from 'node:child_process';
import { resolve } from 'node:path';

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

await run('bun', ['scripts/stage-analyzer.ts', release], packageRoot);
await run('bun', ['run', 'build'], packageRoot);
await run('bun', [
  'run', 'alpha:release:verify', '--',
  '--out', release,
  '--shell-dir', resolve(packageRoot, 'dist')
], repositoryRoot);
await run('bun', [
  'scripts/audit-build.ts', '--require-analyzer', '--release', release
], packageRoot);
if (!skipE2e) await run('bun', ['run', 'test:e2e'], packageRoot);

console.log(
  `Browser alpha qualification passed for ${release} with a verified production-shell fingerprint`
  + (skipE2e ? ' (E2E skipped)' : '')
);
