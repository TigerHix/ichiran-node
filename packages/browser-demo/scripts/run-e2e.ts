import { spawn } from 'node:child_process';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';

const scriptPath = fileURLToPath(import.meta.url);
const pinned = process.argv[2] === '--pinned';
const forwarded = process.argv.slice(pinned ? 3 : 2);

function affinityList(status: string): string {
  const value = /^Cpus_allowed_list:\s*(.+)$/m.exec(status)?.[1]?.trim();
  if (!value) throw new Error('Linux did not report Cpus_allowed_list');
  return value;
}

function allowedCpus(value: string): number[] {
  const result: number[] = [];
  for (const part of value.split(',')) {
    const match = /^(\d+)(?:-(\d+))?$/.exec(part);
    if (!match) throw new Error(`Unsupported Linux CPU affinity list: ${value}`);
    const start = Number(match[1]);
    const end = Number(match[2] ?? match[1]);
    for (let cpu = start; cpu <= end; cpu++) result.push(cpu);
  }
  return result;
}

async function exitCode(child: ReturnType<typeof spawn>): Promise<number> {
  return new Promise((resolve, reject) => {
    child.once('error', reject);
    child.once('exit', (code, signal) => {
      if (signal) reject(new Error(`Child process exited on ${signal}`));
      else resolve(code ?? 1);
    });
  });
}

if (process.platform !== 'linux') {
  throw new Error('The browser performance gate requires Linux taskset');
}

if (pinned) {
  const expected = process.env.ICHIRAN_E2E_AFFINITY_CPU;
  const actual = affinityList(await readFile('/proc/self/status', 'utf8'));
  if (!expected || actual !== expected) {
    throw new Error(`Pinned E2E affinity ${actual} does not match requested CPU ${expected ?? '(missing)'}`);
  }
  const child = spawn('playwright', ['test', ...forwarded], { stdio: 'inherit' });
  process.exitCode = await exitCode(child);
} else {
  const available = allowedCpus(affinityList(await readFile('/proc/self/status', 'utf8')));
  const configured = process.env.ICHIRAN_E2E_CPU;
  const cpu = configured === undefined ? available.at(-1)! : Number(configured);
  if (!Number.isSafeInteger(cpu) || !available.includes(cpu)) {
    throw new Error(`ICHIRAN_E2E_CPU must be one of: ${available.join(', ')}`);
  }

  const child = spawn(
    'taskset',
    ['-c', String(cpu), process.execPath, scriptPath, '--pinned', ...forwarded],
    {
      detached: true,
      stdio: 'inherit',
      env: { ...process.env, ICHIRAN_E2E_AFFINITY_CPU: String(cpu) }
    }
  );
  if (!child.pid) throw new Error('taskset did not start the E2E process group');

  let stopping = false;
  const killGroup = (signal: NodeJS.Signals): void => {
    try { process.kill(-child.pid!, signal); } catch { /* The process group already exited. */ }
  };
  for (const signal of ['SIGINT', 'SIGTERM', 'SIGHUP'] as const) {
    process.once(signal, () => {
      if (stopping) return;
      stopping = true;
      killGroup('SIGTERM');
      setTimeout(() => killGroup('SIGKILL'), 1_000).unref();
    });
  }

  let code = 1;
  try {
    code = await exitCode(child);
  } finally {
    // Any CPU hog orphaned by an abrupt Playwright failure remains in this
    // process group, so the wrapper owns the final cleanup boundary.
    killGroup('SIGTERM');
    await new Promise(resolve => setTimeout(resolve, 100));
    killGroup('SIGKILL');
  }
  process.exitCode = code;
}
