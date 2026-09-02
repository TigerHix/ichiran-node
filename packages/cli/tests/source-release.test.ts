import { describe, expect, test } from 'bun:test';
import { resolve } from 'node:path';

const releaseDirectory = process.env.ICHIRAN_PACK_DIR;
const packageRoot = resolve(import.meta.dir, '..');

describe.skipIf(!releaseDirectory)('CLI source release', () => {
  test('uses the installed source pack through the executable Rust adapter', () => {
    const result = Bun.spawnSync(['node', 'dist/index.js', '今日'], {
      cwd: packageRoot,
      env: { ...process.env, ICHIRAN_PACK_DIR: releaseDirectory! },
      stderr: 'pipe',
      stdout: 'pipe'
    });

    expect(result.exitCode).toBe(0);
    expect(result.stderr.toString()).toBe('');
    expect(result.stdout.toString()).toBe('kyō\n');
  });

  test('returns the retained full JSON shape without a database', () => {
    const result = Bun.spawnSync(['node', 'dist/index.js', '--full', '食べた'], {
      cwd: packageRoot,
      env: { ...process.env, ICHIRAN_PACK_DIR: releaseDirectory! },
      stderr: 'pipe',
      stdout: 'pipe'
    });

    expect(result.exitCode).toBe(0);
    expect(result.stderr.toString()).toBe('');
    expect(JSON.parse(result.stdout.toString())).toBeArray();
  });
});
