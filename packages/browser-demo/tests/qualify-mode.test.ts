import { describe, expect, test } from 'bun:test';
import { resolve } from 'node:path';

const packageRoot = resolve(import.meta.dir, '..');

describe('browser qualification mode', () => {
  test('rejects the frozen TypeScript oracle before touching release inputs', () => {
    const result = Bun.spawnSync([
      'bun',
      'scripts/qualify.ts',
      '--release',
      'definitely-missing-release'
    ], {
      cwd: packageRoot,
      env: { ...process.env, ICHIRAN_TYPESCRIPT_ORACLE: '1' },
      stderr: 'pipe',
      stdout: 'pipe'
    });

    expect(result.exitCode).not.toBe(0);
    expect(result.stderr.toString()).toContain(
      'Production browser qualification requires the Rust kernel'
    );
    expect(result.stderr.toString()).not.toContain('ENOENT');
  });
});
