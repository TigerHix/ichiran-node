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

  test('rejects the immutable-artifact override before touching release inputs', () => {
    const result = Bun.spawnSync([
      'bun',
      'scripts/qualify.ts',
      '--release',
      'definitely-missing-release'
    ], {
      cwd: packageRoot,
      env: { ...process.env, ICHIRAN_QUALIFIED_ARTIFACT: 'portable-core-260118-baseline' },
      stderr: 'pipe',
      stdout: 'pipe'
    });

    expect(result.exitCode).not.toBe(0);
    expect(result.stderr.toString()).toContain(
      'Source browser qualification does not accept ICHIRAN_QUALIFIED_ARTIFACT'
    );
    expect(result.stderr.toString()).not.toContain('ENOENT');
  });

  test('standalone source verification rejects the immutable-artifact override', () => {
    const result = Bun.spawnSync([
      'bun',
      'scripts/verify-release.ts',
      'definitely-missing-release'
    ], {
      cwd: packageRoot,
      env: { ...process.env, ICHIRAN_QUALIFIED_ARTIFACT: 'portable-core-260118-baseline' },
      stderr: 'pipe',
      stdout: 'pipe'
    });

    expect(result.exitCode).not.toBe(0);
    expect(result.stderr.toString()).toContain(
      'Source release verification does not accept ICHIRAN_QUALIFIED_ARTIFACT'
    );
    expect(result.stderr.toString()).not.toContain('ENOENT');
  });
});
