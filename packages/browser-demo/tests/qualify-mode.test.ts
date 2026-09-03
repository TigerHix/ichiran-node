import { describe, expect, test } from 'bun:test';
import { readFile } from 'node:fs/promises';
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

  test('long source-pack qualifiers pin HEAD and cleanliness for the complete run', async () => {
    const repository = resolve(packageRoot, '..', '..');
    const files = [
      'scripts/rust-kernel-source-release-differential.sh',
      'scripts/rust-kernel-source-release-c-qualification.sh',
      'scripts/source-release-host-qualification.sh'
    ];
    for (const file of files) {
      const source = await readFile(resolve(repository, file), 'utf8');
      expect(source).toContain('qualification_commit=$(git rev-parse HEAD)');
      expect(source).toContain('test "$(git rev-parse HEAD)" = "$qualification_commit"');
      expect(source.match(/git status --porcelain=v1/g)).toHaveLength(2);
    }
    const browser = await readFile(resolve(packageRoot, 'scripts/qualify.ts'), 'utf8');
    expect(browser).not.toContain('--skip-e2e');
    expect(browser).toContain("await run('bun', ['run', 'test:e2e']");
    expect(browser).toContain("const qualificationCommit = gitOutput(['rev-parse', 'HEAD']);");
    expect(browser.match(/assertCleanCheckout\(qualificationCommit\)/g)).toHaveLength(2);
  });

  test('source host qualification runs the pinned upstream regression on its release', async () => {
    const repository = resolve(packageRoot, '..', '..');
    const source = await readFile(resolve(
      repository,
      'scripts/source-release-host-qualification.sh'
    ), 'utf8');
    expect(source).toContain(
      'RUN_PARITY_TESTS=true ICHIRAN_PACK_DIR="$release" bun test'
    );
    expect(source).toContain('packages/cli/tests/upstream-260118-parity.test.ts');
  });
});
