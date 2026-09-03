import { describe, expect, test } from 'bun:test';
import { resolve } from 'node:path';

const releaseDirectory = process.env.ICHIRAN_PACK_DIR;
const packageRoot = resolve(import.meta.dir, '..');

function run(...arguments_: string[]) {
  return Bun.spawnSync(['node', 'dist/index.js', ...arguments_], {
    cwd: packageRoot,
    env: { ...process.env, ICHIRAN_PACK_DIR: releaseDirectory! },
    stderr: 'pipe',
    stdout: 'pipe'
  });
}

describe.skipIf(!releaseDirectory)('CLI source release', () => {
  test('exposes explicit analyze, romanize, details, and entry commands', () => {
    const romanize = run('romanize', '今日');
    expect(romanize.exitCode).toBe(0);
    expect(romanize.stderr.toString()).toBe('');
    expect(romanize.stdout.toString()).toBe('kyō\n');

    const analyze = run('analyze', '--limit', '1', '食べた');
    expect(analyze.exitCode).toBe(0);
    const result = JSON.parse(analyze.stdout.toString()) as {
      readonly input: string;
      readonly paths: readonly { readonly tokens: readonly { readonly entryIndex: number | null }[] }[];
    };
    expect(result.input).toBe('食べた');
    const entryIndex = result.paths[0]?.tokens.find(token => token.entryIndex !== null)?.entryIndex;
    expect(entryIndex).toBeNumber();

    const entry = run('entry', String(entryIndex));
    expect(entry.exitCode).toBe(0);
    expect(JSON.parse(entry.stdout.toString())).toMatchObject({ seq: expect.any(Number) });

    const details = run('details', '--limit', '1', '--path', '0', '--token', '0', '食べた');
    expect(details.exitCode).toBe(0);
    expect(JSON.parse(details.stdout.toString())).toMatchObject({
      text: '食べた',
      meanings: [],
      conjugations: expect.any(Array)
    });
  });

  test('reports stable analyzer errors and has no implicit compatibility mode', () => {
    const invalid = run('analyze', '--limit', '99', '猫');
    expect(invalid.exitCode).toBe(2);
    expect(invalid.stderr.toString()).toContain('ERROR [invalid-input]');

    const implicit = run('今日');
    expect(implicit.exitCode).not.toBe(0);
    expect(implicit.stderr.toString()).toContain("unknown command '今日'");
  });
});
