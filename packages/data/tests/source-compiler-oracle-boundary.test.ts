import { expect, test } from 'bun:test';
import { resolve } from 'node:path';

test('source release graph excludes migration-oracle modules', async () => {
  const script = [
    "Bun.plugin({name:'block-oracles',setup(builder){",
    "builder.onResolve({filter:/(?:browser-pack\\/(?:analyzer-support|analyzer-generated|details|morphology-compiler|root-payload)-oracle|@ichiran\\/reference-postgres|(^|\\/)postgres$)/},",
    "input=>{throw new Error('source release loaded '+input.path)})}});",
    "await import('./packages/data/src/index.ts?oracle-boundary=1');",
    "console.log('source-release-oracle-boundary-ok');"
  ].join('');
  const child = Bun.spawn([process.execPath, '-e', script], {
    cwd: resolve(import.meta.dir, '../../..'),
    stdout: 'pipe',
    stderr: 'pipe'
  });
  const [status, stdout, stderr] = await Promise.all([
    child.exited,
    new Response(child.stdout).text(),
    new Response(child.stderr).text()
  ]);
  expect(stderr).toBe('');
  expect(status).toBe(0);
  expect(stdout.trim()).toBe('source-release-oracle-boundary-ok');
});

test('canonical data package dependencies exclude migration oracles', async () => {
  const packageJson = await Bun.file(resolve(import.meta.dir, '../package.json')).json() as {
    readonly bin: Record<string, string>;
    readonly dependencies: Record<string, string>;
    readonly devDependencies: Record<string, string>;
  };
  expect(packageJson.bin['ichiran-data']).toBe('./dist/source-compiler/cli.js');
  expect(Object.keys(packageJson.dependencies).sort()).toEqual([
    '@ichiran/core',
    'csv-parse',
    'fast-xml-parser'
  ]);
  expect(packageJson.devDependencies).toMatchObject({
    '@ichiran/reference-postgres': '0.0.0',
    postgres: '^3.4.7'
  });
});

test('keeps direct compilation separate from the Linux isolation proof', async () => {
  const repository = resolve(import.meta.dir, '../../..');
  const rootPackage = await Bun.file(resolve(repository, 'package.json')).json() as {
    readonly scripts: Record<string, string>;
  };
  expect(rootPackage.scripts['source:release']).toBe(
    'bun --smol scripts/source-compiler-release.ts'
  );
  expect(rootPackage.scripts['source:release:isolated']).toBe(
    'sh scripts/source-compiler-release-no-postgres.sh'
  );
  const isolation = await Bun.file(resolve(
    repository,
    'scripts/source-compiler-release-no-postgres.sh'
  )).text();
  expect(isolation).not.toContain('pg_isready');
});
