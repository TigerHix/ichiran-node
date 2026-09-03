import { expect, test } from 'bun:test';
import { resolve } from 'node:path';
import ts from 'typescript';

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
    readonly private: boolean;
    readonly bin: Record<string, string>;
    readonly dependencies: Record<string, string>;
    readonly devDependencies: Record<string, string>;
  };
  expect(packageJson.private).toBe(true);
  expect(packageJson.bin['ichiran-data']).toBe('./dist/source-compiler/cli.js');
  expect(packageJson.dependencies['@ichiran/core']).toBe('workspace:*');
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

test('checked-in lock records the private compiler workspace metadata', async () => {
  const repository = resolve(import.meta.dir, '../../..');
  const [packageJson, lockText] = await Promise.all([
    Bun.file(resolve(repository, 'packages/data/package.json')).json(),
    Bun.file(resolve(repository, 'bun.lock')).text()
  ]) as [
    { readonly bin: object; readonly dependencies: Record<string, string>; readonly devDependencies: object },
    string
  ];
  const parsed = ts.parseConfigFileTextToJson('bun.lock', lockText);
  if (parsed.error) {
    throw new Error(ts.flattenDiagnosticMessageText(parsed.error.messageText, '\n'));
  }
  const workspace = parsed.config.workspaces['packages/data'] as {
    readonly bin: object;
    readonly dependencies: Record<string, string>;
    readonly devDependencies: object;
  };
  expect(workspace.bin).toEqual(packageJson.bin);
  expect(workspace.dependencies).toEqual(packageJson.dependencies);
  expect(workspace.devDependencies).toEqual(packageJson.devDependencies);
});

test('keeps direct compilation separate from the Linux isolation proof', async () => {
  const repository = resolve(import.meta.dir, '../../..');
  const rootPackage = await Bun.file(resolve(repository, 'package.json')).json() as {
    readonly scripts: Record<string, string>;
  };
  expect(rootPackage.scripts['source:release']).toBe(
    'bun run build:source-compiler && bun --smol packages/data/dist/source-compiler/cli.js'
  );
  expect(rootPackage.scripts['source:release:isolated']).toBe(
    'sh scripts/source-compiler-release-no-postgres.sh'
  );
  const isolation = await Bun.file(resolve(
    repository,
    'scripts/source-compiler-release-no-postgres.sh'
  )).text();
  expect(isolation).not.toContain('pg_isready');
  expect(isolation).toContain('packages/data/dist/source-compiler/cli.js');
  expect(isolation.indexOf('if [ "${1:-}" = --probe-only ]')).toBeLessThan(
    isolation.indexOf('bun run build:source-compiler')
  );
});

test('private migration command preserves repository-root data paths', async () => {
  const repository = resolve(import.meta.dir, '../../..');
  const rootPackage = await Bun.file(resolve(repository, 'package.json')).json() as {
    readonly scripts: Record<string, string>;
  };
  expect(rootPackage.scripts['migration:data']).toBe('bun packages/data/src/migration-cli.ts');
  const buildDb = await Bun.file(resolve(repository, 'scripts/build-db.sh')).text();
  expect(buildDb).not.toContain('--cwd packages/data');
  expect(buildDb).not.toContain('./data/kanjidic2.xml.gz');
  expect(buildDb).toContain('bun run migration:data -- load-kanjidic 2>&1');
  const migrationCli = await Bun.file(resolve(repository, 'packages/data/src/migration-cli.ts')).text();
  expect(migrationCli).not.toContain('bun run data download');

  const result = Bun.spawnSync([
    process.execPath,
    '-e',
    "import('./packages/data/src/data/conj-rules.ts').then(value => console.log(value.getPosIndex('v1')))"
  ], { cwd: repository, stdout: 'pipe', stderr: 'pipe' });
  expect(result.exitCode).toBe(0);
  expect(result.stderr.toString()).toBe('');
  expect(result.stdout.toString().trim()).toMatch(/^[0-9]+$/);
});
