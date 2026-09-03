import { expect, test } from 'bun:test';
import { chmod, mkdtemp, rm, writeFile } from 'node:fs/promises';
import { createServer } from 'node:net';
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
    readonly bin?: Record<string, string>;
    readonly dependencies: Record<string, string>;
    readonly devDependencies: Record<string, string>;
  };
  expect(packageJson.private).toBe(true);
  expect(packageJson.bin).toBeUndefined();
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
    readonly packageManager: string;
    readonly scripts: Record<string, string>;
  };
  expect(rootPackage.packageManager).toBe('bun@1.3.5');
  expect(rootPackage.scripts['source:release']).toBe(
    'sh scripts/source-compiler-release.sh'
  );
  expect(rootPackage.scripts.data).toBeUndefined();
  expect(rootPackage.scripts['source:release:isolated']).toBe(
    'sh scripts/source-compiler-release-no-postgres.sh'
  );
  const dataPackage = await Bun.file(resolve(repository, 'packages/data/package.json')).json() as {
    readonly scripts: Record<string, string>;
  };
  expect(dataPackage.scripts['test:source']).toBe('bun scripts/test-source.ts');
  const isolation = await Bun.file(resolve(
    repository,
    'scripts/source-compiler-release-no-postgres.sh'
  )).text();
  expect(isolation).not.toContain('pg_isready');
  expect(isolation).toContain('scripts/source-compiler-release.sh');
  expect(isolation).toContain('mount --bind "$SOURCE_COMPILER_PRIVATE_TMP_DIRECTORY" /tmp');
  expect(isolation).toContain('/var/tmp/ichiran-source-private-tmp.XXXXXX');
  expect(isolation).not.toContain('mount -t tmpfs');
  expect(isolation).toContain('requires executable private /tmp backing storage');
  expect(isolation.indexOf('sc_exec_probe=/tmp/')).toBeLessThan(
    isolation.indexOf('if [ "${1:-}" = --probe-only ]')
  );
  expect(isolation).toContain('/run/postgresql /var/run/postgresql /tmp');
  expect(isolation).toContain("sc_output_physical=$(realpath -m \"$sc_output_candidate\")");
  expect(isolation.indexOf('if [ "${1:-}" = --probe-only ]')).toBeLessThan(
    isolation.indexOf('scripts/source-compiler-release.sh')
  );
  const release = await Bun.file(resolve(repository, 'scripts/source-compiler-release.sh')).text();
  expect(release).toContain('qualified_head=$(git rev-parse HEAD)');
  expect(release).toContain('required_bun_version=1.3.5');
  expect(release).toContain('cargo +1.92.0 --version');
  expect(release).toContain('ICHIRAN_SOURCE_COMPILER_COMMIT="$qualified_head"');
  expect(release.match(/assert_source_checkout/g)?.length).toBeGreaterThanOrEqual(4);
  const packageAudit = await Bun.file(resolve(repository, 'scripts/audit-data-package.sh')).text();
  expect(packageAudit).toContain('@ichiran/grammar');
});

test.skipIf(process.platform !== 'linux')(
  'PostgreSQL isolation hides a host tmp socket behind a private writable disk bind',
  async () => {
    const repository = resolve(import.meta.dir, '../../..');
    const hostDirectory = await mkdtemp('/tmp/ichiran-host-socket-');
    const socketPath = resolve(hostDirectory, '.s.PGSQL.5432');
    const server = createServer();
    try {
      await new Promise<void>((resolveListen, reject) => {
        server.once('error', reject);
        server.listen(socketPath, resolveListen);
      });
      const result = Bun.spawnSync([
        '/bin/sh',
        resolve(repository, 'scripts/source-compiler-release-no-postgres.sh'),
        '--probe-only'
      ], {
        cwd: repository,
        env: { ...process.env, SOURCE_COMPILER_HOST_TMP_PROBE: socketPath },
        stdout: 'pipe',
        stderr: 'pipe'
      });
      expect(result.exitCode).toBe(0);
      expect(result.stderr.toString()).toBe('');
      expect(JSON.parse(result.stdout.toString())).toMatchObject({
        postgresqlUnavailable: true,
        unixSockets: {
          runPostgresql: 'masked',
          varRunPostgresql: 'masked',
          tmp: 'private-disk-bind'
        },
        temporaryStorage: 'writable-executable'
      });
    } finally {
      await new Promise<void>(resolveClose => server.close(() => resolveClose()));
      await rm(hostDirectory, { recursive: true, force: true });
    }
  }
);

test.skipIf(process.platform !== 'linux')(
  'PostgreSQL isolation rejects the last repeated output when it is private temp',
  () => {
    const repository = resolve(import.meta.dir, '../../..');
    const result = Bun.spawnSync([
      '/bin/sh',
      resolve(repository, 'scripts/source-compiler-release-no-postgres.sh'),
      '--probe-only',
      '--out',
      'work/apparently-safe-source-release',
      '--out',
      '/tmp/ephemeral-source-release'
    ], { cwd: repository, stdout: 'pipe', stderr: 'pipe' });
    expect(result.exitCode).not.toBe(0);
    expect(result.stderr.toString()).toContain(
      'PostgreSQL-unavailable release output cannot be under private /tmp'
    );
    expect(result.stdout.toString()).toBe('');
  }
);

test('source release wrapper fails before building under an unpinned Bun', async () => {
  const repository = resolve(import.meta.dir, '../../..');
  const fakeBin = await mkdtemp('/tmp/ichiran-fake-bun-');
  try {
    const bun = resolve(fakeBin, 'bun');
    await writeFile(bun, '#!/bin/sh\necho 9.9.9\n');
    await chmod(bun, 0o755);
    const result = Bun.spawnSync([
      '/bin/sh',
      resolve(repository, 'scripts/source-compiler-release.sh'),
      '--probe-only'
    ], {
      cwd: repository,
      env: { ...process.env, PATH: fakeBin },
      stdout: 'pipe',
      stderr: 'pipe'
    });
    expect(result.exitCode).not.toBe(0);
    expect(result.stderr.toString()).toContain(
      'Source compiler release requires Bun 1.3.5; found 9.9.9'
    );
    expect(result.stdout.toString()).toBe('');
  } finally {
    await rm(fakeBin, { recursive: true, force: true });
  }
});

test('source release wrapper preflights the pinned Rust toolchain before building', async () => {
  const repository = resolve(import.meta.dir, '../../..');
  const fakeBin = await mkdtemp('/tmp/ichiran-no-cargo-');
  try {
    const bun = resolve(fakeBin, 'bun');
    await writeFile(bun, '#!/bin/sh\necho 1.3.5\n');
    await chmod(bun, 0o755);
    const result = Bun.spawnSync([
      '/bin/sh',
      resolve(repository, 'scripts/source-compiler-release.sh'),
      '--probe-only'
    ], {
      cwd: repository,
      env: { ...process.env, PATH: fakeBin },
      stdout: 'pipe',
      stderr: 'pipe'
    });
    expect(result.exitCode).not.toBe(0);
    expect(result.stderr.toString()).toContain(
      'requires cargo from rustup with toolchain 1.92.0'
    );
    expect(result.stdout.toString()).toBe('');
  } finally {
    await rm(fakeBin, { recursive: true, force: true });
  }
});

test('source compiler rejects a mismatched wrapper commit handshake', () => {
  const repository = resolve(import.meta.dir, '../../..');
  const result = Bun.spawnSync([
    process.execPath,
    'packages/data/src/source-compiler/cli.ts',
    'baseline',
    '--out',
    'work/mismatched-compiler-head',
    '--pack-version',
    'mismatched-head-test'
  ], {
    cwd: repository,
    env: { ...process.env, ICHIRAN_SOURCE_COMPILER_COMMIT: '0'.repeat(40) },
    stdout: 'pipe',
    stderr: 'pipe'
  });
  expect(result.exitCode).not.toBe(0);
  expect(result.stderr.toString()).toContain('does not match HEAD');
});

test('source compiler reserves lock selection for explicit update mode', () => {
  const repository = resolve(import.meta.dir, '../../..');
  const run = (args: readonly string[]) => Bun.spawnSync([
    process.execPath,
    'packages/data/src/source-compiler/cli.ts',
    ...args
  ], { cwd: repository, stdout: 'pipe', stderr: 'pipe' });
  const baseline = run([
    'baseline', '--out', 'work/release', '--pack-version', 'test',
    '--source-lock', 'work/operator-selected.lock.json'
  ]);
  expect(baseline.exitCode).not.toBe(0);
  expect(baseline.stderr.toString()).toContain(
    'baseline uses data/source-compiler-sources.lock.json and does not accept --source-lock'
  );
  const update = run(['update', '--out', 'work/release', '--pack-version', 'test']);
  expect(update.exitCode).not.toBe(0);
  expect(update.stderr.toString()).toContain('update requires --source-lock');
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
