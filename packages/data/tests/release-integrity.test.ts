import { afterEach, describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import {
  lstat,
  mkdir,
  mkdtemp,
  readFile,
  readdir,
  rename,
  rm,
  symlink,
  writeFile
} from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

import {
  assertBrowserAlphaDatabaseIdentity,
  normalizePgDump16Schema,
  pgDumpSchemaInvocation
} from '../src/browser-pack/database-identity.js';
import {
  assertActiveReleaseGeneration,
  assertExactReleaseInventory,
  analyzerReleaseGenerationIdentity,
  publishAnalyzerRelease
} from '../src/browser-pack/release-publication.js';
import { measureProductionShell } from '../src/browser-pack/shell-measurement.js';
import {
  assertSourceReleaseDestination,
  resolveSourceReleaseDestination
} from '../src/source-compiler/release.js';

const temporaryDirectories: string[] = [];

afterEach(async () => {
  await Promise.all(temporaryDirectories.splice(0).map(directory => rm(directory, {
    recursive: true,
    force: true
  })));
});

async function temporary(): Promise<string> {
  const directory = await mkdtemp(join(tmpdir(), 'ichiran-release-integrity-'));
  temporaryDirectories.push(directory);
  return directory;
}

describe('atomic release publication', () => {
  test('keeps the old generation active when publication faults before activation', async () => {
    const root = await temporary();
    const output = join(root, 'browser-alpha');
    const first = new Map([['manifest.json', new TextEncoder().encode('first')]]);
    const second = new Map([['manifest.json', new TextEncoder().encode('second')]]);
    const verify = async (directory: string) => {
      await assertExactReleaseInventory(directory, ['manifest.json']);
    };
    await publishAnalyzerRelease(output, first, { verify });
    expect((await lstat(output)).isSymbolicLink()).toBeTrue();
    await expect(assertActiveReleaseGeneration(output, ['manifest.json'])).resolves.toBeUndefined();
    expect(await readFile(join(output, 'manifest.json'), 'utf8')).toBe('first');

    await expect(publishAnalyzerRelease(output, second, {
      verify,
      beforeActivate: () => { throw new Error('injected activation fault'); }
    })).rejects.toThrow('injected activation fault');
    expect(await readFile(join(output, 'manifest.json'), 'utf8')).toBe('first');

    await publishAnalyzerRelease(output, second, { verify });
    expect(await readFile(join(output, 'manifest.json'), 'utf8')).toBe('second');
  });

  test('leaves an absent output absent when first activation faults', async () => {
    const root = await temporary();
    const output = join(root, 'browser-alpha');
    const files = new Map([['manifest.json', new TextEncoder().encode('first')]]);
    const verify = async (directory: string) => {
      await assertExactReleaseInventory(directory, ['manifest.json']);
    };

    await expect(publishAnalyzerRelease(output, files, {
      verify,
      beforeActivate: () => { throw new Error('injected first activation fault'); }
    })).rejects.toThrow('injected first activation fault');
    await expect(lstat(output)).rejects.toMatchObject({ code: 'ENOENT' });

    await publishAnalyzerRelease(output, files, { verify });
    await expect(assertActiveReleaseGeneration(output, ['manifest.json']))
      .resolves.toBeUndefined();
  });

  test('does not overwrite a regular file that appears before first activation', async () => {
    const root = await temporary();
    const output = join(root, 'browser-alpha');
    const files = new Map([['manifest.json', new TextEncoder().encode('release')]]);

    await expect(publishAnalyzerRelease(output, files, {
      verify: async () => undefined,
      beforeActivate: async () => { await writeFile(output, 'foreign'); }
    })).rejects.toThrow('appeared before exclusive first activation');
    expect((await lstat(output)).isFile()).toBeTrue();
    expect(await readFile(output, 'utf8')).toBe('foreign');
  });

  test('replaces an active release only while its exact symlink is still owned', async () => {
    const root = await temporary();
    const output = join(root, 'browser-alpha');
    const first = new Map([['manifest.json', new TextEncoder().encode('first')]]);
    const second = new Map([['manifest.json', new TextEncoder().encode('second')]]);
    await publishAnalyzerRelease(output, first, { verify: async () => undefined });

    await expect(publishAnalyzerRelease(output, second, {
      verify: async () => undefined,
      beforeActivate: async () => {
        await rm(output);
        await writeFile(output, 'foreign');
      }
    })).rejects.toThrow('changed before release activation');
    expect((await lstat(output)).isFile()).toBeTrue();
    expect(await readFile(output, 'utf8')).toBe('foreign');
  });

  test('publishes to a fresh nested destination', async () => {
    const root = await temporary();
    const output = join(root, 'new', 'nested', 'browser-alpha');
    const files = new Map([['manifest.json', new TextEncoder().encode('first')]]);
    await publishAnalyzerRelease(output, files, { verify: async () => undefined });
    expect(await readFile(join(output, 'manifest.json'), 'utf8')).toBe('first');
  });

  test('rejects a historical flat output without moving or writing anything', async () => {
    const root = await temporary();
    const output = join(root, 'browser-alpha');
    await mkdir(output);
    await writeFile(join(output, 'manifest.json'), 'historical bytes');
    const files = new Map([['manifest.json', new TextEncoder().encode('new bytes')]]);

    await expect(publishAnalyzerRelease(output, files, { verify: async () => undefined }))
      .rejects.toThrow('must be absent or an atomic release symlink');
    expect((await lstat(output)).isDirectory()).toBeTrue();
    expect(await readFile(join(output, 'manifest.json'), 'utf8')).toBe('historical bytes');
    await expect(lstat(`${output}.generations`)).rejects.toMatchObject({ code: 'ENOENT' });
  });

  test('rejects a symlinked generations root before writing outside the destination', async () => {
    const root = await temporary();
    const outside = await temporary();
    const output = join(root, 'browser-alpha');
    await symlink(outside, `${output}.generations`, 'dir');
    const files = new Map([['manifest.json', new TextEncoder().encode('new bytes')]]);

    await expect(publishAnalyzerRelease(output, files, { verify: async () => undefined }))
      .rejects.toThrow('real directory, never a symlink');
    expect(await readdir(outside)).toEqual([]);
    await expect(lstat(output)).rejects.toMatchObject({ code: 'ENOENT' });
  });

  test('rejects symlinked artifacts in existing and active generations', async () => {
    const root = await temporary();
    const output = join(root, 'browser-alpha');
    const bytes = new TextEncoder().encode('expected bytes');
    const files = new Map([['manifest.json', bytes]]);
    const generations = `${output}.generations`;
    const generation = join(generations, analyzerReleaseGenerationIdentity(files));
    const external = join(root, 'external-manifest.json');
    await mkdir(generation, { recursive: true });
    await writeFile(external, bytes);
    await symlink(external, join(generation, 'manifest.json'));

    await expect(publishAnalyzerRelease(output, files, { verify: async () => undefined }))
      .rejects.toThrow('not a regular file');
    await rm(generation, { recursive: true });
    await publishAnalyzerRelease(output, files, { verify: async () => undefined });
    await rm(join(generation, 'manifest.json'));
    await symlink(external, join(generation, 'manifest.json'));
    await expect(assertActiveReleaseGeneration(output, ['manifest.json']))
      .rejects.toThrow('not a regular file');
  });

  test.skipIf(process.platform === 'win32')(
    'rejects a FIFO artifact without blocking on open',
    async () => {
      const root = await mkdtemp('/tmp/ichiran-release-fifo-');
      temporaryDirectories.push(root);
      const output = join(root, 'browser-alpha');
      const files = new Map([['manifest.json', new TextEncoder().encode('expected bytes')]]);
      const generation = join(
        `${output}.generations`,
        analyzerReleaseGenerationIdentity(files)
      );
      await mkdir(generation, { recursive: true });
      const fifo = Bun.spawnSync(['mkfifo', join(generation, 'manifest.json')], {
        stdout: 'pipe',
        stderr: 'pipe'
      });
      expect(fifo.exitCode).toBe(0);

      const started = performance.now();
      await expect(publishAnalyzerRelease(output, files, { verify: async () => undefined }))
        .rejects.toThrow('not a regular file');
      expect(performance.now() - started).toBeLessThan(1_000);
    }
  );

  test('rejects physical destination drift through a swapped work ancestor', async () => {
    const root = await temporary();
    const repository = join(root, 'repository');
    const work = join(repository, 'work');
    const selectedParent = join(work, 'selected');
    const movedParent = join(work, 'selected-before-swap');
    const otherParent = join(work, 'other');
    await Promise.all([
      mkdir(selectedParent, { recursive: true }),
      mkdir(otherParent, { recursive: true })
    ]);
    const selected = await resolveSourceReleaseDestination(
      repository,
      'work/selected/release'
    );
    await rename(selectedParent, movedParent);
    await symlink('other', selectedParent, 'dir');
    const files = new Map([['manifest.json', new TextEncoder().encode('release')]]);

    await expect(publishAnalyzerRelease(selected.lexical, files, {
      beforeWrite: async () => { await assertSourceReleaseDestination(repository, selected); },
      verify: async () => undefined
    })).rejects.toThrow('physical output changed before publication');
    expect(await readdir(otherParent)).toEqual([]);
  });

  test('rejects extra files in a release generation', async () => {
    const root = await temporary();
    await writeFile(join(root, 'manifest.json'), 'manifest');
    await writeFile(join(root, 'surprise.txt'), 'extra');
    await expect(assertExactReleaseInventory(root, ['manifest.json']))
      .rejects.toThrow('inventory mismatch');
  });

  test('makes identical concurrent publication idempotent', async () => {
    const root = await temporary();
    const output = join(root, 'browser-alpha');
    const files = new Map([['manifest.json', new TextEncoder().encode('same bytes')]]);
    const verify = async (directory: string) => {
      await assertExactReleaseInventory(directory, ['manifest.json']);
    };

    await Promise.all([
      publishAnalyzerRelease(output, files, { verify }),
      publishAnalyzerRelease(output, files, { verify })
    ]);
    await expect(assertActiveReleaseGeneration(output, ['manifest.json']))
      .resolves.toBeUndefined();
    expect(await readFile(join(output, 'manifest.json'), 'utf8')).toBe('same bytes');
  });
});

describe('production shell measurement', () => {
  test('derives bytes and a hash, and requires the release-finalized cache version', async () => {
    const root = await temporary();
    await mkdir(join(root, 'assets'));
    await writeFile(join(root, 'index.html'), '<main>demo</main>');
    await writeFile(join(root, 'assets/app.js'), 'console.log("demo")');
    const precache = ['/assets/app.js', '/index.html', '/analyzer/manifest.json'];
    const worker = (version: string) => [
      `const CACHE = 'ichiran-shell-${version}';`,
      `const CORE = ${JSON.stringify(precache)};`
    ].join('\n');
    await writeFile(join(root, 'sw.js'), worker('0'.repeat(16)));
    const manifest = new TextEncoder().encode('{"release":1}\n');
    const projected = await measureProductionShell(root, manifest);
    expect(projected.bytes).toBeGreaterThan(0);
    expect(projected.sha256).toMatch(/^[0-9a-f]{64}$/);
    await expect(measureProductionShell(root, manifest, {
      requireFinalizedServiceWorker: true
    })).rejects.toThrow('cache version');

    await writeFile(join(root, 'sw.js'), worker(projected.cacheVersion));
    expect(await measureProductionShell(root, manifest, {
      requireFinalizedServiceWorker: true
    })).toEqual(projected);

    await writeFile(join(root, 'assets/app.js'), 'console.log("tampered")');
    await expect(measureProductionShell(root, manifest, {
      requireFinalizedServiceWorker: true
    })).rejects.toThrow('cache version');
  });
});

describe('locked database identity', () => {
  test('keeps database credentials out of pg_dump arguments', () => {
    const database = 'postgresql://oracle:secret-value@database.example/ichiran';
    const invocation = pgDumpSchemaInvocation(database);
    expect(invocation.args).toEqual([
      '--schema-only',
      '--no-owner',
      '--no-privileges',
      '--dbname',
      'postgresql://oracle@database.example/ichiran'
    ]);
    expect(invocation.args.join(' ')).not.toContain('secret-value');
    expect(invocation.env.PGPASSWORD === 'secret-value').toBe(true);
  });

  test('keeps a password-free local socket URL usable', () => {
    const database = 'postgresql:///ichiran?host=%2Fvar%2Frun%2Fpostgresql';
    const invocation = pgDumpSchemaInvocation(database);
    expect(invocation.args.at(-1)).toBe(database);
  });

  test('rejects an SSL key password before constructing pg_dump arguments', () => {
    expect(() => pgDumpSchemaInvocation(
      'postgresql://oracle@database.example/ichiran?sslpassword=secret-value'
    )).toThrow('Analyzer database URLs do not support sslpassword');
  });

  test('normalizes only pg_dump session keys and rejects a different schema digest', () => {
    const dump = '-- header\n\\restrict random-one\nCREATE TABLE x();\n\\unrestrict random-one\n';
    expect(new TextDecoder().decode(normalizePgDump16Schema(dump)))
      .toBe('-- header\nCREATE TABLE x();\n');
    const digest = createHash('sha256').update('schema').digest('hex');
    const actual = {
      name: 'oracle',
      postgresServerVersion: '16.15',
      encoding: 'UTF8',
      collation: 'ja_JP.utf8',
      ctype: 'ja_JP.utf8',
      readOnly: true,
      schemaNormalization: 'pg-dump-16-schema-v1' as const,
      schemaSha256: digest
    };
    expect(() => assertBrowserAlphaDatabaseIdentity(actual, {
      name: actual.name,
      postgresServerVersion: actual.postgresServerVersion,
      encoding: actual.encoding,
      collation: actual.collation,
      ctype: actual.ctype,
      schemaNormalization: actual.schemaNormalization,
      schemaSha256: '0'.repeat(64)
    })).toThrow('Database schema digest');
  });
});
