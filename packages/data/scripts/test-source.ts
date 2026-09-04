import { readdir } from 'node:fs/promises';
import { resolve } from 'node:path';

const packageRoot = resolve(import.meta.dir, '..');
const testDirectory = resolve(packageRoot, 'tests');
const excluded = new Set([
  'analyzer-support-postgres.test.ts',
  'conjugate.test.ts',
  'dictionary-postgres.test.ts',
  'load-entry.test.ts',
  'root-payload-postgres.test.ts'
]);
const names = (await readdir(testDirectory)).filter(name => name.endsWith('.test.ts'));
for (const name of excluded) {
  if (!names.includes(name)) throw new Error(`Missing PostgreSQL-only test file: ${name}`);
}
const files = names
  .filter(name => !excluded.has(name))
  .sort()
  .map(name => resolve(testDirectory, name));
if (files.length === 0) throw new Error('No PostgreSQL-free data tests were found');

const child = Bun.spawn([process.execPath, 'test', ...files], {
  cwd: packageRoot,
  env: {
    ...process.env,
    ICHIRAN_RUN_DATABASE_TESTS: 'false',
    RUN_ANALYZER_SUPPORT_POSTGRES: 'false',
    RUN_DICTIONARY_POSTGRES: 'false',
    RUN_ROOT_PAYLOAD_POSTGRES: 'false'
  },
  stdio: ['inherit', 'inherit', 'inherit']
});
process.exit(await child.exited);
