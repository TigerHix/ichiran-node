import { expect, test } from 'bun:test';
import { resolve } from 'node:path';

test('source release graph excludes migration-oracle modules', async () => {
  const script = [
    "Bun.plugin({name:'block-oracles',setup(builder){",
    "builder.onResolve({filter:/(?:browser-pack\\/(?:analyzer-support|analyzer-generated|details|morphology-compiler|root-payload)-oracle|@ichiran\\/reference-postgres|(^|\\/)postgres$)/},",
    "input=>{throw new Error('source release loaded '+input.path)})}});",
    "await import('./packages/data/src/source-compiler/release-output.ts?oracle-boundary=1');",
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
