import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';

import {
  expect,
  isExpectedOfflineFetchFailure,
  test,
  watchConsoleHealth
} from './console-health.js';

const SERVICE_WORKER_PATH = resolve(import.meta.dirname, '../dist/sw.js');

test('keeps the old offline shell alive until its tabs close, then cleans it up', async ({
  browser
}) => {
  const original = await readFile(SERVICE_WORKER_PATH, 'utf8');
  let deliberatelyMissingAsset: string | null = null;
  const context = await browser.newContext({
    baseURL: 'http://127.0.0.1:4173',
    serviceWorkers: 'allow',
    viewport: { width: 390, height: 844 }
  });
  watchConsoleHealth(
    context,
    failure => isExpectedOfflineFetchFailure(failure, deliberatelyMissingAsset)
  );
  try {
    const page = await context.newPage();
    await page.goto('/');
    await expect(page.getByRole('heading', { name: 'Japanese analyzer data' })).toBeVisible();
    await expect.poll(() => page.evaluate(() => navigator.serviceWorker.controller !== null))
      .toBe(true);

    const coreSource = /const CORE = (\[[^\n]+\]);/.exec(original)?.[1];
    const cacheName = /const CACHE = '([^']+)';/.exec(original)?.[1];
    if (!coreSource || !cacheName) throw new Error('Finalized Service Worker has no shell identity');
    const core = JSON.parse(coreSource) as string[];
    const lazyAsset = '/licenses.html';
    if (!core.includes(lazyAsset)) throw new Error('Production shell does not cache licenses.html');
    deliberatelyMissingAsset = lazyAsset;
    expect(await page.evaluate(async path => (await fetch(path)).ok, lazyAsset)).toBe(true);

    const nextCache = `${cacheName}-upgrade-test`;
    const upgraded = original
      .replace(`const CACHE = '${cacheName}';`, `const CACHE = '${nextCache}';`)
      .replace(coreSource, JSON.stringify(core.filter(path => path !== lazyAsset)));
    await writeFile(SERVICE_WORKER_PATH, upgraded);

    await page.evaluate(async () => {
      const registration = await navigator.serviceWorker.getRegistration();
      if (!registration) throw new Error('Page has no Service Worker registration');
      await registration.update();
    });
    await expect(page.getByText('App update downloaded', { exact: true })).toBeVisible();
    await expect(page.getByText('Close every analyzer tab', { exact: false })).toBeVisible();
    // The currently controlling shell and its cached manifest still match, so
    // a merely waiting update must not block first-time device data install.
    await expect(page.getByRole('button', { name: 'Install analyzer data' })).toBeEnabled();
    await expect.poll(() => page.evaluate(async () =>
      (await navigator.serviceWorker.getRegistration())?.waiting?.state
    )).toBe('installed');
    expect(await page.evaluate(() => caches.keys())).toEqual(
      expect.arrayContaining([cacheName, nextCache])
    );

    // A fresh page can begin with registration.waiting already populated.
    await page.reload();
    await expect(page.getByText('App update downloaded', { exact: true })).toBeVisible();

    await context.setOffline(true);
    // The old tab remains on its matching generation and can still fetch a
    // lazy chunk that the waiting generation deliberately omitted.
    const oldLazyAsset = await page.evaluate(async path => {
      const response = await fetch(path);
      return { ok: response.ok, bytes: (await response.arrayBuffer()).byteLength };
    }, lazyAsset);
    expect(oldLazyAsset.ok).toBe(true);
    expect(oldLazyAsset.bytes).toBeGreaterThan(0);

    await page.close();
    await expect.poll(async () => {
      for (const worker of context.serviceWorkers()) {
        try {
          const state = await worker.evaluate(() => {
            const scope = globalThis as unknown as {
              readonly registration: ServiceWorkerRegistration;
            };
            return {
              active: scope.registration.active?.state ?? null,
              waiting: scope.registration.waiting?.state ?? null
            };
          });
          if (state.active === 'activated' && state.waiting === null) return true;
        } catch {
          // A redundant Worker may disappear between enumeration and evaluation.
        }
      }
      return false;
    }).toBe(true);
    const reopened = await context.newPage();
    await reopened.goto('/');
    await expect(reopened.getByRole('heading', { name: 'Japanese analyzer data' })).toBeVisible();
    await expect(reopened.getByRole('button', { name: 'Install analyzer data' })).toBeEnabled();
    await expect.poll(() => reopened.evaluate(() => caches.keys())).toEqual([nextCache]);
    expect(await reopened.evaluate(async path => {
      try {
        await fetch(path);
        return false;
      } catch {
        return true;
      }
    }, lazyAsset)).toBe(true);
  } finally {
    await context.setOffline(false).catch(() => undefined);
    await context.close().catch(() => undefined);
    await writeFile(SERVICE_WORKER_PATH, original);
  }
});
