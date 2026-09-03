import { createHash } from 'node:crypto';
import { spawn, type ChildProcess } from 'node:child_process';
import { readFile } from 'node:fs/promises';
import { gzipSync } from 'node:zlib';
import type {
  Browser,
  BrowserContext,
  Page,
  Route
} from 'playwright/test';
import type {
  AnalyzerPackManifest,
  PackAssetManifest
} from '../src/protocol.js';
import {
  expect,
  watchConsoleHealth
} from './console-health.js';

export const BASE_URL = 'http://127.0.0.1:4173';
export const DIRECTORY_NAME = 'ichiran-browser-alpha';
export const INSTALL_ID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/;

export interface RoutedAsset {
  readonly manifest: PackAssetManifest;
  readonly body: Uint8Array;
}

export interface OpfsSnapshot {
  readonly markerBytes: number | null;
  readonly hotBytes: number | null;
  readonly detailsBytes: number | null;
  readonly downloadBytes: number | null;
}

export interface WorkerCalibrationSample {
  readonly ms: number;
  readonly state: number;
}

export interface WorkerHeapUsage {
  readonly usedSize: number;
  readonly totalSize: number;
  readonly embedderHeapUsedSize: number;
  readonly backingStorageSize: number;
}

export function median(values: readonly number[]): number {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.floor(sorted.length / 2)]!;
}

export async function singleCpuAffinity(): Promise<number> {
  if (process.platform !== 'linux') {
    throw new Error('The browser performance gate requires Linux CPU affinity');
  }
  const value = /^Cpus_allowed_list:\s*(.+)$/m
    .exec(await readFile('/proc/self/status', 'utf8'))?.[1]?.trim();
  if (!value || !/^\d+$/.test(value)) {
    throw new Error(
      `Playwright must run on exactly one CPU via scripts/run-e2e.ts; got ${value ?? '(missing)'}`
    );
  }
  const cpu = Number(value);
  if (process.env.ICHIRAN_E2E_AFFINITY_CPU !== String(cpu)) {
    throw new Error('Playwright CPU affinity does not match the E2E wrapper');
  }
  return cpu;
}

export async function attachAnalyzerWorker(browser: Browser): Promise<{
  readonly target: { readonly type: string; readonly title: string; readonly url: string };
  readonly samples: (count: number) => Promise<readonly WorkerCalibrationSample[]>;
  readonly heapUsage: () => Promise<WorkerHeapUsage>;
  readonly collectGarbage: () => Promise<void>;
  readonly close: () => Promise<void>;
}> {
  const cdp = await browser.newBrowserCDPSession();
  let matches: readonly { readonly targetId: string; readonly type: string; readonly title: string; readonly url: string }[] = [];
  for (let attempt = 0; attempt < 100; attempt++) {
    const { targetInfos } = await cdp.send('Target.getTargets');
    matches = targetInfos.filter(target =>
      target.type === 'worker'
      && target.title === 'ichiran-analyzer'
      && target.url.includes('/assets/analyzer.worker-'));
    if (matches.length === 1) break;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  if (matches.length !== 1) {
    await cdp.detach();
    throw new Error(`Expected one exact ichiran-analyzer Worker target; found ${matches.length}`);
  }
  const target = matches[0]!;
  const { sessionId } = await cdp.send('Target.attachToTarget', {
    targetId: target.targetId,
    flatten: false
  });
  interface NestedResponse {
    readonly id: number;
    readonly result?: Record<string, unknown>;
    readonly error?: { readonly code: number; readonly message: string };
  }
  let nextId = 0;
  const pending = new Map<number, (response: NestedResponse) => void>();
  const receive = (event: { readonly sessionId: string; readonly message: string }): void => {
    if (event.sessionId !== sessionId) return;
    const response = JSON.parse(event.message) as NestedResponse;
    pending.get(response.id)?.(response);
    pending.delete(response.id);
  };
  cdp.on('Target.receivedMessageFromTarget', receive);
  const send = async (
    method: string,
    params: Readonly<Record<string, unknown>> = {}
  ): Promise<Record<string, unknown>> => {
    const id = ++nextId;
    const response = new Promise<NestedResponse>(resolve => pending.set(id, resolve));
    await cdp.send('Target.sendMessageToTarget', {
      sessionId,
      message: JSON.stringify({ id, method, params })
    });
    const value = await response;
    if (value.error) throw new Error(`${method}: ${value.error.message}`);
    return value.result ?? {};
  };
  await send('Runtime.enable');

  const expression = `(() => {
    const start = performance.now();
    let state = 0x12345678 | 0;
    for (let index = 0; index < 60000000; index++) {
      state = (Math.imul(state ^ index, 1664525) + 1013904223) | 0;
    }
    return { ms: performance.now() - start, state };
  })()`;
  return {
    target: { type: target.type, title: target.title, url: target.url },
    async samples(count) {
      const result: WorkerCalibrationSample[] = [];
      for (let index = 0; index < count; index++) {
        const evaluated = await send('Runtime.evaluate', {
          expression,
          returnByValue: true,
          awaitPromise: true
        });
        const remote = evaluated.result as { readonly value?: unknown } | undefined;
        const sample = remote?.value as WorkerCalibrationSample | undefined;
        if (
          !sample
          || !Number.isFinite(sample.ms)
          || !Number.isSafeInteger(sample.state)
        ) {
          throw new Error('Exact analyzer Worker returned an invalid CPU calibration sample');
        }
        result.push(sample);
      }
      return result;
    },
    async heapUsage() {
      const value = await send('Runtime.getHeapUsage');
      const number = (key: keyof WorkerHeapUsage): number => {
        const result = value[key];
        if (typeof result !== 'number' || !Number.isFinite(result)) {
          throw new Error(`Analyzer Worker heap metric ${key} is unavailable`);
        }
        return result;
      };
      return {
        usedSize: number('usedSize'),
        totalSize: number('totalSize'),
        embedderHeapUsedSize: number('embedderHeapUsedSize'),
        backingStorageSize: number('backingStorageSize')
      };
    },
    async collectGarbage() {
      await send('HeapProfiler.collectGarbage');
    },
    async close() {
      cdp.off('Target.receivedMessageFromTarget', receive);
      await cdp.send('Target.detachFromTarget', { sessionId });
      await cdp.detach();
    }
  };
}

export async function startCpuHogs(cpu: number, count: number): Promise<readonly ChildProcess[]> {
  const source = `
    const cell = new Int32Array(new SharedArrayBuffer(4));
    let state = 1;
    for (;;) {
      for (let index = 0; index < 1000000; index++) {
        state = Math.imul(state ^ index, 1664525) | 0;
      }
      Atomics.store(cell, 0, state);
    }
  `;
  const children = Array.from({ length: count }, () =>
    spawn(process.execPath, ['-e', source], { stdio: 'ignore' }));
  try {
    await new Promise(resolve => setTimeout(resolve, 500));
    for (const child of children) {
      if (!child.pid || child.exitCode !== null) {
        throw new Error('CPU contention peer failed to start');
      }
      const affinity = /^Cpus_allowed_list:\s*(.+)$/m
        .exec(await readFile(`/proc/${child.pid}/status`, 'utf8'))?.[1]?.trim();
      if (affinity !== String(cpu)) {
        throw new Error(`CPU contention peer affinity ${affinity ?? '(missing)'} != ${cpu}`);
      }
    }
    return children;
  } catch (error) {
    await stopCpuHogs(children);
    throw error;
  }
}

export async function stopCpuHogs(children: readonly ChildProcess[]): Promise<void> {
  const exits = children.map(child => child.exitCode !== null
    ? Promise.resolve()
    : new Promise<void>(resolveExit => child.once('exit', () => resolveExit())));
  for (const child of children) child.kill('SIGTERM');
  await Promise.race([
    Promise.all(exits),
    new Promise(resolve => setTimeout(resolve, 1_000))
  ]);
  for (const child of children) {
    if (child.exitCode === null) child.kill('SIGKILL');
  }
  // WSL can delay child exit notifications after SIGKILL indefinitely. The
  // E2E wrapper owns the whole process group and performs the final reap, so
  // cleanup here must remain bounded rather than consuming the test watchdog.
  await Promise.race([
    Promise.all(exits),
    new Promise(resolve => setTimeout(resolve, 1_000))
  ]);
  for (const child of children) {
    // A SIGKILLed Bun child can remain a zombie under WSL without delivering
    // its exit event. Do not let that stale process handle keep the Playwright
    // worker alive; the outer E2E process group owns the final reap.
    if (child.exitCode === null) child.unref();
  }
}

export async function expectNoHorizontalOverflow(page: Page, width: number): Promise<void> {
  await expect.poll(() => page.evaluate(() => ({
    width: window.innerWidth,
    fits: document.documentElement.scrollWidth <= document.documentElement.clientWidth
  }))).toEqual({ width, fits: true });
}

export async function holdInstallLifecycleLock(
  page: Page,
  mode: 'shared' | 'exclusive' = 'exclusive'
): Promise<() => Promise<void>> {
  await page.evaluate(({ lockName, mode }) => {
    const state = window as typeof window & {
      __ichiranInstallLockAcquired?: boolean;
      __ichiranReleaseInstallLock?: () => void;
    };
    state.__ichiranInstallLockAcquired = false;
    void navigator.locks.request(lockName, { mode }, async () => {
      state.__ichiranInstallLockAcquired = true;
      try {
        await new Promise<void>(resolve => { state.__ichiranReleaseInstallLock = resolve; });
      } finally {
        state.__ichiranInstallLockAcquired = false;
        state.__ichiranReleaseInstallLock = undefined;
      }
    });
  }, { lockName: 'ichiran-browser-alpha-install', mode });
  await expect.poll(() => page.evaluate(() => Boolean(
    (window as typeof window & { __ichiranInstallLockAcquired?: boolean })
      .__ichiranInstallLockAcquired
  ))).toBe(true);
  return async () => {
    await page.evaluate(() => {
      (window as typeof window & { __ichiranReleaseInstallLock?: () => void })
        .__ichiranReleaseInstallLock?.();
    });
    await expect.poll(() => page.evaluate(() => Boolean(
      (window as typeof window & { __ichiranInstallLockAcquired?: boolean })
        .__ichiranInstallLockAcquired
    ))).toBe(false);
  };
}

export async function pendingInstallLifecycleLocks(page: Page): Promise<readonly string[]> {
  return page.evaluate(async lockName => {
    const snapshot = await navigator.locks.query();
    return (snapshot.pending ?? []).flatMap(lock =>
      lock.name === lockName && lock.mode ? [lock.mode] : []
    );
  }, 'ichiran-browser-alpha-install');
}

export async function prepareStandaloneInstall(page: Page): Promise<void> {
  await page.evaluate(async () => {
    let workerUrl = performance.getEntriesByType('resource')
      .find(entry => new URL(entry.name).pathname.includes('/assets/analyzer.worker-'))
      ?.name;
    if (!workerUrl) {
      for (const cacheName of await caches.keys()) {
        const requests = await (await caches.open(cacheName)).keys();
        workerUrl = requests
          .find(request => new URL(request.url).pathname.includes('/assets/analyzer.worker-'))
          ?.url;
        if (workerUrl) break;
      }
    }
    if (!workerUrl) throw new Error('Could not locate the production analyzer Worker');
    const response = await fetch('/analyzer/manifest.json', { cache: 'no-store' });
    if (!response.ok) throw new Error(`Could not load analyzer manifest: HTTP ${response.status}`);
    const release = await response.json();

    const worker = new Worker(workerUrl, { type: 'module', name: 'ichiran-aba-installer' });
    const state = window as typeof window & {
      __ichiranStandaloneInstall?: {
        readonly worker: Worker;
        ready: boolean;
        started: boolean;
        done: boolean;
        error: string | null;
      };
    };
    state.__ichiranStandaloneInstall = {
      worker,
      ready: false,
      started: false,
      done: false,
      error: null
    };
    await new Promise<void>((resolve, reject) => {
      worker.addEventListener('message', (event: MessageEvent<{
        readonly id: number;
        readonly type: 'progress' | 'result' | 'error';
        readonly message?: string;
      }>) => {
        if (event.data.type === 'progress') return;
        const current = state.__ichiranStandaloneInstall;
        if (!current) return;
        if (event.data.id === 1) {
          if (event.data.type === 'result') {
            current.ready = true;
            resolve();
          } else {
            current.done = true;
            current.error = event.data.message ?? 'Standalone Worker initialization failed';
            reject(new Error(current.error));
          }
          return;
        }
        if (event.data.id !== 2) return;
        current.done = true;
        current.error = event.data.type === 'error'
          ? event.data.message ?? 'Standalone install failed'
          : null;
      });
      worker.postMessage({ id: 1, op: 'expect-release', release });
    });
  });
}

export async function startStandaloneInstall(page: Page): Promise<void> {
  await page.evaluate(() => {
    const current = (window as typeof window & {
      __ichiranStandaloneInstall?: {
        readonly worker: Worker;
        readonly ready: boolean;
        started: boolean;
      };
    }).__ichiranStandaloneInstall;
    if (!current?.ready || current.started) {
      throw new Error('Standalone installer is not ready to start');
    }
    current.started = true;
    current.worker.postMessage({ id: 2, op: 'install', manifestUrl: '/analyzer/manifest.json' });
  });
}

export async function queueStandaloneInstall(page: Page): Promise<void> {
  await prepareStandaloneInstall(page);
  await startStandaloneInstall(page);
}

export async function waitForStandaloneInstall(page: Page): Promise<string | null> {
  await expect.poll(() => page.evaluate(() => {
    const current = (window as typeof window & {
      __ichiranStandaloneInstall?: { readonly done: boolean; readonly error: string | null };
    }).__ichiranStandaloneInstall;
    return current ? { done: current.done, error: current.error } : null;
  }), { timeout: 180_000 }).toMatchObject({ done: true });
  const error = await page.evaluate(() => (
    window as typeof window & {
      __ichiranStandaloneInstall?: { readonly error: string | null };
    }
  ).__ichiranStandaloneInstall?.error ?? null);
  await page.evaluate(() => {
    const state = window as typeof window & {
      __ichiranStandaloneInstall?: { readonly worker: Worker };
    };
    state.__ichiranStandaloneInstall?.worker.terminate();
    state.__ichiranStandaloneInstall = undefined;
  });
  return error;
}

export function analyzerReady(page: Page) {
  return page.getByRole('textbox', { name: 'Japanese text', exact: true });
}

export async function removeAnalyzerData(page: Page): Promise<void> {
  await page.getByRole('button', { name: 'Analyzer settings' }).click();
  await page.getByRole('menuitem', { name: 'Remove data' }).click();
}

export function sha256(value: string | Uint8Array): string {
  return createHash('sha256').update(value).digest('hex');
}

export function identityAsset(file: string, body: Uint8Array): RoutedAsset {
  const digest = sha256(body);
  return {
    manifest: {
      file,
      encoding: 'identity',
      downloadBytes: body.byteLength,
      downloadSha256: digest,
      installedBytes: body.byteLength,
      installedSha256: digest
    },
    body
  };
}

export function gzipAsset(file: string, installed: Uint8Array): RoutedAsset {
  const body = gzipSync(installed);
  return {
    manifest: {
      file,
      encoding: 'gzip',
      downloadBytes: body.byteLength,
      downloadSha256: sha256(body),
      installedBytes: installed.byteLength,
      installedSha256: sha256(installed)
    },
    body
  };
}

export function signedManifest(
  hot: PackAssetManifest,
  details: PackAssetManifest
): AnalyzerPackManifest {
  const unsigned = {
    formatVersion: 1 as const,
    packVersion: 'e2e.integrity.1',
    sourceCommit: 'e'.repeat(40),
    sourcesLockSha256: sha256('e2e-sources-lock'),
    hot: {
      file: hot.file,
      encoding: hot.encoding,
      downloadBytes: hot.downloadBytes,
      downloadSha256: hot.downloadSha256,
      installedBytes: hot.installedBytes,
      installedSha256: hot.installedSha256
    },
    details: {
      file: details.file,
      encoding: details.encoding,
      downloadBytes: details.downloadBytes,
      downloadSha256: details.downloadSha256,
      installedBytes: details.installedBytes,
      installedSha256: details.installedSha256
    }
  };
  return {
    ...unsigned,
    manifestSha256: sha256(JSON.stringify(unsigned))
  };
}

export function withReleaseIdentity(
  manifest: AnalyzerPackManifest,
  packVersion: string,
  sourceCommit: string
): AnalyzerPackManifest {
  const unsigned = {
    formatVersion: 1 as const,
    packVersion,
    sourceCommit,
    sourcesLockSha256: manifest.sourcesLockSha256,
    hot: { ...manifest.hot },
    details: { ...manifest.details }
  };
  return { ...unsigned, manifestSha256: sha256(JSON.stringify(unsigned)) };
}

export async function denyPersistentStorage(context: BrowserContext): Promise<void> {
  await context.addInitScript(() => {
    if (!('storage' in navigator) || !navigator.storage) return;
    Object.defineProperty(navigator.storage, 'persist', {
      configurable: true,
      value: () => Promise.reject(new DOMException('Denied by acceptance harness', 'NotAllowedError'))
    });
    Object.defineProperty(navigator.storage, 'persisted', {
      configurable: true,
      value: () => Promise.resolve(false)
    });
  });
}

export async function fulfillAsset(route: Route, asset: RoutedAsset): Promise<void> {
  await route.fulfill({
    status: 200,
    headers: {
      'content-type': asset.manifest.encoding === 'gzip'
        ? 'application/gzip'
        : 'application/octet-stream'
    },
    body: Buffer.from(asset.body)
  });
}

export async function routePack(
  context: BrowserContext,
  manifest: AnalyzerPackManifest,
  hot: RoutedAsset,
  details: RoutedAsset,
  bodies: Readonly<Partial<Record<'hot' | 'details', Uint8Array>>> = {}
): Promise<void> {
  await context.route('**/analyzer/manifest.json', route => route.fulfill({
    status: 200,
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(manifest)
  }));
  await context.route(`**/analyzer/${hot.manifest.file}`, route => fulfillAsset(route, {
    manifest: hot.manifest,
    body: bodies.hot ?? hot.body
  }));
  await context.route(`**/analyzer/${details.manifest.file}`, route => fulfillAsset(route, {
    manifest: details.manifest,
    body: bodies.details ?? details.body
  }));
}

export async function mockWorkerStorageEstimateFromAppFiles(
  context: BrowserContext,
  quota: number
): Promise<void> {
  await context.route('**/assets/analyzer.worker-*.js', async route => {
    const response = await route.fetch();
    const source = await response.text();
    const prefix = `
Object.defineProperty(navigator.storage, 'estimate', {
  configurable: true,
  value: async () => {
    const root = await navigator.storage.getDirectory();
    const directory = await root.getDirectoryHandle('${DIRECTORY_NAME}', { create: true });
    const names = [
      'install-a.json', 'install-b.json', 'hot-a.bin', 'details-a.bin',
      'hot-b.bin', 'details-b.bin', 'install.json', 'hot.bin', 'details.bin',
      'asset.download'
    ];
    let usage = 0;
    for (const name of names) {
      try {
        usage += (await (await directory.getFileHandle(name)).getFile()).size;
      } catch (error) {
        if (!(error instanceof DOMException) || error.name !== 'NotFoundError') throw error;
      }
    }
    return { quota: ${quota}, usage };
  }
});
`;
    const headers = { ...response.headers() };
    delete headers['content-length'];
    delete headers['content-encoding'];
    await route.fulfill({ response, headers, body: `${prefix}\n${source}` });
  });
}

export async function openIsolatedContext(
  browser: Browser
): Promise<BrowserContext> {
  const context = await browser.newContext({
    baseURL: BASE_URL,
    serviceWorkers: 'block',
    viewport: { width: 390, height: 844 }
  });
  watchConsoleHealth(context);
  await denyPersistentStorage(context);
  return context;
}

export async function opfsSnapshot(page: Page): Promise<OpfsSnapshot> {
  return page.evaluate(async directoryName => {
    const root = await navigator.storage.getDirectory();
    const directory = await root.getDirectoryHandle(directoryName, { create: true });
    async function fileBytes(name: string): Promise<number | null> {
      try {
        return (await (await directory.getFileHandle(name)).getFile()).size;
      } catch (error) {
        if (error instanceof DOMException && error.name === 'NotFoundError') return null;
        throw error;
      }
    }
    async function totalBytes(names: readonly string[]): Promise<number | null> {
      const sizes = await Promise.all(names.map(fileBytes));
      const present = sizes.filter((size): size is number => size !== null);
      return present.length === 0 ? null : present.reduce((total, size) => total + size, 0);
    }
    const [markerBytes, hotBytes, detailsBytes, downloadBytes] = await Promise.all([
      totalBytes(['install-a.json', 'install-b.json']),
      totalBytes(['hot-a.bin', 'hot-b.bin']),
      totalBytes(['details-a.bin', 'details-b.bin']),
      fileBytes('asset.download')
    ]);
    return { markerBytes, hotBytes, detailsBytes, downloadBytes };
  }, DIRECTORY_NAME);
}

export async function committedInstallId(page: Page): Promise<string | null> {
  return page.evaluate(async ({ databaseName, storeName, key }) => {
    const database = await new Promise<IDBDatabase>((resolve, reject) => {
      const request = indexedDB.open(databaseName, 1);
      request.onupgradeneeded = () => request.result.createObjectStore(storeName);
      request.onerror = () => reject(request.error);
      request.onsuccess = () => resolve(request.result);
    });
    try {
      return await new Promise<string | null>((resolve, reject) => {
        const request = database.transaction(storeName).objectStore(storeName).get(key);
        request.onerror = () => reject(request.error);
        request.onsuccess = () => resolve(
          typeof request.result === 'string' ? request.result : null
        );
      });
    } finally {
      database.close();
    }
  }, {
    databaseName: 'ichiran-browser-alpha-control',
    storeName: 'state',
    key: 'install-id'
  });
}

export async function writeCommittedInstallId(page: Page, installId: string | null): Promise<void> {
  await page.evaluate(async ({ databaseName, storeName, key, installId }) => {
    const database = await new Promise<IDBDatabase>((resolve, reject) => {
      const request = indexedDB.open(databaseName, 1);
      request.onupgradeneeded = () => request.result.createObjectStore(storeName);
      request.onerror = () => reject(request.error);
      request.onsuccess = () => resolve(request.result);
    });
    try {
      await new Promise<void>((resolve, reject) => {
        const transaction = database.transaction(storeName, 'readwrite');
        const store = transaction.objectStore(storeName);
        if (installId === null) store.delete(key);
        else store.put(installId, key);
        transaction.oncomplete = () => resolve();
        transaction.onerror = () => reject(transaction.error);
        transaction.onabort = () => reject(transaction.error);
      });
    } finally {
      database.close();
    }
  }, {
    databaseName: 'ichiran-browser-alpha-control',
    storeName: 'state',
    key: 'install-id',
    installId
  });
}

export async function staleInstallFiles(page: Page): Promise<readonly string[]> {
  return page.evaluate(async directoryName => {
    const root = await navigator.storage.getDirectory();
    const directory = await root.getDirectoryHandle(directoryName, { create: true });
    const found: string[] = [];
    for (const name of ['install.json', 'hot.bin', 'details.bin']) {
      try {
        await directory.getFileHandle(name);
        found.push(name);
      } catch (error) {
        if (!(error instanceof DOMException) || error.name !== 'NotFoundError') throw error;
      }
    }
    return found;
  }, DIRECTORY_NAME);
}

export async function activeOpfsFiles(page: Page): Promise<{
  readonly marker: string;
  readonly hot: string;
  readonly details: string;
}> {
  const installId = await committedInstallId(page);
  if (!installId) throw new Error('Analyzer has no committed install ID');
  return page.evaluate(async ({ directoryName, installId }) => {
    const root = await navigator.storage.getDirectory();
    const directory = await root.getDirectoryHandle(directoryName);
    for (const markerName of ['install-a.json', 'install-b.json']) {
      try {
        const marker = JSON.parse(
          await (await directory.getFileHandle(markerName)).getFile().then(file => file.text())
        ) as { readonly installId?: unknown; readonly slot?: unknown };
        if (marker.installId !== installId) continue;
        if (marker.slot !== 'a' && marker.slot !== 'b') {
          throw new Error(`Committed OPFS marker ${markerName} has no current data slot`);
        }
        return {
          marker: markerName,
          hot: `hot-${marker.slot}.bin`,
          details: `details-${marker.slot}.bin`
        };
      } catch (error) {
        if (error instanceof DOMException && error.name === 'NotFoundError') continue;
        throw error;
      }
    }
    throw new Error(`No OPFS marker matches committed install ${installId}`);
  }, { directoryName: DIRECTORY_NAME, installId });
}

export function expectNoInstalledFiles(snapshot: OpfsSnapshot): void {
  expect(snapshot).toEqual({
    markerBytes: null,
    hotBytes: null,
    detailsBytes: null,
    downloadBytes: null
  });
}

export async function rejectedInstall(
  browser: Browser,
  manifest: AnalyzerPackManifest,
  hot: RoutedAsset,
  details: RoutedAsset,
  expectedMessage: string,
  bodies?: Readonly<Partial<Record<'hot' | 'details', Uint8Array>>>,
  manifestRejected = false
): Promise<void> {
  const context = await openIsolatedContext(browser);
  try {
    await routePack(context, manifest, hot, details, bodies);
    const page = await context.newPage();
    await page.goto('/');
    if (!manifestRejected) {
      await page.getByRole('button', { name: 'Install analyzer data' }).click();
    }
    await expect(page.getByRole('alert').filter({ hasText: expectedMessage })).toBeVisible();
    if (!manifestRejected) await expect(page.getByRole('button', { name: 'Retry' })).toBeEnabled();
    expectNoInstalledFiles(await opfsSnapshot(page));
  } finally {
    await context.close();
  }
}

export async function interruptInstall(
  browser: Browser,
  artifact: 'hot' | 'details'
): Promise<void> {
  const context = await openIsolatedContext(browser);
  try {
    const installingPage = await context.newPage();
    await installingPage.goto('/');
    const manifest = await installingPage.request
      .get('/analyzer/manifest.json')
      .then(response => response.json() as Promise<AnalyzerPackManifest>);
    const target = manifest[artifact];
    const completedBytes = artifact === 'details' ? manifest.hot.downloadBytes : 0;
    const targetPattern = `**/analyzer/${target.file}`;
    const streamPartial = async (route: Route): Promise<void> => {
      const url = new URL(route.request().url());
      url.searchParams.set('__ichiran_e2e_partial', '1');
      await route.continue({ url: url.toString() });
    };
    await context.route(targetPattern, streamPartial);
    await installingPage.getByRole('button', { name: 'Install analyzer data' }).click();
    await expect(installingPage.getByText('Downloading', { exact: true })).toBeVisible();
    await installingPage.waitForFunction(
      ([lower, upper]) => {
        const progress = document.querySelector('progress');
        return progress instanceof HTMLProgressElement
          && progress.value > lower
          && progress.value < upper;
      },
      [completedBytes, completedBytes + target.downloadBytes]
    );
    const duringInstall = await opfsSnapshot(installingPage);
    expect(duringInstall.markerBytes).toBeNull();
    expect(await committedInstallId(installingPage)).toBeNull();
    if (artifact === 'details') {
      expect(duringInstall.hotBytes).toBe(manifest.hot.installedBytes);
    }

    await installingPage.close();
    await context.unroute(targetPattern, streamPartial);

    // Reopen in the same storage context: a killed Worker must never have committed ready.
    const reopened = await context.newPage();
    await reopened.goto('/');
    await expect(analyzerReady(reopened)).toHaveCount(0);
    expect((await opfsSnapshot(reopened)).markerBytes).toBeNull();
    if (artifact === 'details') {
      await expect(reopened.getByText('The saved data is incomplete. Install it again.')).toBeVisible();
      reopened.once('dialog', dialog => dialog.accept());
      await reopened.getByRole('button', { name: 'Remove saved data' }).click();
    }
    await expect(reopened.getByRole('button', { name: 'Install analyzer data' })).toBeEnabled();
    const afterRecovery = await opfsSnapshot(reopened);
    if (artifact === 'hot') {
      // Chromium may retain the transactional target as a zero-byte inode
      // after killing its writable stream. It is never an active or partial
      // pack, and the next install removes it before writing.
      expect({ ...afterRecovery, downloadBytes: null }).toEqual({
        markerBytes: null,
        hotBytes: null,
        detailsBytes: null,
        downloadBytes: null
      });
      expect([null, 0]).toContain(afterRecovery.downloadBytes);
    } else {
      expectNoInstalledFiles(afterRecovery);
    }
  } finally {
    await context.close();
  }
}
