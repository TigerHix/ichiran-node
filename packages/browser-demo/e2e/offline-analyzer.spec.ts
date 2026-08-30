import { createHash } from 'node:crypto';
import { spawn, type ChildProcess } from 'node:child_process';
import { mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { cpus, platform, release, tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import {
  expect,
  test,
  type Browser,
  type BrowserContext,
  type Page,
  type Route
} from 'playwright/test';
import type {
  AnalysisResult,
  AnalyzerPackManifest,
  BenchmarkResult,
  PackAssetManifest
} from '../src/protocol.js';

const BASE_URL = 'http://127.0.0.1:4173';
const DIRECTORY_NAME = 'ichiran-browser-alpha';
const INSTALL_ID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/;

interface RoutedAsset {
  readonly manifest: PackAssetManifest;
  readonly body: Uint8Array;
}

interface OpfsSnapshot {
  readonly markerBytes: number | null;
  readonly hotBytes: number | null;
  readonly detailsBytes: number | null;
  readonly downloadBytes: number | null;
}

interface WorkerCalibrationSample {
  readonly ms: number;
  readonly state: number;
}

interface LegacyDetailedWord {
  readonly text: string;
  readonly reading: string;
  readonly score: number;
  readonly seq?: number;
  readonly gloss?: readonly { readonly pos: string; readonly gloss: string }[];
  readonly compound?: readonly string[];
  readonly components?: readonly LegacyDetailedWord[];
}

type LegacyDetailedToken = readonly [string, LegacyDetailedWord, unknown];
type LegacyDetailedAlternative = readonly [readonly LegacyDetailedToken[], number];

function median(values: readonly number[]): number {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.floor(sorted.length / 2)]!;
}

async function singleCpuAffinity(): Promise<number> {
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

async function attachAnalyzerWorker(browser: Browser): Promise<{
  readonly target: { readonly type: string; readonly title: string; readonly url: string };
  readonly samples: (count: number) => Promise<readonly WorkerCalibrationSample[]>;
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
    async close() {
      cdp.off('Target.receivedMessageFromTarget', receive);
      await cdp.send('Target.detachFromTarget', { sessionId });
      await cdp.detach();
    }
  };
}

async function startCpuHogs(cpu: number, count: number): Promise<readonly ChildProcess[]> {
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

async function stopCpuHogs(children: readonly ChildProcess[]): Promise<void> {
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
  await Promise.all(exits);
}

async function expectInstallablePwa(page: Page): Promise<void> {
  const cdp = await page.context().newCDPSession(page);
  try {
    const report = await cdp.send('Page.getAppManifest');
    expect(report.errors).toEqual([]);
    expect(report.url).toMatch(/\/manifest\.webmanifest$/);
    const manifest = JSON.parse(report.data ?? '{}') as {
      readonly name?: string;
      readonly start_url?: string;
      readonly scope?: string;
      readonly display?: string;
      readonly icons?: readonly { readonly sizes?: string }[];
    };
    expect(manifest).toMatchObject({
      name: 'Ichiran Browser Analyzer',
      start_url: '/',
      scope: '/',
      display: 'standalone'
    });
    expect(manifest.icons?.map(icon => icon.sizes)).toEqual(['192x192', '512x512', 'any']);
  } finally {
    await cdp.detach();
  }
}

async function expectNoHorizontalOverflow(page: Page, width: number): Promise<void> {
  await expect.poll(() => page.evaluate(() => ({
    width: window.innerWidth,
    fits: document.documentElement.scrollWidth <= document.documentElement.clientWidth
  }))).toEqual({ width, fits: true });
}

async function holdInstallLifecycleLock(
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

async function pendingInstallLifecycleLocks(page: Page): Promise<readonly string[]> {
  return page.evaluate(async lockName => {
    const snapshot = await navigator.locks.query();
    return (snapshot.pending ?? []).flatMap(lock =>
      lock.name === lockName && lock.mode ? [lock.mode] : []
    );
  }, 'ichiran-browser-alpha-install');
}

async function queueStandaloneInstall(page: Page): Promise<void> {
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

    const worker = new Worker(workerUrl, { type: 'module', name: 'ichiran-aba-installer' });
    const state = window as typeof window & {
      __ichiranStandaloneInstall?: {
        readonly worker: Worker;
        done: boolean;
        error: string | null;
      };
    };
    state.__ichiranStandaloneInstall = { worker, done: false, error: null };
    worker.addEventListener('message', (event: MessageEvent<{
      readonly id: number;
      readonly type: 'progress' | 'result' | 'error';
      readonly message?: string;
    }>) => {
      if (event.data.id !== 1 || event.data.type === 'progress') return;
      const current = state.__ichiranStandaloneInstall;
      if (!current) return;
      current.done = true;
      current.error = event.data.type === 'error'
        ? event.data.message ?? 'Standalone install failed'
        : null;
    });
    worker.postMessage({ id: 1, op: 'install', manifestUrl: '/analyzer/manifest.json' });
  });
}

async function waitForStandaloneInstall(page: Page): Promise<void> {
  await expect.poll(() => page.evaluate(() => {
    const current = (window as typeof window & {
      __ichiranStandaloneInstall?: { readonly done: boolean; readonly error: string | null };
    }).__ichiranStandaloneInstall;
    return current ? { done: current.done, error: current.error } : null;
  }), { timeout: 180_000 }).toEqual({ done: true, error: null });
  await page.evaluate(() => {
    const state = window as typeof window & {
      __ichiranStandaloneInstall?: { readonly worker: Worker };
    };
    state.__ichiranStandaloneInstall?.worker.terminate();
    state.__ichiranStandaloneInstall = undefined;
  });
}

function runtimeValue(page: Page, label: string) {
  const exact = new RegExp(`^${label.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`);
  return page.locator('.runtime-panel dl div')
    .filter({ has: page.locator('dt').filter({ hasText: exact }) })
    .locator('dd');
}

function sha256(value: string | Uint8Array): string {
  return createHash('sha256').update(value).digest('hex');
}

function identityAsset(file: string, body: Uint8Array): RoutedAsset {
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

function gzipAsset(file: string, installed: Uint8Array): RoutedAsset {
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

function signedManifest(
  hot: PackAssetManifest,
  details: PackAssetManifest
): AnalyzerPackManifest {
  const unsigned = {
    formatVersion: 1 as const,
    packVersion: 'e2e.integrity.1',
    sourceCommit: 'e2e-source-commit',
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

async function denyPersistentStorage(context: BrowserContext): Promise<void> {
  await context.addInitScript(() => {
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

async function fulfillAsset(route: Route, asset: RoutedAsset): Promise<void> {
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

async function routePack(
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

async function openIsolatedContext(
  browser: Browser,
  offlineShell: 'ready' | 'error' = 'ready'
): Promise<BrowserContext> {
  const context = await browser.newContext({
    baseURL: BASE_URL,
    serviceWorkers: 'block',
    viewport: { width: 390, height: 844 }
  });
  await context.addInitScript(mode => {
    Object.defineProperty(navigator.serviceWorker, 'register', {
      configurable: true,
      value: () => mode === 'error'
        ? Promise.reject(new Error('Registration denied by the acceptance harness.'))
        : Promise.resolve({})
    });
    Object.defineProperty(navigator.serviceWorker, 'ready', {
      configurable: true,
      get: () => Promise.resolve({})
    });
  }, offlineShell);
  await denyPersistentStorage(context);
  return context;
}

async function opfsSnapshot(page: Page): Promise<OpfsSnapshot> {
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
    const [markerBytes, hotBytes, detailsBytes, downloadBytes] = await Promise.all([
      fileBytes('install.json'),
      fileBytes('hot.bin'),
      fileBytes('details.bin'),
      fileBytes('asset.download')
    ]);
    return { markerBytes, hotBytes, detailsBytes, downloadBytes };
  }, DIRECTORY_NAME);
}

async function committedInstallId(page: Page): Promise<string | null> {
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

function expectNoInstalledFiles(snapshot: OpfsSnapshot): void {
  expect(snapshot).toEqual({
    markerBytes: null,
    hotBytes: null,
    detailsBytes: null,
    downloadBytes: null
  });
}

async function rejectedInstall(
  browser: Browser,
  manifest: AnalyzerPackManifest,
  hot: RoutedAsset,
  details: RoutedAsset,
  expectedMessage: string,
  bodies?: Readonly<Partial<Record<'hot' | 'details', Uint8Array>>>
): Promise<void> {
  const context = await openIsolatedContext(browser);
  try {
    await routePack(context, manifest, hot, details, bodies);
    const page = await context.newPage();
    await page.goto('/');
    await page.getByRole('button', { name: 'Install analyzer data' }).click();
    await expect(page.locator('.inline-error')).toContainText(expectedMessage);
    await expect(page.getByRole('button', { name: 'Retry' })).toBeEnabled();
    expectNoInstalledFiles(await opfsSnapshot(page));
  } finally {
    await context.close();
  }
}

async function interruptInstall(
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
    await expect(installingPage.getByText('Downloading analyzer data', { exact: false })).toBeVisible();
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
    await expect(reopened.getByText('Ready offline')).toHaveCount(0);
    expect((await opfsSnapshot(reopened)).markerBytes).toBeNull();
    if (artifact === 'details') {
      await expect(reopened.getByText('Analyzer data is incomplete or corrupted.')).toBeVisible();
      reopened.once('dialog', dialog => dialog.accept());
      await reopened.getByRole('button', { name: 'Clear installed data' }).click();
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

test('does not claim offline readiness when the app shell cannot activate', async ({ browser }) => {
  const context = await openIsolatedContext(browser, 'error');
  try {
    const page = await context.newPage();
    await page.goto('/');
    await expect(page.getByText('Offline app shell could not be prepared.', { exact: false }))
      .toBeVisible();
    await expect(page.getByRole('button', { name: 'Install analyzer data' })).toBeDisabled();
    await expect(page.getByText('Ready offline')).toHaveCount(0);
  } finally {
    await context.close();
  }
});

test('shows the unsupported screen instead of constructing a missing Worker', async ({ browser }) => {
  const context = await browser.newContext({ serviceWorkers: 'block' });
  try {
    await context.addInitScript(() => {
      Object.defineProperty(window, 'Worker', { configurable: true, value: undefined });
    });
    const page = await context.newPage();
    await page.goto('/');
    await expect(page.getByRole('heading', {
      name: 'This browser does not support the storage features required by this alpha.'
    })).toBeVisible();
  } finally {
    await context.close();
  }
});

test('rejects bad manifest, transfer, and installed digests without committing ready', async ({
  browser
}) => {
  const hotBytes = Uint8Array.from({ length: 64 }, (_, index) => index);
  const detailsBytes = Uint8Array.of(9, 8, 7, 6);
  const hot = identityAsset('integrity-hot.bin', hotBytes);
  const details = identityAsset('integrity-details.bin', detailsBytes);
  const valid = signedManifest(hot.manifest, details.manifest);

  await rejectedInstall(
    browser,
    { ...valid, manifestSha256: '0'.repeat(64) },
    hot,
    details,
    'Analyzer manifest checksum does not match'
  );
  await rejectedInstall(
    browser,
    valid,
    hot,
    details,
    'Downloaded 11 bytes; expected 64',
    { hot: hotBytes.slice(0, 11) }
  );
  const corruptTransfer = Uint8Array.from(hotBytes);
  corruptTransfer[17] ^= 0xff;
  await rejectedInstall(
    browser,
    valid,
    hot,
    details,
    'Downloaded asset checksum does not match',
    { hot: corruptTransfer }
  );

  const installed = Uint8Array.from({ length: 128 }, (_, index) => index ^ 0x5a);
  const compressed = gzipAsset('installed-hash-hot.bin.gz', installed);
  const wrongInstalledDigest = sha256(Uint8Array.from(installed, value => value ^ 0xff));
  const badInstalledAsset: RoutedAsset = {
    ...compressed,
    manifest: { ...compressed.manifest, installedSha256: wrongInstalledDigest }
  };
  await rejectedInstall(
    browser,
    signedManifest(badInstalledAsset.manifest, details.manifest),
    badInstalledAsset,
    details,
    'Installed asset checksum does not match'
  );
});

test('interrupted hot and details installs never commit a ready marker', async ({ browser }) => {
  await interruptInstall(browser, 'hot');
  await interruptInstall(browser, 'details');
});

test('installs once, restarts offline, meets the 6x proxy, and detects runtime corruption', async ({
  browser
}) => {
  const browserType = browser.browserType();
  const profileDirectory = await mkdtemp(join(tmpdir(), 'ichiran-browser-alpha-e2e-'));
  let context: BrowserContext | null = null;
  try {
    context = await browserType.launchPersistentContext(profileDirectory, {
      baseURL: BASE_URL,
      headless: true,
      permissions: ['clipboard-read', 'clipboard-write'],
      serviceWorkers: 'allow',
      viewport: { width: 390, height: 844 }
    });
    await denyPersistentStorage(context);
    let page = context.pages()[0] ?? await context.newPage();
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto('/');
  await expectNoHorizontalOverflow(page, 390);
  await page.setViewportSize({ width: 320, height: 844 });
  await expectNoHorizontalOverflow(page, 320);
  await page.setViewportSize({ width: 1280, height: 900 });
  await expectNoHorizontalOverflow(page, 1280);
  await page.setViewportSize({ width: 390, height: 844 });
  await expectInstallablePwa(page);
  const manifest = await page.request
    .get('/analyzer/manifest.json')
    .then(response => response.json() as Promise<AnalyzerPackManifest>);
  for (const asset of [manifest.hot, manifest.details] as const) {
    const response = await page.request.head(`/analyzer/${asset.file}`);
    expect(response.ok()).toBe(true);
    expect(response.headers()['content-type']).toBe('application/gzip');
    expect(response.headers()['content-encoding']).toBeUndefined();
  }
  await expect(page.getByRole('button', { name: 'Install analyzer data' })).toBeVisible();
  await page.getByRole('button', { name: 'Install analyzer data' }).click();
  await expect(page.getByText('Ready offline')).toBeVisible({ timeout: 180_000 });
  const committedInstall = await opfsSnapshot(page);
  expect(committedInstall.markerBytes).not.toBeNull();
  expect(await committedInstallId(page)).toMatch(INSTALL_ID_PATTERN);

  await page.evaluate(() => navigator.serviceWorker.ready.then(() => undefined));
  if (!await page.evaluate(() => Boolean(navigator.serviceWorker.controller))) {
    await page.reload();
  }
  await expect(page.getByText('Ready offline')).toBeVisible();

  await page.getByRole('button', { name: 'Analyze' }).click();
  const talking = page.getByRole('button', { name: /話しました/ }).first();
  await expect(talking).toBeVisible();
  await talking.click();
  await expect(page.getByRole('heading', { name: '話しました' })).toBeVisible();
  await expect(page.getByText('Dictionary forms')).toBeVisible();
  await expect(page.getByText('Conjugation path')).toBeVisible();

  await page.getByText('Advanced', { exact: true }).click();
  await page.getByLabel('Top results').selectOption('3');
  await page.getByLabel('Entity spans').fill('0:2:120');
  await page.getByRole('button', { name: 'Analyze' }).click();
  await expect(page.locator('.result-heading span')).toContainText('score');

  await page.getByText('Runtime & data', { exact: true }).click();
  await expect(runtimeValue(page, 'Worker')).toHaveText('Open');
  await expect(runtimeValue(page, 'One-time download')).not.toHaveText('0 B');
  await expect(runtimeValue(page, 'Persistent storage')).toHaveText('Best effort');

  // Close Chromium completely, then launch the same on-disk profile with networking disabled.
  await context.close();
  context = null;
  context = await browserType.launchPersistentContext(profileDirectory, {
    baseURL: BASE_URL,
    headless: true,
    offline: true,
    permissions: ['clipboard-read', 'clipboard-write'],
    serviceWorkers: 'allow',
    viewport: { width: 390, height: 844 }
  });
  await denyPersistentStorage(context);
  await context.setOffline(true);
  page = context.pages()[0] ?? await context.newPage();
  await page.goto('/');
  // Chromium flips navigator.onLine back to true after a cached Service Worker
  // navigation, so prove the transport is offline with a URL the worker ignores.
  expect(await page.evaluate(async () => {
    try {
      await fetch(`/__ichiran-offline-probe-${Date.now()}`, { cache: 'no-store' });
      return false;
    } catch {
      return true;
    }
  })).toBe(true);
  await expect(page.getByText('Ready offline')).toBeVisible();
  await expectNoHorizontalOverflow(page, 390);
  await expectInstallablePwa(page);
  const analyzerRequests: string[] = [];
  const recordRequest = (request: { url(): string }) => {
    const url = new URL(request.url());
    if (url.pathname.startsWith('/analyzer/')) analyzerRequests.push(request.url());
  };
  page.on('request', recordRequest);
  await page.evaluate(() => {
    const durations: number[] = [];
    const observer = new PerformanceObserver(list => {
      durations.push(...list.getEntries().map(entry => entry.duration));
    });
    observer.observe({ type: 'longtask' });
    const state = window as typeof window & {
      __ichiranLongTasks?: number[];
      __ichiranLongTaskObserver?: PerformanceObserver;
    };
    state.__ichiranLongTasks = durations;
    state.__ichiranLongTaskObserver = observer;
  });
  await page.getByRole('textbox', { name: 'Japanese text', exact: true })
    .fill('日本語を勉強しています。');
  await page.getByText('Advanced', { exact: true }).click();
  await page.getByLabel('Top results').selectOption('3');
  await page.getByLabel('Entity spans').fill('0:3:120');
  await page.getByLabel('Normalize punctuation').check();
  await page.getByRole('button', { name: 'Analyze' }).click();
  await expect(page.getByRole('button', { name: /日本語/ }).first()).toBeVisible();
  await expect(page.locator('details.alternatives summary span')).toHaveText('2');
  await page.getByRole('button', { name: /日本語/ }).first().click();
  await expect(page.getByText('Dictionary forms')).toBeVisible();
  await page.getByText('Runtime & data', { exact: true }).click();
  await expect(runtimeValue(page, 'Request')).toHaveText('12 units · top 3 · 1 boosts');

  await page.evaluate(() => navigator.clipboard.writeText(''));
  await page.getByRole('button', { name: 'Copy clean JSON' }).click();
  await expect.poll(() => page.evaluate(() => navigator.clipboard.readText()))
    .toContain('"input": "日本語を勉強しています。"');
  const clean = JSON.parse(
    await page.evaluate(() => navigator.clipboard.readText())
  ) as AnalysisResult;
  expect(clean).toMatchObject({
    input: '日本語を勉強しています。',
    normalized: '日本語を勉強しています. '
  });
  expect(clean.paths.map(path => path.score)).toEqual([3453, 3439, 2928]);
  expect(clean.paths[0]?.tokens.map(token => token.text)).toEqual([
    '日本語', 'を', '勉強しています', '. '
  ]);
  expect(clean.paths[0]?.tokens[0]).toMatchObject({
    start: 0,
    end: 3,
    text: '日本語',
    score: 1054,
    entity: true,
    root: { seq: 1464530, form: '日本語', reading: 'にほんご' }
  });

  await page.getByRole('button', { name: 'Copy legacy JSON' }).click();
  await expect(page.locator('.runtime-message')).toHaveText('Legacy JSON copied.');
  const legacy = JSON.parse(
    await page.evaluate(() => navigator.clipboard.readText())
  ) as unknown as readonly [readonly LegacyDetailedAlternative[], string];
  expect(legacy[1]).toBe('. ');
  expect(legacy[0].map(alternative => alternative[1])).toEqual([3453, 3439, 2928]);
  const topLegacy = legacy[0][0]?.[0];
  expect(topLegacy?.map(token => token[1].text)).toEqual(['日本語', 'を', '勉強しています']);
  expect(topLegacy?.[0]?.[0]).toBe('nihongo');
  expect(topLegacy?.[0]?.[1]).toMatchObject({
    reading: '日本語 【にほんご】',
    text: '日本語',
    score: 1054,
    seq: 1464530,
    gloss: [
      { pos: '[n-pr]', gloss: 'proper noun (named entity)' },
      { pos: '[n]', gloss: 'Japanese (language)' }
    ]
  });
  expect(topLegacy?.[2]?.[1]).toMatchObject({
    reading: '勉強しています 【べんきょう しています】',
    score: 2254,
    compound: ['勉強', 'して', 'います']
  });
  expect(topLegacy?.[2]?.[1].components?.map(component => component.text)).toEqual([
    '勉強', 'して', 'います'
  ]);

  await page.getByRole('button', { name: 'Romanize input' }).click();
  await expect(runtimeValue(page, 'Romanization')).toHaveText('nihongo wo benkyō shiteimasu。');
  await expectNoHorizontalOverflow(page, 390);
  const longTasks = await page.evaluate(
    () => {
      const state = window as typeof window & {
        __ichiranLongTasks?: number[];
        __ichiranLongTaskObserver?: PerformanceObserver;
      };
      state.__ichiranLongTaskObserver?.disconnect();
      return state.__ichiranLongTasks ?? [];
    }
  );
  expect(longTasks.filter(duration => duration > 50)).toEqual([]);

  const affinityCpu = await singleCpuAffinity();
  const analyzerBrowser = context.browser();
  if (!analyzerBrowser) throw new Error('Persistent analyzer context has no Chromium browser');
  const workerRuntime = await attachAnalyzerWorker(analyzerBrowser);
  let hogs: readonly ChildProcess[] = [];
  try {
    const warmup = await workerRuntime.samples(2);
    const baseline = await workerRuntime.samples(7);
    hogs = await startCpuHogs(affinityCpu, 5);
    await new Promise(resolve => setTimeout(resolve, 1_000));
    const contended = await workerRuntime.samples(7);
    const checksums = [...warmup, ...baseline, ...contended].map(sample => sample.state);
    expect(new Set(checksums).size).toBe(1);
    const baselineMedianMs = median(baseline.map(sample => sample.ms));
    const contendedMedianMs = median(contended.map(sample => sample.ms));
    const contentionRatio = contendedMedianMs / baselineMedianMs;
    expect(contentionRatio).toBeGreaterThanOrEqual(5);
    expect(contentionRatio).toBeLessThanOrEqual(7.5);

    await page.getByRole('button', { name: 'Run benchmark' }).click();
    // This watchdog includes the entire corpus under induced host contention.
    // The assertions below enforce the actual analyzer latency requirements.
    await expect(page.getByText('Benchmark complete.')).toBeVisible({ timeout: 20 * 60 * 1000 });
    const ordinaryP95 = Number.parseFloat(await runtimeValue(page, 'ordinary p95').innerText());
    const pathologicalP95 = Number.parseFloat(
      await runtimeValue(page, 'pathological-morphology p95').innerText()
    );
    const downloadPromise = page.waitForEvent('download');
    await page.getByRole('button', { name: 'Download benchmark JSON' }).click();
    const download = await downloadPromise;
    const downloadPath = await download.path();
    if (!downloadPath) throw new Error('Benchmark download did not produce a local file');
    const benchmark = JSON.parse(await readFile(downloadPath, 'utf8')) as BenchmarkResult;
    expect(benchmark.release).toEqual(manifest);
    expect(benchmark.corpusVersion).toBe(2);
    expect(benchmark.groups.map(group => group.corpus)).toEqual([
      'ordinary', 'pathological-morphology'
    ]);
    expect(benchmark.diagnostics.analyzeGroups.map(group => [group.corpus, group.samples])).toEqual([
      ['segmentation-short', 4590],
      ['long-noun-compound', 500],
      ['hiragana-colloquial', 500],
      ['modern-mixed-script', 500],
      ['top-n', 20],
      ['entities', 540],
      ['counters', 2000],
      ['numbers', 70]
    ]);
    expect(benchmark.diagnostics.describe.samples).toBe(500);
    expect(benchmark.diagnostics.workerReadyMs).toBeGreaterThanOrEqual(0);
    expect(benchmark.diagnostics.firstAnalyzeMs).toBeGreaterThanOrEqual(0);
    const environment = await page.evaluate(() => ({
      userAgent: navigator.userAgent,
      hardwareConcurrency: navigator.hardwareConcurrency
    }));
    const benchmarkPath = resolve(import.meta.dirname, '../../../work/browser-benchmark.json');
    await mkdir(resolve(benchmarkPath, '..'), { recursive: true });
    await writeFile(benchmarkPath, `${JSON.stringify({
      ...benchmark,
      environment: {
        ...environment,
        playwrightBrowserVersion: context.browser()?.version() ?? browser.version(),
        hostPlatform: platform(),
        hostRelease: release(),
        cpuModel: cpus()[0]?.model ?? 'unknown'
      },
      performanceProxy: {
        method: 'linux-taskset-single-cpu-contention',
        affinityCpu,
        hogCount: 5,
        calibrationTarget: workerRuntime.target,
        iterationsPerSample: 60_000_000,
        stateChecksum: checksums[0]!,
        warmupRawMs: warmup.map(sample => sample.ms),
        baselineRawMs: baseline.map(sample => sample.ms),
        contendedRawMs: contended.map(sample => sample.ms),
        baselineMedianMs,
        contendedMedianMs,
        ratio: contentionRatio,
        acceptedRatio: { minimum: 5, maximum: 7.5 }
      },
      mainThreadLongTasksMeasurement: 'uncontended',
      mainThreadLongTasksMs: longTasks
    }, null, 2)}\n`);
    // Preserve the complete raw report even when a performance threshold fails.
    expect(ordinaryP95).toBeLessThanOrEqual(75);
    expect(pathologicalP95).toBeLessThanOrEqual(250);
    expect(analyzerRequests).toEqual([]);
  } finally {
    page.off('request', recordRequest);
    await stopCpuHogs(hogs);
    await workerRuntime.close();
  }

  await context.setOffline(false);
  const coordinator = await context.newPage();
  await coordinator.goto('/');
  await coordinator.getByText('Runtime & data', { exact: true }).click();

  // Force detail block 91 into the one-block cache, then select a token in
  // block 357 only after the backing file is truncated. A same-release install
  // is already queued ahead of the stale corruption report, exercising the
  // per-install-ID ABA guard rather than only a manifest identity check.
  await page.getByLabel('Entity spans').fill('');
  await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('猫');
  await page.getByRole('button', { name: 'Analyze' }).click();
  await expect(page.locator('.dictionary-forms')).toContainText('猫');
  await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('鮟鱇を食べる');
  await page.getByRole('button', { name: 'Analyze' }).click();
  const anglerfish = page.getByRole('button', { name: /鮟鱇/ }).first();
  await expect(anglerfish).toBeVisible();

  const oldInstallId = await committedInstallId(page);
  expect(oldInstallId).toMatch(INSTALL_ID_PATTERN);
  await coordinator.evaluate(async directoryName => {
    const root = await navigator.storage.getDirectory();
    const directory = await root.getDirectoryHandle(directoryName);
    const details = await directory.getFileHandle('details.bin');
    const writable = await details.createWritable();
    await writable.close();
  }, DIRECTORY_NAME);

  const releaseAbaLock = await holdInstallLifecycleLock(coordinator);
  await anglerfish.click();
  await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
    .toEqual(['shared']);
  await queueStandaloneInstall(coordinator);
  await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
    .toEqual(['shared', 'exclusive']);
  await releaseAbaLock();
  await waitForStandaloneInstall(coordinator);
  await expect(page.locator('.dictionary-forms'))
    .toContainText('鮟鱇', { timeout: 180_000 });
  await expect(page.getByText('potbellied sumo wrestler', { exact: true })).toBeVisible();

  const newInstallId = await committedInstallId(page);
  expect(newInstallId).toMatch(INSTALL_ID_PATTERN);
  expect(newInstallId).not.toBe(oldInstallId);
  await expect(page.getByText('Ready offline')).toBeVisible();

  // A warm runtime request must wait behind an exclusive lifecycle mutation.
  await page.getByLabel('Entity spans').fill('');
  await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('猫');
  const releaseRuntimeLock = await holdInstallLifecycleLock(coordinator);
  await page.getByRole('button', { name: 'Analyze' }).click();
  await expect(page.getByText('Analyzing…')).toBeVisible();
  await page.waitForTimeout(250);
  await expect(page.getByRole('button', { name: /猫/ })).toHaveCount(0);
  await releaseRuntimeLock();
  await expect(page.getByRole('button', { name: /猫/ }).first()).toBeVisible();

  // Queue a stale-tab read behind a cross-tab writer. Once clear commits, that
  // already-waiting reader must observe the new install ID before using runtime.
  const releaseSharedLock = await holdInstallLifecycleLock(page, 'shared');
  await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('犬');
  await page.getByRole('button', { name: 'Analyze' }).click();
  await expect(page.getByRole('button', { name: /犬/ }).first()).toBeVisible();
  // A one-token result automatically starts describe(). Finish that shared
  // runtime read before queueing clear, otherwise clear can win the lock queue
  // and make describe invalidate this tab before the explicit stale read below.
  await expect(page.locator('.dictionary-forms')).toContainText('犬');
  coordinator.once('dialog', dialog => dialog.accept());
  await coordinator.getByRole('button', { name: 'Clear installed data' }).click();
  await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
    .toContain('exclusive');
  expect((await opfsSnapshot(page)).markerBytes).not.toBeNull();

  await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('鳥');
  await page.getByRole('button', { name: 'Analyze' }).click();
  await expect(page.getByText('Analyzing…')).toBeVisible();
  await page.waitForTimeout(250);
  await expect(page.getByRole('button', { name: /鳥/ })).toHaveCount(0);

  await releaseSharedLock();
  await expect(coordinator.getByRole('button', { name: 'Install analyzer data' })).toBeVisible();
  await expect(page.getByRole('button', { name: 'Install analyzer data' })).toBeVisible();
  expect(await committedInstallId(page)).toBeNull();

  const releaseInstallLock = await holdInstallLifecycleLock(coordinator);
  await page.getByRole('button', { name: 'Install analyzer data' }).click();
  await page.waitForTimeout(250);
  expectNoInstalledFiles(await opfsSnapshot(page));
  expect(await committedInstallId(page)).toBeNull();
  await releaseInstallLock();
  await expect(page.getByText('Ready offline')).toBeVisible({ timeout: 180_000 });
  expect(await committedInstallId(page)).toMatch(INSTALL_ID_PATTERN);
  await coordinator.close();
  await page.evaluate(async () => {
    const root = await navigator.storage.getDirectory();
    const directory = await root.getDirectoryHandle('ichiran-browser-alpha');
    const handle = await directory.getFileHandle('hot.bin');
    const file = await handle.getFile();
    const offset = 128;
    const original = new Uint8Array(await file.slice(offset, offset + 1).arrayBuffer())[0]!;
    const writable = await handle.createWritable({ keepExistingData: true });
    await writable.seek(offset);
    await writable.write(Uint8Array.of(original ^ 0xff));
    await writable.close();
  });
  await page.reload();
  await expect(page.getByText('Analyzer data is incomplete or corrupted.')).toBeVisible();
  await expect(page.getByRole('button', { name: 'Reinstall' })).toBeVisible();
  } finally {
    await context?.close().catch(() => undefined);
    await rm(profileDirectory, { recursive: true, force: true });
  }
});
