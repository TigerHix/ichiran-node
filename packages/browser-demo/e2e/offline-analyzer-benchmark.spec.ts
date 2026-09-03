import type { ChildProcess } from 'node:child_process';
import { mkdir, mkdtemp, rm, writeFile } from 'node:fs/promises';
import { cpus, platform, release, tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import type { BrowserContext } from 'playwright/test';
import type {
  AnalysisResult,
  AnalyzerPackManifest
} from '../src/protocol.js';
import type { BenchmarkResult } from '../src/qualification-client.js';
import {
  expect,
  isExpectedOfflineFetchFailure,
  test,
  watchConsoleHealth
} from './console-health.js';
import {
  BASE_URL,
  INSTALL_ID_PATTERN,
  attachAnalyzerWorker,
  committedInstallId,
  denyPersistentStorage,
  expectInstallablePwa,
  expectNoHorizontalOverflow,
  median,
  opfsSnapshot,
  runtimeValue,
  singleCpuAffinity,
  startCpuHogs,
  stopCpuHogs
} from './offline-analyzer-helpers.js';

// The exhaustive corpus runs under a calibrated 6x single-core contention
// proxy. Keep the outer watchdog above the measured sweep so final report
// download and process cleanup have deterministic headroom.
test.setTimeout(40 * 60 * 1000);
test.skip(
  process.env.ICHIRAN_BROWSER_QUALIFICATION !== '1',
  'requires the explicit browser qualification build'
);

test('installs once, restarts offline, and meets the 6x proxy', async ({
  browser
}) => {
  const browserType = browser.browserType();
  const profileDirectory = await mkdtemp(join(tmpdir(), 'ichiran-browser-alpha-e2e-'));
  let context: BrowserContext | null = null;
  let deliberatelyOfflineProbe: string | null = null;
  try {
    context = await browserType.launchPersistentContext(profileDirectory, {
      baseURL: BASE_URL,
      headless: true,
      permissions: ['clipboard-read', 'clipboard-write'],
      serviceWorkers: 'allow',
      viewport: { width: 390, height: 844 }
    });
    watchConsoleHealth(context);
    await denyPersistentStorage(context);
    let page = context.pages()[0] ?? await context.newPage();
    await page.setViewportSize({ width: 390, height: 844 });
    await page.goto('/?qualification=1');
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

    expect(await page.evaluate(() => navigator.serviceWorker.controller !== null)).toBe(true);
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
    watchConsoleHealth(
      context,
      failure => isExpectedOfflineFetchFailure(failure, deliberatelyOfflineProbe)
    );
    await denyPersistentStorage(context);
    await context.setOffline(true);
    page = context.pages()[0] ?? await context.newPage();
    await page.goto('/?qualification=1');
    // Chromium flips navigator.onLine back to true after a cached Service Worker
    // navigation, so prove the transport is offline with a URL the worker ignores.
    deliberatelyOfflineProbe = `/__ichiran-offline-probe-${Date.now()}`;
    expect(await page.evaluate(async probe => {
      try {
        await fetch(probe, { cache: 'no-store' });
        return false;
      } catch {
        return true;
      }
    }, deliberatelyOfflineProbe)).toBe(true);
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

      await expect.poll(() => page.evaluate(() => Boolean(
        (window as typeof window & { __ichiranQualification?: unknown })
          .__ichiranQualification
      ))).toBe(true);
      // This watchdog includes the entire corpus under induced host contention.
      const benchmark = await page.evaluate(() => (
        window as typeof window & {
          __ichiranQualification: { benchmark(): Promise<BenchmarkResult> };
        }
      ).__ichiranQualification.benchmark());
      expect(benchmark.release).toEqual(manifest);
      expect(benchmark.corpusVersion).toBe(3);
      expect(benchmark.groups.map(group => group.corpus)).toEqual([
        'ordinary', 'pathological-morphology', 'dense-contiguous-boundary'
      ]);
      const exactP95 = Object.fromEntries(
        benchmark.groups.map(group => [group.corpus, group.p95Ms])
      );
      expect(benchmark.diagnostics.analyzeGroups.map(group => [group.corpus, group.samples])).toEqual([
        ['segmentation-short', 4590],
        ['long-noun-compound', 500],
        ['hiragana-colloquial', 500],
        ['modern-mixed-script', 500],
        ['top-n', 20],
        ['entities', 540],
        ['counters', 2000],
        ['numbers', 70],
        ['paragraph-scaling', 50]
      ]);
      expect(benchmark.diagnostics.entry.samples).toBe(500);
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
      expect(exactP95.ordinary).toBeLessThanOrEqual(75);
      expect(exactP95['pathological-morphology']).toBeLessThanOrEqual(250);
      expect(exactP95['dense-contiguous-boundary']).toBeLessThanOrEqual(500);
      expect(analyzerRequests).toEqual([]);
    } finally {
      page.off('request', recordRequest);
      await stopCpuHogs(hogs);
      await workerRuntime.close();
    }
  } finally {
    if (context) {
      // WSL Chromium can finish every assertion and still leave an offline
      // persistent-context close unresolved. The outer E2E process group owns
      // the final browser reap, so teardown must not consume the test watchdog.
      await Promise.race([
        context.close().catch(() => undefined),
        new Promise(resolve => setTimeout(resolve, 5_000))
      ]);
    }
    await rm(profileDirectory, { recursive: true, force: true });
  }
});
