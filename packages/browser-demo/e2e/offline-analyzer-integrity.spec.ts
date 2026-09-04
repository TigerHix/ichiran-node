import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import type { Route } from 'playwright/test';
import type { AnalyzerPackManifest } from '../src/protocol.js';
import {
  expect,
  test,
  watchConsoleHealth
} from './console-health.js';
import {
  BASE_URL,
  DIRECTORY_NAME,
  INSTALL_ID_PATTERN,
  activeOpfsFiles,
  analyzerReady,
  committedInstallId,
  expectNoInstalledFiles,
  gzipAsset,
  identityAsset,
  interruptInstall,
  mockWorkerStorageEstimateFromAppFiles,
  openIsolatedContext,
  opfsSnapshot,
  queueStandaloneInstall,
  rejectedInstall,
  type RoutedAsset,
  sha256,
  signedManifest,
  staleInstallFiles,
  waitForStandaloneInstall,
  withReleaseIdentity,
  writeCommittedInstallId
} from './offline-analyzer-helpers.js';

test('does not register a Service Worker', async ({ browser }) => {
  const context = await browser.newContext({
    baseURL: BASE_URL,
    serviceWorkers: 'allow'
  });
  watchConsoleHealth(context);
  try {
    const page = await context.newPage();
    await page.goto('/');
    await page.waitForTimeout(250);
    expect(await page.evaluate(async () => ({
      controller: navigator.serviceWorker.controller,
      registrations: (await navigator.serviceWorker.getRegistrations()).length
    }))).toEqual({ controller: null, registrations: 0 });
  } finally {
    await context.close();
  }
});

test('shows the unsupported screen instead of constructing a missing Worker', async ({ browser }) => {
  const context = await browser.newContext({ serviceWorkers: 'block' });
  watchConsoleHealth(context);
  try {
    await context.addInitScript(() => {
      Object.defineProperty(window, 'Worker', { configurable: true, value: undefined });
    });
    const page = await context.newPage();
    await page.goto('/');
    await expect(page.getByRole('heading', {
      name: 'This browser cannot store the analyzer locally.'
    })).toBeVisible();
  } finally {
    await context.close();
  }
});

test('rejects bad manifest, transfer, and installed digests without committing ready', async ({
  browser
}) => {
  const hotBytes = Uint8Array.from({ length: 64 }, (_, index) => index);
  const definitionBytes = Uint8Array.of(9, 8, 7, 6);
  const hot = identityAsset('hot.bin', hotBytes);
  const definition = identityAsset('lexicon.bin', definitionBytes);
  const valid = signedManifest(hot.manifest, definition.manifest);

  await rejectedInstall(
    browser,
    { ...valid, manifestSha256: '0'.repeat(64) },
    hot,
    definition,
    'Analyzer manifest checksum does not match',
    undefined,
    true
  );
  await rejectedInstall(
    browser,
    valid,
    hot,
    definition,
    'Downloaded 11 bytes; expected 64',
    { hot: hotBytes.slice(0, 11) }
  );
  const corruptTransfer = Uint8Array.from(hotBytes);
  corruptTransfer[17] ^= 0xff;
  await rejectedInstall(
    browser,
    valid,
    hot,
    definition,
    'Downloaded asset checksum does not match',
    { hot: corruptTransfer }
  );

  const installed = Uint8Array.from({ length: 128 }, (_, index) => index ^ 0x5a);
  const compressed = gzipAsset('hot.bin.gz', installed);
  const wrongInstalledDigest = sha256(Uint8Array.from(installed, value => value ^ 0xff));
  const badInstalledAsset: RoutedAsset = {
    ...compressed,
    manifest: { ...compressed.manifest, installedSha256: wrongInstalledDigest }
  };
  await rejectedInstall(
    browser,
    signedManifest(badInstalledAsset.manifest, definition.manifest),
    badInstalledAsset,
    definition,
    'Installed asset checksum does not match'
  );
});

test('interrupted hot and lexicon installs never commit a ready marker', async ({ browser }) => {
  await interruptInstall(browser, 'hot');
  await interruptInstall(browser, 'lexicon');
});

test('opens the installed OPFS pack when no usable published manifest is available', async ({ browser }) => {
  const context = await openIsolatedContext(browser);
  try {
    const page = await context.newPage();
    await page.goto('/');
    await page.getByRole('button', { name: 'Install analyzer data' }).click();
    await expect(analyzerReady(page)).toBeVisible({ timeout: 180_000 });

    await page.route('**/analyzer/manifest.json', route => route.fulfill({
      contentType: 'application/json',
      body: '{"formatVersion":1}'
    }));
    await page.reload();
    await expect(analyzerReady(page)).toBeVisible();
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    await expect(page.getByRole('button', { name: /今日/ }).first()).toBeVisible();
  } finally {
    await context.close();
  }
});

test('removes slot-less legacy files without opening their marker', async ({ browser }) => {
  const context = await openIsolatedContext(browser);
  try {
    const page = await context.newPage();
    await page.goto('/');
    const legacyInstallId = '00000000-0000-4000-8000-000000000001';
    await page.evaluate(async directoryName => {
      const root = await navigator.storage.getDirectory();
      const directory = await root.getDirectoryHandle(directoryName, { create: true });
      const write = async (name: string, value: string | Uint8Array): Promise<void> => {
        const handle = await directory.getFileHandle(name, { create: true });
        const writable = await handle.createWritable();
        if (typeof value === 'string') {
          await writable.write(value);
        } else {
          const bytes = new ArrayBuffer(value.byteLength);
          new Uint8Array(bytes).set(value);
          await writable.write(bytes);
        }
        await writable.close();
      };
      // Invalid JSON proves the retired marker is deleted, never parsed.
      await write('install.json', '{this is not a marker');
      await write('hot.bin', Uint8Array.of(1, 2, 3));
      await write('details.bin', Uint8Array.of(4, 5));
    }, DIRECTORY_NAME);
    await writeCommittedInstallId(page, legacyInstallId);

    await page.reload();
    await expect(page.getByRole('button', { name: 'Install analyzer data' })).toBeEnabled();
    expect(await committedInstallId(page)).toBeNull();
    expect(await staleInstallFiles(page)).toEqual([]);
    expectNoInstalledFiles(await opfsSnapshot(page));
  } finally {
    await context.close();
  }
});

test('clears an unverified generation before estimating reinstall capacity', async ({ browser }) => {
  const manifest = JSON.parse(
    await readFile(resolve(import.meta.dirname, '../dist/analyzer/manifest.json'), 'utf8')
  ) as AnalyzerPackManifest;
  const requiredBytes = manifest.hot.installedBytes
    + manifest.lexicon.installedBytes
    + Object.values(manifest.locales).reduce((total, asset) => total + asset.installedBytes, 0)
    + Math.max(
      ...[manifest.hot, manifest.lexicon, ...Object.values(manifest.locales)].map(asset =>
        asset.encoding === 'gzip' ? asset.downloadBytes : 0)
    );
  const context = await openIsolatedContext(browser);
  try {
    // The quota is exactly one production install. Any byte retained from the
    // corrupt B slot would reject the primary Reinstall action.
    await mockWorkerStorageEstimateFromAppFiles(context, requiredBytes);
    const page = await context.newPage();
    await page.goto('/');
    const corruptInstallId = '00000000-0000-4000-8000-000000000002';
    await page.evaluate(async directoryName => {
      const root = await navigator.storage.getDirectory();
      const directory = await root.getDirectoryHandle(directoryName, { create: true });
      const write = async (name: string, value: string | Uint8Array): Promise<void> => {
        const handle = await directory.getFileHandle(name, { create: true });
        const writable = await handle.createWritable();
        if (typeof value === 'string') {
          await writable.write(value);
        } else {
          const bytes = new ArrayBuffer(value.byteLength);
          new Uint8Array(bytes).set(value);
          await writable.write(bytes);
        }
        await writable.close();
      };
      await write('install-b.json', '{corrupt marker');
      await write('hot-b.bin', new Uint8Array(96));
      await write('details-b.bin', new Uint8Array(96));
    }, DIRECTORY_NAME);
    await writeCommittedInstallId(page, corruptInstallId);

    await page.reload();
    await expect(page.getByText('The saved data is incomplete. Install it again.')).toBeVisible();
    await page.getByRole('button', { name: 'Reinstall analyzer data' }).click();
    await expect(analyzerReady(page)).toBeVisible();
    expect(await committedInstallId(page)).not.toBe(corruptInstallId);
    expect(await staleInstallFiles(page)).toEqual([]);
    const installed = await opfsSnapshot(page);
    expect(installed.hotBytes).toBe(manifest.hot.installedBytes);
    expect(installed.definitionBytes).toBe(
      manifest.lexicon.installedBytes
        + Object.values(manifest.locales).reduce((total, asset) => total + asset.installedBytes, 0)
    );
    expect(installed.downloadBytes).toBeNull();
  } finally {
    await context.close();
  }
});

test('preserves a compatible pack after failed reinstall and gates an older release', async ({
  browser
}) => {
  const context = await openIsolatedContext(browser);
  try {
    const page = await context.newPage();
    await page.goto('/');
    const manifest = await page.request
      .get('/analyzer/manifest.json')
      .then(response => response.json() as Promise<AnalyzerPackManifest>);
    await page.getByRole('button', { name: 'Install analyzer data' }).click();
    await expect(analyzerReady(page)).toBeVisible({ timeout: 180_000 });
    const firstInstallId = await committedInstallId(page);
    expect(firstInstallId).toMatch(INSTALL_ID_PATTERN);

    // Simulate a process death immediately after the atomic pointer switch but
    // before old-slot cleanup. Cold inspection prunes the full orphan slot.
    const active = await activeOpfsFiles(page);
    const inactiveSlot = active.hot === 'hot-a.bin' ? 'b' : 'a';
    await page.evaluate(async ({ directoryName, active, inactiveSlot }) => {
      const root = await navigator.storage.getDirectory();
      const directory = await root.getDirectoryHandle(directoryName);
      const copy = async (sourceName: string, targetName: string): Promise<void> => {
        const source = await (await directory.getFileHandle(sourceName)).getFile();
        const target = await directory.getFileHandle(targetName, { create: true });
        await source.stream().pipeTo(await target.createWritable());
      };
      await copy(active.hot, `hot-${inactiveSlot}.bin`);
      await copy(active.lexicon, `lexicon-${inactiveSlot}.bin`);
      for (const [locale, source] of Object.entries(active.locales)) {
        await copy(source, `gloss-${locale}-${inactiveSlot}.bin`);
      }
      const activeMarker = JSON.parse(
        await (await directory.getFileHandle(active.marker)).getFile().then(file => file.text())
      ) as Record<string, unknown>;
      const orphan = await directory.getFileHandle(`install-${inactiveSlot}.json`, { create: true });
      const writable = await orphan.createWritable();
      await writable.write(JSON.stringify({
        ...activeMarker,
        installId: crypto.randomUUID(),
        slot: inactiveSlot
      }));
      await writable.close();
    }, { directoryName: DIRECTORY_NAME, active, inactiveSlot });
    const doubled = await opfsSnapshot(page);
    expect(doubled.hotBytes).toBe(manifest.hot.installedBytes * 2);
    const installedDefinitionBytes = manifest.lexicon.installedBytes
      + Object.values(manifest.locales).reduce((total, asset) => total + asset.installedBytes, 0);
    expect(doubled.definitionBytes).toBe(installedDefinitionBytes * 2);
    await page.reload();
    await expect(analyzerReady(page)).toBeVisible();
    const recovered = await opfsSnapshot(page);
    expect(recovered.hotBytes).toBe(manifest.hot.installedBytes);
    expect(recovered.definitionBytes).toBe(installedDefinitionBytes);
    expect(recovered.downloadBytes).toBeNull();
    expect(await committedInstallId(page)).toBe(firstInstallId);

    // A failed same-release replacement writes only the inactive slot. The
    // original generation remains selected and usable after a full restart.
    const rejectLexicon = async (route: Route): Promise<void> => {
      await route.fulfill({
        status: 200,
        headers: { 'content-type': 'application/gzip' },
        body: Buffer.from([0x1f])
      });
    };
    const lexiconPattern = `**/analyzer/${manifest.lexicon.file}`;
    await context.route(lexiconPattern, rejectLexicon);
    await queueStandaloneInstall(page);
    expect(await waitForStandaloneInstall(page)).toContain('Downloaded 1 bytes; expected');
    await context.unroute(lexiconPattern, rejectLexicon);
    expect(await committedInstallId(page)).toBe(firstInstallId);
    await page.reload();
    await expect(analyzerReady(page)).toBeVisible();
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    await expect(page.getByRole('button', { name: /話しました/ }).first()).toBeVisible();

    // Simulate the generation left by the previous deployed manifest. The
    // current manifest is checked before runtime open and demands reinstall.
    const previousRelease = withReleaseIdentity(
      manifest,
      `${manifest.packVersion}.previous`,
      '1'.repeat(40)
    );
    const previousFiles = await activeOpfsFiles(page);
    await page.evaluate(async ({ directoryName, markerName, previousRelease }) => {
      const root = await navigator.storage.getDirectory();
      const directory = await root.getDirectoryHandle(directoryName);
      const handle = await directory.getFileHandle(markerName);
      const marker = JSON.parse(await (await handle.getFile()).text()) as Record<string, unknown>;
      const writable = await handle.createWritable();
      await writable.write(JSON.stringify({ ...marker, manifest: previousRelease }));
      await writable.close();
    }, {
      directoryName: DIRECTORY_NAME,
      markerName: previousFiles.marker,
      previousRelease
    });
    await page.reload();
    await expect(page.getByText('Your local data needs an update.', { exact: false }))
      .toBeVisible();
    await expect(page.getByRole('button', { name: 'Reinstall analyzer data' })).toBeEnabled();
    expect(await committedInstallId(page)).toBe(firstInstallId);

    await page.getByRole('button', { name: 'Reinstall analyzer data' }).click();
    await expect(analyzerReady(page)).toBeVisible({ timeout: 180_000 });
    expect(await committedInstallId(page)).not.toBe(firstInstallId);
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    await expect(page.getByRole('button', { name: /話しました/ }).first()).toBeVisible();
  } finally {
    await context.close();
  }
});
