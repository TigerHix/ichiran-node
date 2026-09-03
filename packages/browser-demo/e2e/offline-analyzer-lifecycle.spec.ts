import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import type { BrowserContext } from 'playwright/test';
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
  denyPersistentStorage,
  expectNoInstalledFiles,
  holdInstallLifecycleLock,
  opfsSnapshot,
  pendingInstallLifecycleLocks,
  prepareStandaloneInstall,
  removeAnalyzerData,
  startStandaloneInstall,
  waitForStandaloneInstall
} from './offline-analyzer-helpers.js';

test('repairs cross-tab ABA races and detects runtime corruption', async ({ browser }) => {
  const browserType = browser.browserType();
  const profileDirectory = await mkdtemp(join(tmpdir(), 'ichiran-browser-alpha-lifecycle-e2e-'));
  let context: BrowserContext | null = null;
  try {
    context = await browserType.launchPersistentContext(profileDirectory, {
      baseURL: BASE_URL,
      headless: true,
      serviceWorkers: 'block',
      viewport: { width: 390, height: 844 }
    });
    watchConsoleHealth(context);
    await denyPersistentStorage(context);
    const page = context.pages()[0] ?? await context.newPage();
    await page.goto('/');
    await page.getByRole('button', { name: 'Install analyzer data' }).click();
    await expect(analyzerReady(page)).toBeVisible({ timeout: 180_000 });

    const coordinator = await context.newPage();
    await coordinator.goto('/');

    // Force detail block 91 into the one-block cache, then select a token in
    // block 357 only after the backing file is truncated. A same-release install
    // is already queued ahead of the stale corruption report, exercising the
    // per-install-ID ABA guard rather than only a manifest identity check.
    await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('猫');
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    await expect(page.locator('.word-details:visible > .detail-content > .token-meanings'))
      .toContainText('cat');
    await page.getByRole('button', { name: 'Close', exact: true }).click();
    await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('鮟鱇を食べる');
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    const anglerfish = page.getByRole('button', { name: /鮟鱇/ }).first();
    await expect(anglerfish).toBeVisible();

    const oldInstallId = await committedInstallId(page);
    expect(oldInstallId).toMatch(INSTALL_ID_PATTERN);
    const abaFiles = await activeOpfsFiles(coordinator);
    await prepareStandaloneInstall(coordinator);
    await coordinator.evaluate(async ({ directoryName, detailsName }) => {
      const root = await navigator.storage.getDirectory();
      const directory = await root.getDirectoryHandle(directoryName);
      const details = await directory.getFileHandle(detailsName);
      const writable = await details.createWritable();
      await writable.close();
    }, { directoryName: DIRECTORY_NAME, detailsName: abaFiles.details });

    const releaseAbaLock = await holdInstallLifecycleLock(coordinator);
    await anglerfish.click();
    await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
      .toEqual(['shared']);
    await startStandaloneInstall(coordinator);
    await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
      .toEqual(['shared', 'exclusive']);
    await releaseAbaLock();
    expect(await waitForStandaloneInstall(coordinator)).toBeNull();
    await expect(page.locator('.word-details:visible > .detail-content > .token-meanings'))
      .toContainText('potbellied sumo wrestler', { timeout: 180_000 });
    await page.getByRole('button', { name: 'Close', exact: true }).click();

    const newInstallId = await committedInstallId(page);
    expect(newInstallId).toMatch(INSTALL_ID_PATTERN);
    expect(newInstallId).not.toBe(oldInstallId);
    await expect(analyzerReady(page)).toBeVisible();

    // Exercise the opposite lock order: the stale runtime quarantines its old
    // generation first, a repair already queued behind it commits next, and the
    // Worker reopens that generation before retrying the interrupted detail read.
    await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('猫');
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    await expect(page.locator('.word-details:visible > .detail-content > .token-meanings'))
      .toContainText('cat');
    await page.getByRole('button', { name: 'Close', exact: true }).click();
    await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('鮟鱇を食べる');
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    const repairedAnglerfish = page.getByRole('button', { name: /鮟鱇/ }).first();
    await expect(repairedAnglerfish).toBeVisible();

    const quarantineFirstInstallId = await committedInstallId(page);
    expect(quarantineFirstInstallId).toBe(newInstallId);
    const quarantineFiles = await activeOpfsFiles(coordinator);
    await prepareStandaloneInstall(coordinator);
    await coordinator.evaluate(async ({ directoryName, detailsName }) => {
      const root = await navigator.storage.getDirectory();
      const directory = await root.getDirectoryHandle(directoryName);
      const details = await directory.getFileHandle(detailsName);
      const writable = await details.createWritable();
      await writable.close();
    }, { directoryName: DIRECTORY_NAME, detailsName: quarantineFiles.details });

    const releaseOuterLock = await holdInstallLifecycleLock(coordinator);
    await repairedAnglerfish.click();
    await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
      .toEqual(['shared']);
    const barrier = holdInstallLifecycleLock(page);
    await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
      .toEqual(['shared', 'exclusive']);
    await releaseOuterLock();
    const releaseBarrier = await barrier;
    // The shared read has now failed, and the only pending exclusive request is
    // its install-ID-specific quarantine. The detecting Worker cannot probe for
    // a replacement until that request completes.
    await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
      .toEqual(['exclusive']);
    expect(await committedInstallId(page)).toBe(quarantineFirstInstallId);
    await startStandaloneInstall(coordinator);
    await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
      .toEqual(['exclusive', 'exclusive']);
    await releaseBarrier();

    expect(await waitForStandaloneInstall(coordinator)).toBeNull();
    await expect(page.locator('.word-details:visible > .detail-content > .token-meanings'))
      .toContainText('potbellied sumo wrestler', { timeout: 180_000 });
    await page.getByRole('button', { name: 'Close', exact: true }).click();
    const repairedInstallId = await committedInstallId(page);
    expect(repairedInstallId).toMatch(INSTALL_ID_PATTERN);
    expect(repairedInstallId).not.toBe(quarantineFirstInstallId);
    await expect(analyzerReady(page)).toBeVisible();

    // A warm runtime request must wait behind an exclusive lifecycle mutation.
    await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('猫');
    const releaseRuntimeLock = await holdInstallLifecycleLock(coordinator);
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    await expect(page.getByText('Analyzing', { exact: true })).toBeVisible();
    await page.waitForTimeout(250);
    await expect(page.getByRole('button', { name: /猫/ })).toHaveCount(0);
    await releaseRuntimeLock();
    // The one-token result opens a modal detail sheet, making the sentence
    // intentionally inert to the accessibility tree until the sheet closes.
    await expect(page.locator('.word-details:visible > .detail-content > .token-meanings'))
      .toContainText('cat');
    await page.getByRole('button', { name: 'Close', exact: true }).click();

    // Queue a stale-tab read behind a cross-tab writer. Once clear commits, that
    // already-waiting reader must observe the new install ID before using runtime.
    const releaseSharedLock = await holdInstallLifecycleLock(page, 'shared');
    await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('犬');
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    // A one-token result automatically opens its entry. Finish that shared
    // runtime read before queueing clear, otherwise clear can win the lock queue
    // and invalidate this tab before the explicit stale read below.
    await expect(page.locator('.word-details:visible > .detail-content > .token-meanings'))
      .toContainText('dog');
    await page.getByRole('button', { name: 'Close', exact: true }).click();
    coordinator.once('dialog', dialog => dialog.accept());
    await removeAnalyzerData(coordinator);
    await expect.poll(() => pendingInstallLifecycleLocks(coordinator))
      .toContain('exclusive');
    expect((await opfsSnapshot(page)).markerBytes).not.toBeNull();

    await page.getByRole('textbox', { name: 'Japanese text', exact: true }).fill('鳥');
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    await expect(page.getByText('Analyzing', { exact: true })).toBeVisible();
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
    await expect(analyzerReady(page)).toBeVisible({ timeout: 180_000 });
    expect(await committedInstallId(page)).toMatch(INSTALL_ID_PATTERN);
    await coordinator.close();
    const corruptFiles = await activeOpfsFiles(page);
    await page.evaluate(async ({ directoryName, hotName }) => {
      const root = await navigator.storage.getDirectory();
      const directory = await root.getDirectoryHandle(directoryName);
      const handle = await directory.getFileHandle(hotName);
      // Preserve the ICHIPACK magic but make the installed header version unsupported.
      const offset = 8;
      const writable = await handle.createWritable({ keepExistingData: true });
      await writable.seek(offset);
      await writable.write(Uint8Array.of(2, 0));
      await writable.close();
    }, { directoryName: DIRECTORY_NAME, hotName: corruptFiles.hot });
    await page.reload();
    await expect(page.getByText('The saved data is incomplete. Install it again.')).toBeVisible();
    await expect(page.getByRole('button', { name: 'Reinstall analyzer data' })).toBeVisible();
  } finally {
    await context?.close().catch(() => undefined);
    await rm(profileDirectory, { recursive: true, force: true });
  }
});
