import { expect, test, watchConsoleHealth } from './console-health.js';
import { attachAnalyzerWorker } from './offline-analyzer-helpers.js';

test('records actual steady analyzer Worker heap and backing storage', async ({ browser }) => {
  const context = await browser.newContext({
    baseURL: 'http://127.0.0.1:4173',
    serviceWorkers: 'allow'
  });
  watchConsoleHealth(context);
  try {
    const page = await context.newPage();
    await page.goto('/');
    await page.getByRole('button', { name: 'Install analyzer data' }).click();
    await expect(page.getByText('Ready offline')).toBeVisible({ timeout: 180_000 });
    const input = page.getByRole('textbox', { name: 'Japanese text', exact: true });
    await input.fill('猫');
    await page.getByRole('button', { name: 'Analyze' }).click();
    await expect(page.getByRole('button', { name: /猫/ }).first()).toBeVisible();
    const worker = await attachAnalyzerWorker(browser);
    try {
      await worker.collectGarbage();
      const usage = await worker.heapUsage();
      console.log(`RUNTIME_MEMORY=${JSON.stringify({
        mode: process.env.ICHIRAN_RUST_M1 === '1' ? 'rust-m1' : 'typescript',
        ...usage
      })}`);
      expect(usage.usedSize).toBeGreaterThan(0);
      expect(usage.totalSize).toBeGreaterThanOrEqual(usage.usedSize);
      expect(usage.embedderHeapUsedSize).toBeGreaterThanOrEqual(0);
      expect(usage.backingStorageSize).toBeGreaterThan(0);
    } finally {
      await worker.close();
    }
  } finally {
    await context.close();
  }
});
