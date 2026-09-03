import { defineConfig } from 'playwright/test';

export default defineConfig({
  testDir: './e2e',
  fullyParallel: false,
  workers: 1,
  // The exhaustive corpus test deliberately adds host CPU contention; its own
  // Worker-latency assertions remain the product performance gates.
  timeout: 25 * 60 * 1000,
  expect: { timeout: 60_000 },
  use: {
    baseURL: 'http://127.0.0.1:4173',
    headless: true,
    serviceWorkers: 'block',
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure'
  },
  webServer: {
    command: 'bun run preview --host 127.0.0.1',
    url: 'http://127.0.0.1:4173',
    reuseExistingServer: false,
    timeout: 60_000
  }
});
