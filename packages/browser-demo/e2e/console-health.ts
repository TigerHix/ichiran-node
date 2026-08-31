import {
  expect,
  test as base,
  type BrowserContext,
  type ConsoleMessage,
  type Page
} from 'playwright/test';

export interface BrowserConsoleFailure {
  readonly kind: 'console.error' | 'pageerror';
  readonly pageUrl: string;
  readonly message: string;
  readonly sourceUrl: string;
}

type ConsoleAllowance = (failure: BrowserConsoleFailure) => boolean;

let currentFailures: BrowserConsoleFailure[] | null = null;

export const test = base.extend<{ cleanConsole: void }>({
  cleanConsole: [async ({}, use) => {
    currentFailures = [];
    try {
      await use();
      expect(
        currentFailures,
        'Browser console and uncaught page errors must stay empty'
      ).toEqual([]);
    } finally {
      currentFailures = null;
    }
  }, { auto: true }]
});

export { expect };

export function watchConsoleHealth(
  context: BrowserContext,
  allow: ConsoleAllowance = () => false
): void {
  if (currentFailures === null) {
    throw new Error('watchConsoleHealth must run inside the clean-console Playwright fixture');
  }
  const watched = new WeakSet<Page>();
  const record = (failure: BrowserConsoleFailure): void => {
    if (!allow(failure)) currentFailures?.push(failure);
  };
  const watchPage = (page: Page): void => {
    if (watched.has(page)) return;
    watched.add(page);
    page.on('console', (message: ConsoleMessage) => {
      if (message.type() !== 'error') return;
      record({
        kind: 'console.error',
        pageUrl: page.url(),
        message: message.text(),
        sourceUrl: message.location().url
      });
    });
    page.on('pageerror', error => record({
      kind: 'pageerror',
      pageUrl: page.url(),
      message: error.message,
      sourceUrl: ''
    }));
  };
  for (const page of context.pages()) watchPage(page);
  context.on('page', watchPage);
}

export function isExpectedOfflineFetchFailure(
  failure: BrowserConsoleFailure,
  expectedPath: string | null
): boolean {
  if (
    expectedPath === null
    || failure.kind !== 'console.error'
    || !/^Failed to load resource: net::ERR_(?:FAILED|INTERNET_DISCONNECTED)$/.test(
      failure.message
    )
  ) return false;
  try {
    return new URL(failure.sourceUrl).pathname === expectedPath;
  } catch {
    return false;
  }
}
