import { createRoot } from 'react-dom/client';
import { App, type OfflineShellResult } from './App.js';
import './styles.css';

const root = document.getElementById('root');
if (!root) throw new Error('Missing application root');

const OFFLINE_SHELL_ACTIVATION_TIMEOUT_MS = 30_000;

function waitForOfflineController(
  registration: ServiceWorkerRegistration
): Promise<void> {
  if (navigator.serviceWorker.controller) return Promise.resolve();
  return new Promise((resolve, reject) => {
    let watchedWorker: ServiceWorker | null = null;
    const finish = (error?: Error): void => {
      clearTimeout(timeout);
      navigator.serviceWorker.removeEventListener('controllerchange', controllerChanged);
      registration.removeEventListener('updatefound', updateFound);
      watchedWorker?.removeEventListener('statechange', workerStateChanged);
      if (error) reject(error);
      else resolve();
    };
    const controllerChanged = (): void => finish();
    const workerStateChanged = (): void => {
      if (watchedWorker?.state === 'redundant') {
        finish(new Error(
          'Offline app shell installation failed. Check your connection, then reload and retry.'
        ));
      }
    };
    const watchInstallingWorker = (): void => {
      watchedWorker?.removeEventListener('statechange', workerStateChanged);
      watchedWorker = registration.installing;
      watchedWorker?.addEventListener('statechange', workerStateChanged);
      workerStateChanged();
    };
    const updateFound = (): void => watchInstallingWorker();
    const timeout = setTimeout(() => finish(new Error(
      'Offline app shell activation timed out. Check your connection, then reload and retry.'
    )), OFFLINE_SHELL_ACTIVATION_TIMEOUT_MS);
    navigator.serviceWorker.addEventListener('controllerchange', controllerChanged);
    registration.addEventListener('updatefound', updateFound);
    watchInstallingWorker();
    if (navigator.serviceWorker.controller) finish();
  });
}

async function prepareOfflineShell(): Promise<OfflineShellResult> {
  if (!import.meta.env.PROD) return { ready: true };
  if (!('serviceWorker' in navigator)) {
    return { ready: false, message: 'Service Workers are unavailable.' };
  }
  try {
    const registration = await navigator.serviceWorker.register('/sw.js', {
      updateViaCache: 'none'
    });
    await waitForOfflineController(registration);
    return { ready: true, registration };
  } catch (error) {
    return {
      ready: false,
      message: error instanceof Error ? error.message : String(error)
    };
  }
}

createRoot(root).render(<App offlineShellReady={prepareOfflineShell()} />);
