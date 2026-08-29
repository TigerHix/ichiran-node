import { createRoot } from 'react-dom/client';
import { App, type OfflineShellResult } from './App.js';
import './styles.css';

const root = document.getElementById('root');
if (!root) throw new Error('Missing application root');

async function prepareOfflineShell(): Promise<OfflineShellResult> {
  if (!import.meta.env.PROD) return { ready: true };
  if (!('serviceWorker' in navigator)) {
    return { ready: false, message: 'Service Workers are unavailable.' };
  }
  try {
    await navigator.serviceWorker.register('/sw.js');
    await navigator.serviceWorker.ready;
    return { ready: true };
  } catch (error) {
    return {
      ready: false,
      message: error instanceof Error ? error.message : String(error)
    };
  }
}

createRoot(root).render(<App offlineShellReady={prepareOfflineShell()} />);
