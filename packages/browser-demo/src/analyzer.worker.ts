/// <reference lib="webworker" />

import type { WorkerRequest, WorkerResponse } from './protocol.js';
import {
  AnalyzerInstallError,
  clearInstall,
  inspectInstall,
  installAnalyzer,
  installedFiles,
  installedManifestSha256,
  markInstallCorrupt
} from './worker/install.js';
import { AnalyzerRuntime } from './worker/runtime.js';
import { createSerialExecutor } from './worker/serial-executor.js';

let runtime: AnalyzerRuntime | null = null;
let runtimeManifestSha256: string | null = null;
const runSerially = createSerialExecutor();
const INSTALL_LIFECYCLE_LOCK = 'ichiran-browser-alpha-install';

class WorkerOperationError extends Error {
  readonly code: string;

  constructor(code: string, message: string) {
    super(message);
    this.name = 'WorkerOperationError';
    this.code = code;
  }
}

function post(response: WorkerResponse): void {
  self.postMessage(response);
}

function withLifecycleLock<T>(
  mode: 'shared' | 'exclusive',
  operation: () => Promise<T>
): Promise<T> {
  return navigator.locks.request(
    INSTALL_LIFECYCLE_LOCK,
    { mode },
    () => operation()
  ).then(result => result);
}

function withInstallLifecycleLock<T>(operation: () => Promise<T>): Promise<T> {
  return withLifecycleLock('exclusive', operation);
}

function clearRuntime(): void {
  runtime = null;
  runtimeManifestSha256 = null;
}

async function openInstalledUnlocked(): Promise<ReturnType<typeof inspectInstall>> {
  const status = await inspectInstall(runtime !== null);
  if (status.state !== 'ready') {
    clearRuntime();
    return status;
  }
  if (runtime && runtimeManifestSha256 === status.manifestSha256) return status;
  clearRuntime();
  const files = await installedFiles();
  if (!files) return inspectInstall(false);
  try {
    runtime = await AnalyzerRuntime.open(files);
    runtimeManifestSha256 = files.manifest.manifestSha256;
    return inspectInstall(true);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    await markInstallCorrupt(files.manifest.manifestSha256, message);
    clearRuntime();
    return inspectInstall(false);
  }
}

function openInstalled(): Promise<ReturnType<typeof inspectInstall>> {
  return withInstallLifecycleLock(openInstalledUnlocked);
}

function isArtifactCorruption(error: unknown): boolean {
  if (!(error instanceof Error)) return false;
  if (error.name === 'DetailStoreError') {
    return (error as Error & { readonly code?: string }).code !== 'out-of-range';
  }
  return new Set([
    'PackFormatError',
    'SurfaceIndexFormatError',
    'RootPayloadFormatError',
    'MorphologyFormatError',
    'AnalyzerSupportFormatError',
    'AnalyzerAnnotationsError'
  ]).has(error.name);
}

async function withRuntime<T>(operation: (value: AnalyzerRuntime) => T | Promise<T>): Promise<T> {
  while (true) {
    const outcome = await withLifecycleLock('shared', async () => {
      const installedIdentity = await installedManifestSha256();
      if (
        !runtime
        || runtimeManifestSha256 === null
        || installedIdentity !== runtimeManifestSha256
      ) {
        return { state: 'open' as const };
      }
      try {
        return { state: 'result' as const, value: await operation(runtime) };
      } catch (error) {
        if (!isArtifactCorruption(error)) throw error;
        return {
          state: 'corrupt' as const,
          manifestSha256: runtimeManifestSha256,
          message: error instanceof Error ? error.message : String(error)
        };
      }
    });
    if (outcome.state === 'result') return outcome.value;
    if (outcome.state === 'corrupt') {
      await withInstallLifecycleLock(async () => {
        await markInstallCorrupt(outcome.manifestSha256, outcome.message);
        if (runtimeManifestSha256 === outcome.manifestSha256) clearRuntime();
      });
      throw new WorkerOperationError('corrupt-install', outcome.message);
    }
    const status = await openInstalled();
    if (status.state === 'corrupt') {
      throw new WorkerOperationError('corrupt-install', status.message);
    }
    if (status.state !== 'ready') {
      throw new WorkerOperationError('not-installed', 'Analyzer data is not ready');
    }
  }
}

async function handle(request: WorkerRequest): Promise<unknown> {
  switch (request.op) {
    case 'status':
      return openInstalled();
    case 'install': {
      return withInstallLifecycleLock(async () => {
        clearRuntime();
        try {
          await installAnalyzer(request.manifestUrl, (phase, receivedBytes, totalBytes) => {
            post({
              id: request.id,
              type: 'progress',
              phase,
              receivedBytes,
              totalBytes
            });
          });
        } catch (error) {
          if (error instanceof AnalyzerInstallError) {
            throw new WorkerOperationError(error.code, error.message);
          }
          if (error instanceof DOMException && error.name === 'QuotaExceededError') {
            throw new WorkerOperationError('insufficient-storage', 'The browser reported that storage is full.');
          }
          throw error;
        }
        return openInstalledUnlocked();
      });
    }
    case 'clear': {
      return withInstallLifecycleLock(async () => {
        clearRuntime();
        await clearInstall();
        return inspectInstall(false);
      });
    }
    case 'describe': {
      return withRuntime(value => value.describe(request.entryIndex));
    }
    case 'analyze': {
      return withRuntime(value => value.analyze(request.text, request.options));
    }
    case 'legacy': {
      return withRuntime(value => value.legacy(request.text, request.options));
    }
    case 'romanize': {
      return withRuntime(value => value.romanize(request.text));
    }
  }
}

self.addEventListener('message', (event: MessageEvent<WorkerRequest>) => {
  const request = event.data;
  void runSerially(() => handle(request)).then(
    (result) => post({ id: request.id, type: 'result', result }),
    (error: unknown) => post({
      id: request.id,
      type: 'error',
      code: error instanceof WorkerOperationError ? error.code : 'worker-error',
      message: error instanceof Error ? error.message : String(error)
    })
  );
});
