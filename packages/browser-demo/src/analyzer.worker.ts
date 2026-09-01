/// <reference lib="webworker" />

import {
  parseAnalyzerReleaseManifest,
  type PortableAnalysisResult,
  type PortableAnalyzeOptions
} from '@ichiran/core';
import type {
  AnalyzerPackManifest,
  PackStatus,
  RustM1Metrics,
  WorkerRequest,
  WorkerResponse
} from './protocol.js';
import {
  AnalyzerInstallError,
  clearInstall,
  inspectInstall,
  installAnalyzer,
  installedFiles,
  installedInstallId,
  markInstallCorrupt
} from './worker/install.js';
import { openAnalyzerRuntime } from './worker/runtime.js';
import { isArtifactCorruption } from './worker/artifact-corruption.js';
import { createSerialExecutor } from './worker/serial-executor.js';
import { Sha256 } from './worker/sha256.js';

declare const __ICHIRAN_RUST_M1__: boolean;

// Start fetching/compiling the experimental runtime as the Worker boots so its
// module load overlaps install inspection instead of extending ready latency.
const rustRuntimeModule = __ICHIRAN_RUST_M1__
  ? import('./worker/runtime-rust.js')
  : null;

interface WorkerRuntime {
  analyze(text: string, options?: PortableAnalyzeOptions): Promise<PortableAnalysisResult>;
  describe(entryIndex: number): Promise<unknown>;
  legacy(text: string, options?: PortableAnalyzeOptions): Promise<unknown>;
  romanize(text: string, options?: PortableAnalyzeOptions): Promise<string>;
  dispose?(): void;
  metrics?(): RustM1Metrics;
}

let runtime: WorkerRuntime | null = null;
let runtimeManifestSha256: string | null = null;
let runtimeInstallId: string | null = null;
let expectedRelease: AnalyzerPackManifest | null = null;
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
  runtime?.dispose?.();
  runtime = null;
  runtimeManifestSha256 = null;
  runtimeInstallId = null;
}

function parseExpectedRelease(value: unknown): AnalyzerPackManifest {
  return parseAnalyzerReleaseManifest(
    value,
    text => new Sha256().update(new TextEncoder().encode(text)).digestHex()
  );
}

function requiredExpectedRelease(): AnalyzerPackManifest {
  if (!expectedRelease) {
    throw new WorkerOperationError(
      'release-not-set',
      'The deployed analyzer release must be verified before opening device data.'
    );
  }
  return expectedRelease;
}

function staleStatus(
  installed: Extract<PackStatus, { state: 'ready' }>,
  expected: AnalyzerPackManifest
): PackStatus {
  return {
    state: 'stale',
    message: 'Installed analyzer data belongs to an earlier app release. Reinstall to continue.',
    installedPackVersion: installed.packVersion,
    installedManifestSha256: installed.manifestSha256,
    expectedPackVersion: expected.packVersion,
    expectedManifestSha256: expected.manifestSha256
  };
}

async function openInstalledUnlocked(): Promise<ReturnType<typeof inspectInstall>> {
  const expected = requiredExpectedRelease();
  const status = await inspectInstall(runtime !== null);
  if (status.state !== 'ready') {
    clearRuntime();
    return status;
  }
  if (status.manifestSha256 !== expected.manifestSha256) {
    clearRuntime();
    return staleStatus(status, expected);
  }
  const installId = await installedInstallId();
  if (
    runtime
    && runtimeManifestSha256 === status.manifestSha256
    && runtimeInstallId === installId
  ) {
    return status;
  }
  clearRuntime();
  const files = await installedFiles();
  if (!files) return inspectInstall(false);
  const openRuntime = __ICHIRAN_RUST_M1__
    ? (await rustRuntimeModule!).openRustM1Runtime
    : openAnalyzerRuntime;
  try {
    runtime = await openRuntime(files);
    runtimeManifestSha256 = files.manifest.manifestSha256;
    runtimeInstallId = files.installId;
    return inspectInstall(true);
  } catch (error) {
    if (!isArtifactCorruption(error)) throw error;
    await markInstallCorrupt(files.installId);
    clearRuntime();
    return inspectInstall(false);
  }
}

function openInstalled(): Promise<ReturnType<typeof inspectInstall>> {
  return withInstallLifecycleLock(openInstalledUnlocked);
}

async function withRuntime<T>(operation: (value: WorkerRuntime) => T | Promise<T>): Promise<T> {
  let repairAttempted = false;
  while (true) {
    const outcome = await withLifecycleLock('shared', async () => {
      const installId = await installedInstallId();
      if (
        !runtime
        || runtimeManifestSha256 === null
        || runtimeInstallId === null
        || installId !== runtimeInstallId
      ) {
        return { state: 'open' as const };
      }
      try {
        return { state: 'result' as const, value: await operation(runtime) };
      } catch (error) {
        if (!isArtifactCorruption(error)) throw error;
        return {
          state: 'corrupt' as const,
          installId: runtimeInstallId,
          message: error instanceof Error ? error.message : String(error)
        };
      }
    });
    if (outcome.state === 'result') return outcome.value;
    if (outcome.state === 'corrupt') {
      const marked = await withInstallLifecycleLock(async () => {
        try {
          return await markInstallCorrupt(outcome.installId);
        } finally {
          clearRuntime();
        }
      });
      if (!marked) continue;
      // A replacement may already be queued behind this quarantine. Give that
      // generation one chance to commit and reopen before surfacing damage to
      // the page; otherwise a successful cross-tab repair would strand the
      // request that detected the old generation's corruption.
      if (!repairAttempted) {
        repairAttempted = true;
        try {
          const repaired = await openInstalled();
          if (repaired.state === 'ready') continue;
        } catch {
          // Preserve the confirmed corruption error if storage inspection also
          // fails while probing for a queued replacement.
        }
      }
      throw new WorkerOperationError('corrupt-install', outcome.message);
    }
    const status = await openInstalled();
    if (status.state === 'corrupt') {
      throw new WorkerOperationError('corrupt-install', status.message);
    }
    if (status.state === 'stale') {
      throw new WorkerOperationError('stale-install', status.message);
    }
    if (status.state !== 'ready') {
      throw new WorkerOperationError('not-installed', 'Analyzer data is not ready');
    }
  }
}

async function handle(request: WorkerRequest): Promise<unknown> {
  switch (request.op) {
    case 'expect-release': {
      const release = parseExpectedRelease(request.release);
      expectedRelease = release;
      if (runtimeManifestSha256 !== release.manifestSha256) clearRuntime();
      return openInstalled();
    }
    case 'status':
      return openInstalled();
    case 'install': {
      const release = requiredExpectedRelease();
      return withInstallLifecycleLock(async () => {
        clearRuntime();
        try {
          await installAnalyzer(
            request.manifestUrl,
            (phase, receivedBytes, totalBytes) => {
              post({
                id: request.id,
                type: 'progress',
                phase,
                receivedBytes,
                totalBytes
              });
            },
            release
          );
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
    case 'rust-m1-metrics': {
      if (!__ICHIRAN_RUST_M1__) {
        throw new WorkerOperationError('unsupported-operation', 'Rust M1 metrics are unavailable');
      }
      return withRuntime(value => {
        if (!value.metrics) throw new Error('Rust M1 runtime metrics are unavailable');
        return value.metrics();
      });
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
