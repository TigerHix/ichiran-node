import type {
  AnalyzerPackManifest,
  InstallPhase,
  PackAssetManifest,
  PackStatus
} from '../protocol.js';
import { parseAnalyzerReleaseManifest } from '@ichiran/core/release';
import { NETWORK_INACTIVITY_TIMEOUT_MS, fetchBoundedJson } from '../bounded-json-fetch.js';
import { Sha256 } from './sha256.js';

const DIRECTORY_NAME = 'ichiran-browser-alpha';
const MARKER_A_FILE = 'install-a.json';
const MARKER_B_FILE = 'install-b.json';
const HOT_A_FILE = 'hot-a.bin';
const DETAILS_A_FILE = 'details-a.bin';
const HOT_B_FILE = 'hot-b.bin';
const DETAILS_B_FILE = 'details-b.bin';
const DOWNLOAD_FILE = 'asset.download';
const CONTROL_DATABASE = 'ichiran-browser-alpha-control';
const CONTROL_STORE = 'state';
const INSTALL_ID_KEY = 'install-id';
const INSTALL_ID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/;
let controlDatabasePromise: Promise<IDBDatabase> | null = null;

interface InstalledMarker {
  readonly state: 'ready';
  readonly manifest: AnalyzerPackManifest;
  readonly installId: string;
  readonly installedAt: string;
  readonly slot: InstallSlot;
}

type InstallSlot = 'a' | 'b';

interface SlotFiles {
  readonly hot: string;
  readonly details: string;
}

const SLOT_FILES: Record<InstallSlot, SlotFiles> = {
  a: { hot: HOT_A_FILE, details: DETAILS_A_FILE },
  b: { hot: HOT_B_FILE, details: DETAILS_B_FILE }
};
const ALL_DATA_FILES = [
  HOT_A_FILE,
  DETAILS_A_FILE,
  HOT_B_FILE,
  DETAILS_B_FILE
] as const;
const ALL_MARKER_FILES = [MARKER_A_FILE, MARKER_B_FILE] as const;
// Cleanup-only names from the pre-A/B experiment. They are never parsed or opened.
const STALE_INSTALL_FILES = ['install.json', 'hot.bin', 'details.bin'] as const;

export interface InstalledFiles {
  readonly manifest: AnalyzerPackManifest;
  readonly installId: string;
  readonly hot: FileSystemFileHandle;
  readonly details: FileSystemFileHandle;
}

export type InspectedInstall =
  | (Extract<PackStatus, { readonly state: 'ready' }> & {
      readonly files: InstalledFiles;
    })
  | Exclude<PackStatus, { readonly state: 'ready' }>;

export class AnalyzerInstallError extends Error {
  readonly code: 'insufficient-storage' | 'release-changed';

  constructor(code: AnalyzerInstallError['code'], message: string) {
    super(message);
    this.name = 'AnalyzerInstallError';
    this.code = code;
  }
}

export type InstallProgress = (
  phase: InstallPhase,
  receivedBytes: number,
  totalBytes: number
) => void;

export function temporaryInstallBytes(
  assets: readonly PackAssetManifest[]
): number {
  return Math.max(
    0,
    ...assets.map(asset => asset.encoding === 'gzip' ? asset.downloadBytes : 0)
  );
}

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function assertInstallId(value: unknown): asserts value is string {
  if (typeof value !== 'string' || !INSTALL_ID_PATTERN.test(value)) {
    throw new Error('Installed marker has an invalid install ID');
  }
}

function markerFiles(marker: InstalledMarker): SlotFiles {
  return SLOT_FILES[marker.slot];
}

function markerName(slot: InstallSlot): string {
  return slot === 'a' ? MARKER_A_FILE : MARKER_B_FILE;
}

async function cleanupInactiveGeneration(
  directory: FileSystemDirectoryHandle,
  marker: InstalledMarker
): Promise<void> {
  const activeFiles = markerFiles(marker);
  const activeNames = new Set([activeFiles.hot, activeFiles.details]);
  const activeMarker = markerName(marker.slot);
  // An inability to remove an orphan must not quarantine the independently
  // valid committed pack. Every later cold inspection retries this bounded set.
  await Promise.allSettled([
    ...ALL_DATA_FILES
      .filter(name => !activeNames.has(name))
      .map(name => removeIfPresent(directory, name)),
    ...ALL_MARKER_FILES
      .filter(name => name !== activeMarker)
      .map(name => removeIfPresent(directory, name)),
    ...STALE_INSTALL_FILES.map(name => removeIfPresent(directory, name)),
    removeIfPresent(directory, DOWNLOAD_FILE)
  ]);
}

function parseManifest(value: unknown): AnalyzerPackManifest {
  return parseAnalyzerReleaseManifest(
    value,
    text => new Sha256().update(new TextEncoder().encode(text)).digestHex()
  );
}

async function analyzerDirectory(): Promise<FileSystemDirectoryHandle> {
  const root = await navigator.storage.getDirectory();
  return root.getDirectoryHandle(DIRECTORY_NAME, { create: true });
}

async function removeIfPresent(directory: FileSystemDirectoryHandle, name: string): Promise<void> {
  try {
    await directory.removeEntry(name);
  } catch (error) {
    if (!(error instanceof DOMException) || error.name !== 'NotFoundError') throw error;
  }
}

async function abortQuietly(
  writable: FileSystemWritableFileStream,
  reason: unknown
): Promise<void> {
  try {
    await writable.abort(reason);
  } catch {
    // The stream may already have transitioned to errored/closed.
  }
}

async function cancelQuietly(
  reader: ReadableStreamDefaultReader<Uint8Array<ArrayBuffer>>,
  reason: unknown
): Promise<void> {
  try {
    await reader.cancel(reason);
  } catch {
    // The response may already have transitioned to errored/closed.
  }
}

async function fileIfPresent(
  directory: FileSystemDirectoryHandle,
  name: string
): Promise<FileSystemFileHandle | null> {
  try {
    return await directory.getFileHandle(name);
  } catch (error) {
    if (error instanceof DOMException && error.name === 'NotFoundError') return null;
    throw error;
  }
}

async function cleanupStaleInstallFiles(
  directory: FileSystemDirectoryHandle
): Promise<void> {
  await Promise.all(STALE_INSTALL_FILES.map(name => removeIfPresent(directory, name)));
}

async function removeAllInstallFiles(directory: FileSystemDirectoryHandle): Promise<void> {
  await Promise.all([
    ...ALL_MARKER_FILES.map(name => removeIfPresent(directory, name)),
    ...ALL_DATA_FILES.map(name => removeIfPresent(directory, name)),
    ...STALE_INSTALL_FILES.map(name => removeIfPresent(directory, name)),
    removeIfPresent(directory, DOWNLOAD_FILE)
  ]);
}

async function readMarker(
  directory: FileSystemDirectoryHandle,
  name: string
): Promise<InstalledMarker | null> {
  const handle = await fileIfPresent(directory, name);
  if (!handle) return null;
  const file = await handle.getFile();
  if (file.size > 64 * 1024) throw new Error('Installed marker is unexpectedly large');
  const value: unknown = JSON.parse(await file.text());
  if (!isObject(value) || value.state !== 'ready') {
    throw new Error('Installed marker is invalid');
  }
  const manifest = parseManifest(value.manifest);
  assertInstallId(value.installId);
  if (typeof value.installedAt !== 'string') throw new Error('Installed marker has no timestamp');
  if (value.slot !== 'a' && value.slot !== 'b') {
    throw new Error('Installed marker has an invalid data slot');
  }
  return {
    state: value.state,
    manifest,
    installId: value.installId,
    installedAt: value.installedAt,
    slot: value.slot
  };
}

async function writeMarker(
  directory: FileSystemDirectoryHandle,
  name: string,
  marker: InstalledMarker
): Promise<void> {
  const handle = await directory.getFileHandle(name, { create: true });
  const writable = await handle.createWritable();
  await writable.write(JSON.stringify(marker));
  await writable.close();
}

async function readSlotMarker(
  directory: FileSystemDirectoryHandle,
  slot: InstallSlot
): Promise<InstalledMarker | null> {
  const marker = await readMarker(directory, markerName(slot));
  if (!marker) return null;
  if (marker.slot !== slot) {
    throw new Error('Installed marker is stored in the wrong data slot');
  }
  return marker;
}

async function readCommittedMarker(
  directory: FileSystemDirectoryHandle,
  installId: string
): Promise<InstalledMarker | null> {
  let invalidMarker = false;
  for (const slot of ['a', 'b'] as const) {
    try {
      const marker = await readSlotMarker(directory, slot);
      if (marker?.installId === installId) return marker;
    } catch {
      // A killed write to the inactive slot must not hide the committed slot.
      invalidMarker = true;
    }
  }
  if (invalidMarker) throw new Error('The committed analyzer marker is missing or invalid');
  return null;
}

async function hasCompleteGenerationFiles(
  directory: FileSystemDirectoryHandle,
  marker: InstalledMarker
): Promise<boolean> {
  const files = markerFiles(marker);
  const [hot, details] = await Promise.all([
    fileIfPresent(directory, files.hot),
    fileIfPresent(directory, files.details)
  ]);
  if (!hot || !details) return false;
  const [hotFile, detailsFile] = await Promise.all([hot.getFile(), details.getFile()]);
  return hotFile.size === marker.manifest.hot.installedBytes
    && detailsFile.size === marker.manifest.details.installedBytes;
}

function controlDatabase(): Promise<IDBDatabase> {
  if (controlDatabasePromise) return controlDatabasePromise;
  const opening = new Promise<IDBDatabase>((resolve, reject) => {
    const request = indexedDB.open(CONTROL_DATABASE, 1);
    request.onupgradeneeded = () => {
      request.result.createObjectStore(CONTROL_STORE);
    };
    request.onerror = () => {
      if (controlDatabasePromise === opening) controlDatabasePromise = null;
      reject(request.error ?? new Error('Could not open install control'));
    };
    request.onsuccess = () => {
      const database = request.result;
      const reset = () => {
        if (controlDatabasePromise === opening) controlDatabasePromise = null;
      };
      database.addEventListener('close', reset);
      database.addEventListener('versionchange', () => {
        reset();
        database.close();
      });
      resolve(database);
    };
  });
  controlDatabasePromise = opening;
  return opening;
}

async function readInstallId(): Promise<string | null> {
  const database = await controlDatabase();
  return new Promise((resolve, reject) => {
    const request = database.transaction(CONTROL_STORE).objectStore(CONTROL_STORE).get(INSTALL_ID_KEY);
    request.onerror = () => reject(request.error ?? new Error('Could not read install control'));
    request.onsuccess = () => {
      if (request.result === undefined) {
        resolve(null);
        return;
      }
      try {
        assertInstallId(request.result);
        resolve(request.result);
      } catch (error) {
        reject(error);
      }
    };
  });
}

async function writeInstallId(installId: string | null): Promise<void> {
  if (installId !== null) assertInstallId(installId);
  const database = await controlDatabase();
  await new Promise<void>((resolve, reject) => {
    const transaction = database.transaction(
      CONTROL_STORE,
      'readwrite',
      { durability: 'strict' }
    );
    const store = transaction.objectStore(CONTROL_STORE);
    if (installId === null) store.delete(INSTALL_ID_KEY);
    else store.put(installId, INSTALL_ID_KEY);
    transaction.oncomplete = () => resolve();
    transaction.onerror = () => reject(transaction.error ?? new Error('Could not write install control'));
    transaction.onabort = () => reject(transaction.error ?? new Error('Install control write aborted'));
  });
}

async function hashFile(file: File): Promise<string> {
  const hash = new Sha256();
  const reader = file.stream().getReader();
  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    hash.update(value);
  }
  return hash.digestHex();
}

async function download(
  url: URL,
  directory: FileSystemDirectoryHandle,
  name: string,
  asset: PackAssetManifest,
  completedBytes: number,
  totalBytes: number,
  onProgress: InstallProgress
): Promise<FileSystemFileHandle> {
  const controller = new AbortController();
  let timedOut = false;
  let inactivityTimer: ReturnType<typeof setTimeout> | null = null;
  const stopTimer = (): void => {
    if (inactivityTimer !== null) clearTimeout(inactivityTimer);
    inactivityTimer = null;
  };
  const armTimer = (): void => {
    stopTimer();
    inactivityTimer = setTimeout(() => {
      timedOut = true;
      controller.abort();
    }, NETWORK_INACTIVITY_TIMEOUT_MS);
  };
  let reader: ReadableStreamDefaultReader<Uint8Array<ArrayBuffer>> | null = null;
  let writable: FileSystemWritableFileStream | null = null;
  const hash = new Sha256();
  let received = 0;
  try {
    armTimer();
    const response = await fetch(url, {
      cache: 'no-store',
      credentials: 'same-origin',
      signal: controller.signal
    });
    if (!response.ok || !response.body) {
      await response.body?.cancel();
      throw new Error(`Download failed with HTTP ${response.status}`);
    }
    if (response.headers.has('content-encoding')) {
      await response.body.cancel();
      throw new Error(
        'Analyzer assets must be served as opaque gzip files without Content-Encoding'
      );
    }
    const handle = await directory.getFileHandle(name, { create: true });
    writable = await handle.createWritable();
    reader = response.body.getReader();
    while (true) {
      const { done, value } = await reader.read();
      if (done) {
        stopTimer();
        break;
      }
      armTimer();
      received += value.byteLength;
      if (received > asset.downloadBytes) throw new Error('Download exceeds manifest byte length');
      hash.update(value);
      await writable.write(value);
      onProgress('downloading', completedBytes + received, totalBytes);
    }
    await writable.close();
  } catch (error) {
    await Promise.all([
      reader ? cancelQuietly(reader, error) : Promise.resolve(),
      writable ? abortQuietly(writable, error) : Promise.resolve()
    ]);
    if (timedOut) {
      throw new Error(
        'Analyzer download received no data for 30 seconds. Check your connection and retry.'
      );
    }
    throw error;
  } finally {
    stopTimer();
  }
  if (received !== asset.downloadBytes) {
    throw new Error(`Downloaded ${received} bytes; expected ${asset.downloadBytes}`);
  }
  if (hash.digestHex() !== asset.downloadSha256) {
    throw new Error('Downloaded asset checksum does not match');
  }
  return (await directory.getFileHandle(name));
}

async function installAsset(
  manifestUrl: URL,
  directory: FileSystemDirectoryHandle,
  outputName: string,
  asset: PackAssetManifest,
  completedBytes: number,
  totalBytes: number,
  onProgress: InstallProgress
): Promise<void> {
  const assetUrl = new URL(asset.file, manifestUrl);
  if (assetUrl.origin !== self.location.origin) {
    throw new Error('Analyzer assets must be same-origin');
  }
  const downloadName = asset.encoding === 'identity' ? outputName : DOWNLOAD_FILE;
  const downloaded = await download(
    assetUrl,
    directory,
    downloadName,
    asset,
    completedBytes,
    totalBytes,
    onProgress
  );
  onProgress('verifying', completedBytes + asset.downloadBytes, totalBytes);

  let installed: FileSystemFileHandle;
  if (asset.encoding === 'gzip') {
    onProgress('installing', completedBytes + asset.downloadBytes, totalBytes);
    installed = await directory.getFileHandle(outputName, { create: true });
    const writable = await installed.createWritable();
    try {
      const source = (await downloaded.getFile()).stream();
      await source.pipeThrough(new DecompressionStream('gzip')).pipeTo(writable);
    } catch (error) {
      await abortQuietly(writable, error);
      throw error;
    }
    await removeIfPresent(directory, DOWNLOAD_FILE);
  } else {
    installed = downloaded;
  }

  const file = await installed.getFile();
  if (file.size !== asset.installedBytes) {
    throw new Error(`Installed ${file.size} bytes; expected ${asset.installedBytes}`);
  }
  if (asset.encoding === 'gzip' && await hashFile(file) !== asset.installedSha256) {
    throw new Error('Installed asset checksum does not match');
  }
}

export async function inspectInstalled(workerOpen = false): Promise<InspectedInstall> {
  const directory = await analyzerDirectory();
  let installId: string | null;
  try {
    await cleanupStaleInstallFiles(directory);
    installId = await readInstallId();
  } catch (error) {
    return {
      state: 'corrupt',
      message: error instanceof Error ? error.message : String(error)
    };
  }
  if (!installId) {
    const [presentDataFiles, presentMarkerFiles] = await Promise.all([
      Promise.all(ALL_DATA_FILES.map(name => fileIfPresent(directory, name))),
      Promise.all(ALL_MARKER_FILES.map(name => fileIfPresent(directory, name)))
    ]);
    return presentDataFiles.some(Boolean) || presentMarkerFiles.some(Boolean)
      ? { state: 'incomplete', message: 'Analyzer data installation is incomplete.' }
      : { state: 'not-installed' };
  }
  let marker: InstalledMarker | null;
  try {
    marker = await readCommittedMarker(directory, installId);
  } catch (error) {
    return {
      state: 'corrupt',
      message: error instanceof Error ? error.message : String(error)
    };
  }
  if (!marker) {
    const [presentDataFiles, presentMarkerFiles] = await Promise.all([
      Promise.all(ALL_DATA_FILES.map(name => fileIfPresent(directory, name))),
      Promise.all(ALL_MARKER_FILES.map(name => fileIfPresent(directory, name)))
    ]);
    if (!presentDataFiles.some(Boolean) && !presentMarkerFiles.some(Boolean)) {
      try {
        await writeInstallId(null);
        return { state: 'not-installed' };
      } catch (error) {
        return {
          state: 'corrupt',
          message: error instanceof Error ? error.message : String(error)
        };
      }
    }
    return { state: 'incomplete', message: 'Analyzer data installation is incomplete.' };
  }
  if (installId !== marker.installId) {
    return { state: 'corrupt', message: 'Analyzer install IDs do not match.' };
  }
  const files = markerFiles(marker);
  const [hot, details] = await Promise.all([
    fileIfPresent(directory, files.hot),
    fileIfPresent(directory, files.details)
  ]);
  if (!hot || !details) {
    return { state: 'incomplete', message: 'Analyzer data files are missing.' };
  }
  const [hotFile, detailsFile, persistent] = await Promise.all([
    hot.getFile(),
    details.getFile(),
    navigator.storage.persisted()
  ]);
  if (
    hotFile.size !== marker.manifest.hot.installedBytes
    || detailsFile.size !== marker.manifest.details.installedBytes
  ) {
    return { state: 'corrupt', message: 'Analyzer data file sizes do not match the manifest.' };
  }
  await cleanupInactiveGeneration(directory, marker);
  return {
    state: 'ready',
    packVersion: marker.manifest.packVersion,
    manifestSha256: marker.manifest.manifestSha256,
    downloadBytes: marker.manifest.hot.downloadBytes + marker.manifest.details.downloadBytes,
    installedBytes: hotFile.size + detailsFile.size,
    persistent,
    workerOpen,
    files: { manifest: marker.manifest, installId: marker.installId, hot, details }
  };
}

export async function inspectInstall(workerOpen = false): Promise<PackStatus> {
  const inspected = await inspectInstalled(workerOpen);
  if (inspected.state !== 'ready') return inspected;
  const { files: _, ...status } = inspected;
  return status;
}

export async function installAnalyzer(
  manifestLocation: string,
  onProgress: InstallProgress,
  expectedRelease: AnalyzerPackManifest
): Promise<PackStatus> {
  const manifestUrl = new URL(manifestLocation, self.location.href);
  if (manifestUrl.origin !== self.location.origin) {
    throw new Error('Analyzer manifest must be same-origin');
  }
  const manifest = parseManifest(await fetchBoundedJson(
    manifestUrl,
    { cache: 'no-store', credentials: 'same-origin' },
    'Analyzer manifest download'
  ));
  if (manifest.manifestSha256 !== expectedRelease.manifestSha256) {
    throw new AnalyzerInstallError(
      'release-changed',
      'The deployed analyzer release changed. Close every analyzer tab, reopen, and retry.'
    );
  }
  const totalBytes = manifest.hot.downloadBytes + manifest.details.downloadBytes;
  const installedBytes = manifest.hot.installedBytes + manifest.details.installedBytes;
  const temporaryBytes = temporaryInstallBytes([manifest.hot, manifest.details]);
  const directory = await analyzerDirectory();
  await cleanupStaleInstallFiles(directory);
  let previousInstallId: string | null = null;
  try {
    previousInstallId = await readInstallId();
  } catch {
    // An invalid control record cannot select a generation worth preserving.
  }
  let previousMarker: InstalledMarker | null = null;
  if (previousInstallId) {
    try {
      previousMarker = await readCommittedMarker(directory, previousInstallId);
    } catch {
      // A damaged current generation is not eligible for preservation.
    }
  }
  const previousReady = previousMarker?.installId === previousInstallId
    && await hasCompleteGenerationFiles(directory, previousMarker)
    ? previousMarker
    : null;
  if (!previousReady) {
    // No generation can be rolled back to. Remove every app-owned byte before
    // asking for the quota estimate so abandoned files cannot reject reinstall.
    await writeInstallId(null);
    await removeAllInstallFiles(directory);
  }
  const nextSlot: InstallSlot = previousReady?.slot === 'a' ? 'b' : 'a';
  const nextFiles = SLOT_FILES[nextSlot];
  const estimate = await navigator.storage.estimate();
  if (estimate.quota !== undefined && estimate.usage !== undefined) {
    // Only count files that are safe to remove before installing. The active
    // verified pack stays intact until the replacement is fully committed.
    const [existingHot, existingDetails, existingDownload] = await Promise.all([
      fileIfPresent(directory, nextFiles.hot),
      fileIfPresent(directory, nextFiles.details),
      fileIfPresent(directory, DOWNLOAD_FILE)
    ]);
    const reclaimableBytes = (existingHot ? (await existingHot.getFile()).size : 0)
      + (existingDetails ? (await existingDetails.getFile()).size : 0)
      + (existingDownload ? (await existingDownload.getFile()).size : 0);
    const availableBytes = Math.max(0, estimate.quota - estimate.usage + reclaimableBytes);
    const requiredBytes = installedBytes + temporaryBytes;
    if (availableBytes < requiredBytes) {
      throw new AnalyzerInstallError(
        'insufficient-storage',
        `Requires about ${requiredBytes} bytes; storage estimate reports ${availableBytes} bytes available.`
      );
    }
  }

  await Promise.all([
    removeIfPresent(directory, DOWNLOAD_FILE),
    removeIfPresent(directory, nextFiles.hot),
    removeIfPresent(directory, nextFiles.details),
    removeIfPresent(directory, markerName(nextSlot))
  ]);
  const installId = crypto.randomUUID();
  const installedMarker: InstalledMarker = {
    state: 'ready',
    manifest,
    installId,
    installedAt: new Date().toISOString(),
    slot: nextSlot
  };
  try {
    await installAsset(
      manifestUrl,
      directory,
      nextFiles.hot,
      manifest.hot,
      0,
      totalBytes,
      onProgress
    );
    await installAsset(
      manifestUrl,
      directory,
      nextFiles.details,
      manifest.details,
      manifest.hot.downloadBytes,
      totalBytes,
      onProgress
    );
    onProgress('opening', totalBytes, totalBytes);
    await writeMarker(directory, markerName(nextSlot), installedMarker);
  } catch (error) {
    await Promise.all([
      removeIfPresent(directory, nextFiles.hot),
      removeIfPresent(directory, nextFiles.details),
      removeIfPresent(directory, markerName(nextSlot)),
      removeIfPresent(directory, DOWNLOAD_FILE)
    ]);
    throw error;
  }

  try {
    await writeInstallId(installId);
  } catch (error) {
    // The strict IndexedDB write is the sole generation switch. Until it
    // commits, the previous ID continues selecting its immutable marker.
    await Promise.all([
      removeIfPresent(directory, nextFiles.hot),
      removeIfPresent(directory, nextFiles.details),
      removeIfPresent(directory, markerName(nextSlot)),
      removeIfPresent(directory, DOWNLOAD_FILE)
    ]);
    throw error;
  }

  return inspectInstall(false);
}

/** Mark only the ready pack whose identity was observed by the failing runtime. */
export async function markInstallCorrupt(
  installId: string
): Promise<boolean> {
  const directory = await analyzerDirectory();
  const committedInstallId = await readInstallId();
  const marker = committedInstallId
    ? await readCommittedMarker(directory, committedInstallId)
    : null;
  if (
    !marker
    || marker.installId !== installId
    || committedInstallId !== installId
  ) return false;
  // The strict control pointer is the sole authority. The immutable files are
  // left in place for an explicit reinstall or clear to remove.
  await writeInstallId(null);
  return true;
}

/** Cheap identity of the complete install committed under the lifecycle lock. */
export async function installedInstallId(): Promise<string | null> {
  try {
    return await readInstallId();
  } catch {
    return null;
  }
}

export async function clearInstall(): Promise<void> {
  const directory = await analyzerDirectory();
  await writeInstallId(null);
  await removeAllInstallFiles(directory);
}
