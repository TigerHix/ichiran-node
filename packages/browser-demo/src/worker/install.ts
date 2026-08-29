import type {
  AnalyzerPackManifest,
  InstallPhase,
  PackAssetManifest,
  PackStatus
} from '../protocol.js';
import { Sha256 } from './sha256.js';

const DIRECTORY_NAME = 'ichiran-browser-alpha';
const MARKER_FILE = 'install.json';
const HOT_FILE = 'hot.bin';
const DETAILS_FILE = 'details.bin';
const DOWNLOAD_FILE = 'asset.download';

interface InstalledMarker {
  readonly state: 'ready' | 'corrupt';
  readonly manifest: AnalyzerPackManifest;
  readonly installedAt: string;
  readonly message?: string;
}

export interface InstalledFiles {
  readonly manifest: AnalyzerPackManifest;
  readonly hot: FileSystemFileHandle;
  readonly details: FileSystemFileHandle;
}

export class AnalyzerInstallError extends Error {
  readonly code: 'insufficient-storage';

  constructor(message: string) {
    super(message);
    this.name = 'AnalyzerInstallError';
    this.code = 'insufficient-storage';
  }
}

export type InstallProgress = (
  phase: InstallPhase,
  receivedBytes: number,
  totalBytes: number
) => void;

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function assertAsset(value: unknown, label: string): asserts value is PackAssetManifest {
  if (!isObject(value)) throw new Error(`${label} manifest is not an object`);
  const integers = ['downloadBytes', 'installedBytes'] as const;
  for (const key of integers) {
    if (!Number.isSafeInteger(value[key]) || (value[key] as number) <= 0) {
      throw new Error(`${label}.${key} must be a positive integer`);
    }
  }
  for (const key of ['downloadSha256', 'installedSha256'] as const) {
    if (typeof value[key] !== 'string' || !/^[0-9a-f]{64}$/.test(value[key] as string)) {
      throw new Error(`${label}.${key} is not a lowercase SHA-256 digest`);
    }
  }
  if (typeof value.file !== 'string' || value.file.length === 0) {
    throw new Error(`${label}.file is missing`);
  }
  if (value.encoding !== 'identity' && value.encoding !== 'gzip') {
    throw new Error(`${label}.encoding must be identity or gzip`);
  }
  if (
    value.encoding === 'identity'
    && (
      value.downloadBytes !== value.installedBytes
      || value.downloadSha256 !== value.installedSha256
    )
  ) {
    throw new Error(`${label} identity sizes and digests must match`);
  }
}

function parseManifest(value: unknown): AnalyzerPackManifest {
  if (!isObject(value)) throw new Error('Analyzer manifest is not an object');
  if (value.formatVersion !== 1) throw new Error('Unsupported analyzer manifest format');
  for (const key of ['packVersion', 'sourceCommit', 'sourcesLockSha256', 'manifestSha256'] as const) {
    if (typeof value[key] !== 'string' || value[key].length === 0) {
      throw new Error(`Analyzer manifest ${key} is missing`);
    }
  }
  if (!/^[0-9a-f]{64}$/.test(value.manifestSha256 as string)) {
    throw new Error('Analyzer manifest digest is invalid');
  }
  if (!/^[0-9a-f]{64}$/.test(value.sourcesLockSha256 as string)) {
    throw new Error('Analyzer sources-lock digest is invalid');
  }
  assertAsset(value.hot, 'hot');
  assertAsset(value.details, 'details');
  const manifest = value as unknown as AnalyzerPackManifest;
  const digestInput = JSON.stringify({
    formatVersion: manifest.formatVersion,
    packVersion: manifest.packVersion,
    sourceCommit: manifest.sourceCommit,
    sourcesLockSha256: manifest.sourcesLockSha256,
    hot: {
      file: manifest.hot.file,
      encoding: manifest.hot.encoding,
      downloadBytes: manifest.hot.downloadBytes,
      downloadSha256: manifest.hot.downloadSha256,
      installedBytes: manifest.hot.installedBytes,
      installedSha256: manifest.hot.installedSha256
    },
    details: {
      file: manifest.details.file,
      encoding: manifest.details.encoding,
      downloadBytes: manifest.details.downloadBytes,
      downloadSha256: manifest.details.downloadSha256,
      installedBytes: manifest.details.installedBytes,
      installedSha256: manifest.details.installedSha256
    }
  });
  const digest = new Sha256().update(new TextEncoder().encode(digestInput)).digestHex();
  if (digest !== manifest.manifestSha256) {
    throw new Error('Analyzer manifest checksum does not match');
  }
  return manifest;
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

async function readMarker(directory: FileSystemDirectoryHandle): Promise<InstalledMarker | null> {
  const handle = await fileIfPresent(directory, MARKER_FILE);
  if (!handle) return null;
  const file = await handle.getFile();
  if (file.size > 64 * 1024) throw new Error('Installed marker is unexpectedly large');
  const value: unknown = JSON.parse(await file.text());
  if (!isObject(value) || (value.state !== 'ready' && value.state !== 'corrupt')) {
    throw new Error('Installed marker is invalid');
  }
  const manifest = parseManifest(value.manifest);
  if (typeof value.installedAt !== 'string') throw new Error('Installed marker has no timestamp');
  if (value.message !== undefined && typeof value.message !== 'string') {
    throw new Error('Installed marker has an invalid message');
  }
  return {
    state: value.state,
    manifest,
    installedAt: value.installedAt,
    message: value.message
  };
}

async function writeMarker(
  directory: FileSystemDirectoryHandle,
  marker: InstalledMarker
): Promise<void> {
  const handle = await directory.getFileHandle(MARKER_FILE, { create: true });
  const writable = await handle.createWritable();
  await writable.write(JSON.stringify(marker));
  await writable.close();
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
  const response = await fetch(url, { cache: 'no-store', credentials: 'same-origin' });
  if (!response.ok || !response.body) {
    throw new Error(`Download failed with HTTP ${response.status}`);
  }
  if (response.headers.has('content-encoding')) {
    throw new Error('Analyzer assets must be served as opaque gzip files without Content-Encoding');
  }
  const handle = await directory.getFileHandle(name, { create: true });
  const writable = await handle.createWritable();
  const reader = response.body.getReader();
  const hash = new Sha256();
  let received = 0;
  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      received += value.byteLength;
      if (received > asset.downloadBytes) throw new Error('Download exceeds manifest byte length');
      hash.update(value);
      await writable.write(value);
      onProgress('downloading', completedBytes + received, totalBytes);
    }
    await writable.close();
  } catch (error) {
    await abortQuietly(writable, error);
    throw error;
  }
  if (received !== asset.downloadBytes) {
    throw new Error(`Downloaded ${received} bytes; expected ${asset.downloadBytes}`);
  }
  if (hash.digestHex() !== asset.downloadSha256) {
    throw new Error('Downloaded asset checksum does not match');
  }
  return handle;
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

export async function inspectInstall(workerOpen = false): Promise<PackStatus> {
  const directory = await analyzerDirectory();
  let marker: InstalledMarker | null;
  try {
    marker = await readMarker(directory);
  } catch (error) {
    return {
      state: 'corrupt',
      message: error instanceof Error ? error.message : String(error)
    };
  }
  const hot = await fileIfPresent(directory, HOT_FILE);
  const details = await fileIfPresent(directory, DETAILS_FILE);
  if (!marker) {
    return hot || details
      ? { state: 'incomplete', message: 'Analyzer data installation is incomplete.' }
      : { state: 'not-installed' };
  }
  if (marker.state === 'corrupt') {
    return { state: 'corrupt', message: marker.message ?? 'Analyzer data is corrupted.' };
  }
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
  return {
    state: 'ready',
    packVersion: marker.manifest.packVersion,
    manifestSha256: marker.manifest.manifestSha256,
    downloadBytes: marker.manifest.hot.downloadBytes + marker.manifest.details.downloadBytes,
    installedBytes: hotFile.size + detailsFile.size,
    persistent,
    workerOpen
  };
}

export async function installAnalyzer(
  manifestLocation: string,
  onProgress: InstallProgress
): Promise<PackStatus> {
  const manifestUrl = new URL(manifestLocation, self.location.href);
  if (manifestUrl.origin !== self.location.origin) {
    throw new Error('Analyzer manifest must be same-origin');
  }
  const response = await fetch(manifestUrl, { cache: 'no-store', credentials: 'same-origin' });
  if (!response.ok) throw new Error(`Manifest download failed with HTTP ${response.status}`);
  const manifest = parseManifest(await response.json());
  const totalBytes = manifest.hot.downloadBytes + manifest.details.downloadBytes;
  const installedBytes = manifest.hot.installedBytes + manifest.details.installedBytes;
  const temporaryBytes = Math.max(manifest.hot.downloadBytes, manifest.details.downloadBytes);
  const directory = await analyzerDirectory();
  const estimate = await navigator.storage.estimate();
  if (estimate.quota !== undefined && estimate.usage !== undefined) {
    const [existingHot, existingDetails, existingDownload] = await Promise.all([
      fileIfPresent(directory, HOT_FILE),
      fileIfPresent(directory, DETAILS_FILE),
      fileIfPresent(directory, DOWNLOAD_FILE)
    ]);
    const reclaimableBytes = (existingHot ? (await existingHot.getFile()).size : 0)
      + (existingDetails ? (await existingDetails.getFile()).size : 0)
      + (existingDownload ? (await existingDownload.getFile()).size : 0);
    const availableBytes = Math.max(0, estimate.quota - estimate.usage + reclaimableBytes);
    const requiredBytes = installedBytes + temporaryBytes;
    if (availableBytes < requiredBytes) {
      throw new AnalyzerInstallError(
        `Requires about ${requiredBytes} bytes; storage estimate reports ${availableBytes} bytes available.`
      );
    }
  }

  // The marker is the commit record. Remove it before touching payload files so
  // a terminated Worker can leave only an incomplete, never a false-ready, pack.
  await removeIfPresent(directory, MARKER_FILE);
  await Promise.all([
    removeIfPresent(directory, DOWNLOAD_FILE),
    removeIfPresent(directory, HOT_FILE),
    removeIfPresent(directory, DETAILS_FILE)
  ]);
  try {
    await installAsset(
      manifestUrl,
      directory,
      HOT_FILE,
      manifest.hot,
      0,
      totalBytes,
      onProgress
    );
    await installAsset(
      manifestUrl,
      directory,
      DETAILS_FILE,
      manifest.details,
      manifest.hot.downloadBytes,
      totalBytes,
      onProgress
    );
    onProgress('opening', totalBytes, totalBytes);
    await writeMarker(directory, {
      state: 'ready',
      manifest,
      installedAt: new Date().toISOString()
    });
    return inspectInstall(false);
  } catch (error) {
    await removeIfPresent(directory, MARKER_FILE);
    await Promise.all([
      removeIfPresent(directory, HOT_FILE),
      removeIfPresent(directory, DETAILS_FILE),
      removeIfPresent(directory, DOWNLOAD_FILE)
    ]);
    throw error;
  }
}

export async function installedFiles(): Promise<InstalledFiles | null> {
  const directory = await analyzerDirectory();
  const marker = await readMarker(directory);
  if (!marker || marker.state !== 'ready') return null;
  const [hot, details] = await Promise.all([
    fileIfPresent(directory, HOT_FILE),
    fileIfPresent(directory, DETAILS_FILE)
  ]);
  return hot && details ? { manifest: marker.manifest, hot, details } : null;
}

/** Mark only the ready pack whose identity was observed by the failing runtime. */
export async function markInstallCorrupt(
  manifestSha256: string,
  message: string
): Promise<boolean> {
  const directory = await analyzerDirectory();
  const marker = await readMarker(directory);
  if (
    !marker
    || marker.state !== 'ready'
    || marker.manifest.manifestSha256 !== manifestSha256
  ) return false;
  await writeMarker(directory, { ...marker, state: 'corrupt', message });
  return true;
}

/** Identity of the complete install committed by install.json, if one exists. */
export async function installedManifestSha256(): Promise<string | null> {
  const marker = await readMarker(await analyzerDirectory());
  return marker?.state === 'ready' ? marker.manifest.manifestSha256 : null;
}

export async function clearInstall(): Promise<void> {
  const directory = await analyzerDirectory();
  await removeIfPresent(directory, MARKER_FILE);
  await Promise.all([
    removeIfPresent(directory, HOT_FILE),
    removeIfPresent(directory, DETAILS_FILE),
    removeIfPresent(directory, DOWNLOAD_FILE)
  ]);
}
