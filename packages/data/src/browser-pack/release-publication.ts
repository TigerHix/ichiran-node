import { createHash, randomUUID } from 'node:crypto';
import { constants } from 'node:fs';
import {
  lstat,
  mkdir,
  open,
  readdir,
  realpath,
  rename,
  rm,
  symlink,
  writeFile
} from 'node:fs/promises';
import { basename, dirname, join, relative } from 'node:path';

export type AnalyzerReleaseFiles = ReadonlyMap<string, Uint8Array>;

export interface PublishAnalyzerReleaseOptions {
  readonly verify: (directory: string) => Promise<void>;
  /** Test-only fault point: an old active generation must survive this throw. */
  readonly beforeActivate?: (generationDirectory: string) => void | Promise<void>;
}

export function analyzerReleaseGenerationIdentity(files: AnalyzerReleaseFiles): string {
  const digest = createHash('sha256');
  for (const [name, bytes] of [...files].sort(([left], [right]) => left.localeCompare(right))) {
    digest.update(name).update('\0').update(String(bytes.byteLength)).update('\0').update(bytes);
  }
  return digest.digest('hex');
}

export async function assertActiveReleaseGeneration(
  output: string,
  expectedNames: readonly string[]
): Promise<void> {
  const info = await lstat(output);
  if (!info.isSymbolicLink()) throw new Error(`${output} is not an atomic release symlink`);
  const generation = await realpath(output);
  await assertExactReleaseInventory(generation, expectedNames);
  const files = new Map<string, Uint8Array>();
  for (const name of expectedNames) {
    files.set(name, new Uint8Array(await readRegularArtifact(generation, name)));
  }
  const expected = analyzerReleaseGenerationIdentity(files);
  if (basename(generation) !== expected) {
    throw new Error(`Active release generation ${basename(generation)} does not match bytes ${expected}`);
  }
}

export async function assertExactReleaseInventory(
  directory: string,
  expectedNames: readonly string[]
): Promise<void> {
  const actual = (await readdir(directory)).sort();
  const expected = [...expectedNames].sort();
  if (actual.join('\n') !== expected.join('\n')) {
    throw new Error(
      `Release inventory mismatch: found [${actual.join(', ')}], expected [${expected.join(', ')}]`
    );
  }
}

async function assertGenerationBytes(
  directory: string,
  files: AnalyzerReleaseFiles
): Promise<void> {
  await assertExactReleaseInventory(directory, [...files.keys()]);
  for (const [name, expected] of files) {
    const actual = await readRegularArtifact(directory, name);
    if (!Buffer.from(actual).equals(Buffer.from(expected))) {
      throw new Error(`Existing release generation differs at ${name}`);
    }
  }
}

async function readRegularArtifact(directory: string, name: string): Promise<Buffer> {
  const path = join(directory, name);
  let handle;
  try {
    handle = await open(path, constants.O_RDONLY | constants.O_NOFOLLOW);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ELOOP') {
      throw new Error(`Release artifact is not a regular file: ${path}`);
    }
    throw error;
  }
  try {
    if (!(await handle.stat()).isFile()) {
      throw new Error(`Release artifact is not a regular file: ${path}`);
    }
    return await handle.readFile();
  } finally {
    await handle.close();
  }
}

async function assertPublishableOutput(output: string): Promise<void> {
  try {
    const current = await lstat(output);
    if (!current.isSymbolicLink()) {
      throw new Error(`${output} must be absent or an atomic release symlink`);
    }
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error;
  }
}

async function prepareGenerationsRoot(output: string): Promise<string> {
  const generations = `${output}.generations`;
  await mkdir(generations, { recursive: true });
  const info = await lstat(generations);
  if (info.isSymbolicLink() || !info.isDirectory()) {
    throw new Error(`${generations} must be a real directory, never a symlink`);
  }
  const [physicalParent, physicalGenerations] = await Promise.all([
    realpath(dirname(output)),
    realpath(generations)
  ]);
  if (physicalGenerations !== join(physicalParent, basename(generations))) {
    throw new Error(`${generations} escapes its physical destination directory`);
  }
  return generations;
}

/**
 * Publish immutable bytes, then atomically switch one symlink to the complete
 * generation. No reader can observe a mixture of old and new files.
 */
export async function publishAnalyzerRelease(
  output: string,
  files: AnalyzerReleaseFiles,
  options: PublishAnalyzerReleaseOptions
): Promise<string> {
  if (files.size === 0) throw new Error('Release generation must contain files');
  for (const name of files.keys()) {
    if (name !== basename(name) || name === '.' || name === '..') {
      throw new Error(`Release filename must be a plain basename: ${name}`);
    }
  }
  // Reject historical flat outputs before creating a staging directory or
  // writing any bytes. Moving old data aside is an explicit operator action.
  await assertPublishableOutput(output);
  const parent = dirname(output);
  const generations = await prepareGenerationsRoot(output);
  const identity = analyzerReleaseGenerationIdentity(files);
  const generation = join(generations, identity);
  const stage = join(generations, `.stage-${randomUUID()}`);
  let staged = false;
  try {
    try {
      const info = await lstat(generation);
      if (!info.isDirectory()) throw new Error(`Release generation is not a directory: ${generation}`);
      await assertGenerationBytes(generation, files);
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error;
      await mkdir(stage);
      staged = true;
      for (const [name, bytes] of files) {
        await writeFile(join(stage, name), bytes, { flag: 'wx' });
      }
      await assertExactReleaseInventory(stage, [...files.keys()]);
      await options.verify(stage);
      try {
        await rename(stage, generation);
        staged = false;
      } catch (error) {
        const code = (error as NodeJS.ErrnoException).code;
        if (code !== 'EEXIST' && code !== 'ENOTEMPTY') throw error;
        await assertGenerationBytes(generation, files);
      }
    }

    await options.verify(generation);
    await options.beforeActivate?.(generation);

    const link = join(parent, `.${basename(output)}-activate-${randomUUID()}`);
    try {
      await symlink(relative(parent, generation), link, 'dir');
      await rename(link, output);
    } finally {
      await rm(link, { force: true });
    }
    return generation;
  } finally {
    if (staged) await rm(stage, { recursive: true, force: true });
  }
}
