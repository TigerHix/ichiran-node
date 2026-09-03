import { lstat, realpath, stat } from 'node:fs/promises';
import { basename, dirname, isAbsolute, join, parse, relative, resolve, sep } from 'node:path';

function isBelow(parent: string, child: string): boolean {
  const path = relative(parent, child);
  return path !== '' && path !== '..' && !path.startsWith(`..${sep}`) && !isAbsolute(path);
}

async function physicalPath(path: string): Promise<string> {
  const missing: string[] = [];
  let ancestor = path;
  for (;;) {
    try {
      return join(await realpath(ancestor), ...missing.reverse());
    } catch (error) {
      const code = (error as NodeJS.ErrnoException).code;
      if (code === 'ENOTDIR') throw new Error('Release output parent must be a directory');
      if (code !== 'ENOENT') throw error;
      const parent = dirname(ancestor);
      if (parent === ancestor) throw error;
      missing.push(basename(ancestor));
      ancestor = parent;
    }
  }
}

async function assertOutputDirectory(path: string): Promise<void> {
  let link;
  try {
    link = await lstat(path);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') return;
    throw error;
  }
  if (!link.isSymbolicLink()) {
    throw new Error('Release output must be absent or an atomic release symlink');
  }
  try {
    if ((await stat(path)).isDirectory()) return;
  } catch {
    throw new Error('Existing release output symlink must resolve to a directory');
  }
  throw new Error('Existing release output symlink must resolve to a directory');
}

async function assertGenerationsDirectory(path: string): Promise<void> {
  const generations = `${path}.generations`;
  let info;
  try {
    info = await lstat(generations);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') return;
    throw error;
  }
  if (info.isSymbolicLink() || !info.isDirectory()) {
    throw new Error('Release generations root must be a real directory, never a symlink');
  }
  const [physicalParent, physicalGenerations] = await Promise.all([
    realpath(dirname(path)),
    realpath(generations)
  ]);
  if (physicalGenerations !== join(physicalParent, basename(generations))) {
    throw new Error('Release generations root escapes its physical destination directory');
  }
}

export interface SourceReleaseOutputResolution {
  readonly lexical: string;
  readonly physical: string;
}

export async function resolveSourceReleaseDestination(
  repository: string,
  value: string
): Promise<SourceReleaseOutputResolution> {
  if (value.includes('\\')) throw new Error('Release output must use portable forward slashes');
  const lexicalRepository = resolve(repository);
  const lexicalWork = join(lexicalRepository, 'work');
  const path = resolve(lexicalRepository, value);
  if (path === lexicalRepository || path === parse(path).root) {
    throw new Error('Release output must not be the source or filesystem root');
  }
  if (isBelow(lexicalRepository, path) && !isBelow(lexicalWork, path)) {
    throw new Error('In-repository release output must be below work/');
  }
  await Promise.all([assertOutputDirectory(path), assertGenerationsDirectory(path)]);
  const [physicalRepository, physicalOutput] = await Promise.all([
    realpath(lexicalRepository),
    physicalPath(path)
  ]);
  const physicalWork = join(physicalRepository, 'work');
  if (physicalOutput === physicalRepository || physicalOutput === parse(physicalOutput).root) {
    throw new Error('Release output must not resolve to the source or filesystem root');
  }
  if (
    isBelow(physicalRepository, physicalOutput)
    && !isBelow(physicalWork, physicalOutput)
  ) {
    throw new Error('In-repository release output must be below work/');
  }
  if (isBelow(lexicalWork, path) && !isBelow(physicalWork, physicalOutput)) {
    throw new Error('A work/ release output must resolve below the physical work directory');
  }
  return { lexical: path, physical: physicalOutput };
}

export async function assertSourceReleaseDestination(
  repository: string,
  expected: SourceReleaseOutputResolution
): Promise<void> {
  const current = await resolveSourceReleaseDestination(repository, expected.lexical);
  if (current.lexical !== expected.lexical || current.physical !== expected.physical) {
    throw new Error('Source release physical output changed before publication');
  }
}

export async function resolveSourceReleaseOutput(repository: string, value: string): Promise<string> {
  return (await resolveSourceReleaseDestination(repository, value)).lexical;
}
