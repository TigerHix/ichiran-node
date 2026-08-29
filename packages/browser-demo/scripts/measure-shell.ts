import { readdir, stat } from 'node:fs/promises';
import { join, relative, resolve, sep } from 'node:path';

const output = resolve(import.meta.dir, '..', 'dist');

async function files(directory: string): Promise<string[]> {
  const result: string[] = [];
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) result.push(...await files(path));
    else result.push(path);
  }
  return result;
}

let bytes = 0;
for (const path of await files(output)) {
  const name = relative(output, path).split(sep).join('/');
  if (!name.startsWith('analyzer/')) bytes += (await stat(path)).size;
}
console.log(bytes);
