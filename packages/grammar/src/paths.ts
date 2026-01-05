import { existsSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

export function findPackageRoot(fromUrl: string): string {
  let dir = dirname(fileURLToPath(fromUrl));
  // Allow both src/ and dist/ entrypoints.
  for (let i = 0; i < 8; i++) {
    const candidate = join(dir, 'package.json');
    if (existsSync(candidate)) return dir;
    dir = dirname(dir);
  }
  // Fallback: best effort.
  return dirname(fileURLToPath(fromUrl));
}
