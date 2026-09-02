import { copyFile, mkdir } from 'node:fs/promises';
import { join, resolve } from 'node:path';

const packageRoot = resolve(import.meta.dir, '..');
const source = join(packageRoot, 'src', 'rust-kernel', 'generated');
const target = join(packageRoot, 'dist', 'rust-kernel', 'generated');

await mkdir(target, { recursive: true });
await Promise.all([
  'ichiran_kernel.js',
  'ichiran_kernel_bg.wasm'
].map(file => copyFile(join(source, file), join(target, file))));
