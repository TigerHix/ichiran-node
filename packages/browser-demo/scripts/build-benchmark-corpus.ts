import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';

interface Request {
  readonly text: string;
  readonly limit: number;
  readonly entities?: readonly unknown[];
}

interface Corpus {
  readonly groups: Readonly<Record<string, readonly Request[]>>;
}

const packageRoot = resolve(import.meta.dir, '..');
const sourcePath = resolve(packageRoot, '..', '..', 'browser-alpha', 'bench', 'corpus.json');
const outputPath = resolve(packageRoot, 'src', 'generated', 'benchmark-corpus.json');
const source = JSON.parse(await readFile(sourcePath, 'utf8')) as Corpus;

// The app needs only executable benchmark inputs. Source paths, fixture indexes,
// and human titles remain in the authoritative corpus and its qualification logs.
const groups = Object.fromEntries(Object.entries(source.groups).map(([name, requests]) => [
  name,
  requests.map(request => {
    if (request.entities !== undefined) return [request.text, request.limit, request.entities];
    if (request.limit !== 1) return [request.text, request.limit];
    return [request.text];
  })
]));

await mkdir(dirname(outputPath), { recursive: true });
await writeFile(outputPath, `${JSON.stringify({ groups })}\n`);
