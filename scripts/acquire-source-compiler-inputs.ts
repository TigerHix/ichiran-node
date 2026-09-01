import { execFile as execFileCallback, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  mkdtemp,
  mkdir,
  readFile,
  readdir,
  rm,
  stat,
  writeFile
} from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { basename, dirname, join, relative } from 'node:path';
import { promisify } from 'node:util';
import { brotliDecompressSync, gunzipSync } from 'node:zlib';

const execFile = promisify(execFileCallback);
const ARCHIVE_REPOSITORY = 'https://github.com/Jitendex/edrdg-dictionary-archive.git';
const ARCHIVE_COMMIT = '2bdfbdcadaf38a7da3000f68f93ce711c7d5a878';
const TARGET_DATE = '2026-01-01';
const KANJIDIC_URL = 'https://web.archive.org/web/20150317225430id_/http://edrdg.org/kanjidic/kanjidic2.xml.gz';

interface LockedFile {
  readonly archiveDirectory: string;
  readonly fileName: string;
  readonly outputName: string;
  readonly bytes: number;
  readonly sha256: string;
  readonly gzipBytes: number;
  readonly gzipSha256: string;
}

const LOCKED_FILES: readonly LockedFile[] = [
  {
    archiveDirectory: 'JMdict_e',
    fileName: 'JMdict_e',
    outputName: 'JMdict_e.gz',
    bytes: 61_494_891,
    sha256: 'a21b13e465060d1bedd497b5b5d4b603e8ab8130663afb3a5a5c60b4250ef2ca',
    gzipBytes: 10_260_701,
    gzipSha256: '92eb77d60e5b949585e41a777ff3857c412bc97ea75444d14497a5156b6264b7'
  }
];

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function filesBelow(directory: string): Promise<string[]> {
  const entries = await readdir(directory, { withFileTypes: true });
  const files: string[] = [];
  for (const entry of entries) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) files.push(...await filesBelow(path));
    else files.push(path);
  }
  return files;
}

async function reconstruct(archiveRoot: string, locked: LockedFile): Promise<Buffer> {
  const sourceDirectory = join(archiveRoot, locked.archiveDirectory);
  const patchRoot = join(sourceDirectory, 'patches');
  const temporary = join(archiveRoot, `.reconstruct-${locked.archiveDirectory}`);
  await writeFile(
    temporary,
    brotliDecompressSync(await readFile(join(sourceDirectory, `${locked.fileName}.br`)))
  );

  for (const patchPath of (await filesBelow(patchRoot)).sort()) {
    const patchDate = relative(patchRoot, patchPath)
      .replace(/\.patch\.br$/, '')
      .replaceAll('/', '-');
    if (patchDate > TARGET_DATE) break;

    const result = spawnSync('patch', ['--quiet', temporary], {
      input: brotliDecompressSync(await readFile(patchPath))
    });
    if (result.status !== 0) {
      throw new Error(`Failed to patch ${locked.fileName} to ${patchDate}: ${String(result.stderr)}`);
    }
  }

  const bytes = await readFile(temporary);
  await rm(temporary);
  return bytes;
}

function assertIdentity(label: string, bytes: Uint8Array, expectedBytes: number, expectedSha256: string): void {
  const actualSha256 = sha256(bytes);
  if (bytes.byteLength !== expectedBytes || actualSha256 !== expectedSha256) {
    throw new Error(
      `${label} identity mismatch: ${bytes.byteLength} bytes ${actualSha256}; ` +
      `expected ${expectedBytes} bytes ${expectedSha256}`
    );
  }
}

function deterministicGzip(bytes: Uint8Array): Buffer {
  const result = spawnSync('gzip', ['-n', '-9', '-c'], {
    input: bytes,
    maxBuffer: 20 * 1024 * 1024
  });
  if (result.status !== 0) {
    throw new Error(`gzip failed: ${String(result.stderr)}`);
  }
  return result.stdout;
}

const outputDirectory = process.argv[2] ?? 'work/acquired-source-compiler-inputs';
const temporaryBase = process.platform === 'linux' ? '/tmp' : tmpdir();
const temporaryRoot = await mkdtemp(join(temporaryBase, 'ichiran-source-inputs-'));

try {
  const archiveRoot = join(temporaryRoot, 'archive');
  await execFile('git', ['clone', '--filter=blob:none', '--no-checkout', ARCHIVE_REPOSITORY, archiveRoot]);
  await execFile('git', ['-C', archiveRoot, 'sparse-checkout', 'init', '--cone']);
  await execFile('git', [
    '-C', archiveRoot, 'sparse-checkout', 'set',
    ...LOCKED_FILES.map(value => value.archiveDirectory),
    'LICENSE', 'README.md'
  ]);
  await execFile('git', ['-C', archiveRoot, 'checkout', '--detach', ARCHIVE_COMMIT]);
  await mkdir(outputDirectory, { recursive: true });

  for (const locked of LOCKED_FILES) {
    const uncompressed = await reconstruct(archiveRoot, locked);
    assertIdentity(locked.fileName, uncompressed, locked.bytes, locked.sha256);
    const compressed = deterministicGzip(uncompressed);
    assertIdentity(locked.outputName, compressed, locked.gzipBytes, locked.gzipSha256);

    const output = join(outputDirectory, locked.outputName);
    await mkdir(dirname(output), { recursive: true });
    await writeFile(output, compressed);
    const outputStats = await stat(output);
    process.stdout.write(`${basename(output)} ${outputStats.size} ${sha256(compressed)}\n`);
  }

  const response = await fetch(KANJIDIC_URL);
  if (!response.ok) throw new Error(`Failed to download Kanjidic2 capture: HTTP ${response.status}`);
  const compressedKanjidic = Buffer.from(await response.arrayBuffer());
  assertIdentity(
    'kanjidic2.xml.gz',
    compressedKanjidic,
    1_372_016,
    '1861f294b187d491dd127a972d59dfe92117df536466562a0f2a44abf98a7d03'
  );
  assertIdentity(
    'kanjidic2.xml',
    gunzipSync(compressedKanjidic),
    14_652_660,
    'd16ceffeddd0089ae2b4833d937fa34a1216805422165701e0236b7da5afa68f'
  );
  const kanjidicOutput = join(outputDirectory, 'kanjidic2.xml.gz');
  await writeFile(kanjidicOutput, compressedKanjidic);
  process.stdout.write(`kanjidic2.xml.gz ${compressedKanjidic.length} ${sha256(compressedKanjidic)}\n`);
} finally {
  await rm(temporaryRoot, { recursive: true, force: true });
}
