/**
 * Data file download utilities
 * Automatically downloads JMDict and Kanjidic2 files if not present
 */

import fs from 'fs';
import path from 'path';
import { createWriteStream } from 'fs';
import { pipeline } from 'stream/promises';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const DATA_SOURCES = {
  jmdict: {
    url: 'http://ftp.edrdg.org/pub/Nihongo/JMdict_e.gz',
    filename: 'JMdict_e.gz',
    description: 'JMDict Japanese-English dictionary (~10 MB)',
  },
  kanjidic: {
    url: 'https://www.edrdg.org/kanjidic/kanjidic2.xml.gz',
    filename: 'kanjidic2.xml.gz',
    description: 'Kanjidic2 kanji character database (~1.5 MB)',
  },
};

export type DataSource = keyof typeof DATA_SOURCES;

/**
 * Get the default data directory path
 * Uses import.meta.url to find package root, regardless of working directory
 */
export function getDataDir(customPath?: string): string {
  if (customPath) return customPath;
  if (process.env.ICHIRAN_DATA_DIR) return process.env.ICHIRAN_DATA_DIR;
  
  // Get the directory of this source file
  const __filename = fileURLToPath(import.meta.url);
  const __dirname = dirname(__filename);
  
  // Navigate from dist/data/ or src/data/ to package root, then to data/
  // Assumes structure: packages/data/[src|dist]/data/download.js
  return path.join(__dirname, '../../../data');
}

/**
 * Get the full path for a data file
 */
export function getDataPath(source: DataSource): string {
  const dataDir = getDataDir();
  return path.join(dataDir, DATA_SOURCES[source].filename);
}

/**
 * Check if a data file exists locally
 */
export function dataFileExists(source: DataSource): boolean {
  const filePath = getDataPath(source);
  return fs.existsSync(filePath);
}

/**
 * Download a data file from the internet
 */
export async function downloadDataFile(
  source: DataSource,
  options: {
    force?: boolean;
    silent?: boolean;
  } = {}
): Promise<string> {
  const { force = false, silent = false } = options;
  const sourceInfo = DATA_SOURCES[source];
  const filePath = getDataPath(source);

  // Check if file already exists
  if (!force && fs.existsSync(filePath)) {
    if (!silent) {
      console.log(`✓ ${sourceInfo.filename} already exists at ${filePath}`);
    }
    return filePath;
  }

  // Ensure data directory exists
  const dataDir = getDataDir();
  if (!fs.existsSync(dataDir)) {
    fs.mkdirSync(dataDir, { recursive: true });
  }

  if (!silent) {
    console.log(`Downloading ${sourceInfo.description}...`);
    console.log(`  URL: ${sourceInfo.url}`);
    console.log(`  Destination: ${filePath}`);
  }

  try {
    const response = await fetch(sourceInfo.url);

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    if (!response.body) {
      throw new Error('Response body is null');
    }

    // Convert Web ReadableStream to Node.js Readable
    const nodeStream = new ReadableStream({
      async start(controller) {
        const reader = response.body!.getReader();
        try {
          while (true) {
            const { done, value } = await reader.read();
            if (done) break;
            controller.enqueue(value);
          }
          controller.close();
        } catch (error) {
          controller.error(error);
        }
      },
    });

    // Save to file with progress reporting
    const fileStream = createWriteStream(filePath);
    let downloadedBytes = 0;
    const totalBytes = parseInt(response.headers.get('content-length') || '0', 10);

    const progressStream = new TransformStream({
      transform(chunk, controller) {
        downloadedBytes += chunk.length;
        if (!silent && totalBytes > 0) {
          const percent = ((downloadedBytes / totalBytes) * 100).toFixed(1);
          const mb = (downloadedBytes / 1024 / 1024).toFixed(1);
          const totalMb = (totalBytes / 1024 / 1024).toFixed(1);
          process.stdout.write(`\r  Progress: ${percent}% (${mb} / ${totalMb} MB)`);
        }
        controller.enqueue(chunk);
      },
    });

    await pipeline(
      nodeStream.pipeThrough(progressStream),
      async function* (source) {
        for await (const chunk of source as any) {
          yield chunk;
        }
      },
      fileStream
    );

    if (!silent && totalBytes > 0) {
      process.stdout.write('\n');
    }

    if (!silent) {
      const sizeMB = (fs.statSync(filePath).size / 1024 / 1024).toFixed(1);
      console.log(`✓ Downloaded ${sourceInfo.filename} (${sizeMB} MB)`);
    }

    return filePath;
  } catch (error) {
    // Clean up partial download
    if (fs.existsSync(filePath)) {
      fs.unlinkSync(filePath);
    }
    throw new Error(`Failed to download ${sourceInfo.filename}: ${error}`);
  }
}

/**
 * Ensure a data file is available (download if needed)
 */
export async function ensureDataFile(
  source: DataSource,
  options: {
    silent?: boolean;
  } = {}
): Promise<string> {
  if (dataFileExists(source)) {
    const filePath = getDataPath(source);
    if (!options.silent) {
      console.log(`✓ Using existing ${DATA_SOURCES[source].filename}`);
    }
    return filePath;
  }

  return downloadDataFile(source, options);
}

/**
 * Download all required data files
 */
export async function downloadAllDataFiles(
  options: {
    force?: boolean;
    silent?: boolean;
  } = {}
): Promise<{ jmdict: string; kanjidic: string }> {
  const jmdictPath = await downloadDataFile('jmdict', options);
  const kanjidicPath = await downloadDataFile('kanjidic', options);

  return {
    jmdict: jmdictPath,
    kanjidic: kanjidicPath,
  };
}
