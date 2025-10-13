/**
 * JMDict XML loading orchestrator
 * Ported from ~/ichiran/dict-load.lisp lines 161-194
 */

import fs from 'fs';
import { createReadStream } from 'fs';
import { createGunzip } from 'zlib';
import type { Readable } from 'stream';
import { getConnection } from '@ichiran/core';
import { loadEntry, recalcEntryStatsAll } from './load-entry.js';
import { loadConjugations, loadSecondaryConjugations } from './conjugate.js';
import { loadCustomData } from './load-custom.js';
import { addErrata } from './errata.js';

// Maximum buffer size: largest JMDict entry is ~50KB, guard against malformed XML
const MAX_BUFFER_SIZE = 10 * 1024 * 1024; // 10MB

// Parallel batch size: tuned for balance between throughput and connection overhead
// Too high: connection pool exhaustion; Too low: underutilized CPU
const LOAD_PARALLELISM = 10;

// Progress reporting interval
const DEFAULT_PROGRESS_INTERVAL = 1000;

/**
 * Streaming XML parser that yields <entry> elements
 */
async function* streamJMDictEntries(path: string): AsyncGenerator<string, void, undefined> {
  let stream: Readable = createReadStream(path);

  // If file is gzipped, decompress it
  if (path.endsWith('.gz')) {
    stream = stream.pipe(createGunzip());
  }

  let buffer = '';
  let inEntry = false;

  for await (const chunk of stream) {
    buffer += chunk.toString('utf-8');
    
    // Guard against unbounded buffer growth (malformed XML with unclosed <entry>)
    if (buffer.length > MAX_BUFFER_SIZE) {
      throw new Error(
        `XML buffer exceeded ${MAX_BUFFER_SIZE} bytes - likely malformed entry ` +
        `(buffer starts with: ${buffer.substring(0, 200).replace(/\n/g, ' ')}...)`
      );
    }

    // Process complete entries
    while (true) {
      if (!inEntry) {
        // Look for entry start tag
        const startMatch = buffer.indexOf('<entry>');
        if (startMatch === -1) break;

        inEntry = true;
        buffer = buffer.slice(startMatch);
      }

      // Find the closing tag
      // Note: We don't need to handle nested <entry> tags because JMDict doesn't have them
      const nextClose = buffer.indexOf('</entry>');

      if (nextClose === -1) {
        // No closing tag yet, need more data
        break;
      }

      // Found the closing tag - yield the complete entry
      const endPos = nextClose + 8; // length of '</entry>'
      const entryXml = buffer.slice(0, endPos);
      yield entryXml;

      buffer = buffer.slice(endPos);
      inEntry = false;

      if (!inEntry) continue;
      break;
    }
  }
}

export interface LoadJMDictOptions {
  /** Path to JMDict XML file (can be .xml or .gz) */
  path: string;
  /** Maximum number of entries to load (for testing, default: unlimited) */
  maxEntries?: number;
  /** Progress reporting interval (default: 1000) */
  progressInterval?: number;
}

/**
 * Loads JMDict XML file into database (main entries only)
 * Ported from dict-load.lisp:170-183 load-jmdict
 *
 * Note: This only loads the base dictionary entries. Use separate commands for:
 * - Table initialization: init-db
 * - Errata corrections: load-errata
 * - Conjugations: load-conjugations, load-secondary-conjugations
 * - Additional data: load-custom, load-kanjidic
 *
 * @param options - Loading options
 */
export async function loadJMDict(options: LoadJMDictOptions): Promise<void> {
  const {
    path,
    maxEntries = Infinity,
    progressInterval = DEFAULT_PROGRESS_INTERVAL
  } = options;

  const sql = getConnection();

  // Check that file exists
  if (!fs.existsSync(path)) {
    throw new Error(`JMDict file not found: ${path}`);
  }

  console.log(`Loading JMDict from ${path}...`);
  const startTime = Date.now();

  let count = 0;
  let errors = 0;

  // Parallel loading configuration
  let batch: string[] = [];

  try {
    for await (const entryXml of streamJMDictEntries(path)) {
      if (count >= maxEntries) break;

      batch.push(entryXml);

      // Process batch when it reaches the parallelism limit
      if (batch.length >= LOAD_PARALLELISM) {
        const results = await Promise.allSettled(
          batch.map(xml => loadEntry(xml))
        );

        // Count successes and failures
        for (const result of results) {
          if (result.status === 'fulfilled') {
            count++;
          } else {
            errors++;
            console.error(`Error loading entry:`, result.reason);
            if (errors > 100) {
              throw new Error('Too many errors, aborting');
            }
          }
        }

        if (count % progressInterval === 0) {
          const elapsed = (Date.now() - startTime) / 1000;
          const rate = count / elapsed;
          console.log(`${count} entries loaded (${rate.toFixed(1)} entries/sec)`);
        }

        batch = [];
      }
    }

    // Process remaining entries in batch
    if (batch.length > 0) {
      const results = await Promise.allSettled(
        batch.map(xml => loadEntry(xml))
      );

      for (const result of results) {
        if (result.status === 'fulfilled') {
          count++;
        } else {
          errors++;
          console.error(`Error loading entry:`, result.reason);
        }
      }
    }

    console.log('Recalculating entry statistics...');
    await recalcEntryStatsAll(sql);

    console.log('Analyzing database...');
    await sql`ANALYZE`;

    const elapsed = (Date.now() - startTime) / 1000;
    console.log(`✓ ${count} entries loaded in ${elapsed.toFixed(1)}s (${(count / elapsed).toFixed(1)} entries/sec)`);

    if (errors > 0) {
      console.warn(`⚠ ${errors} entries failed to load`);
    }
  } catch (error) {
    console.error('Fatal error during loading:', error);
    throw error;
  }
}

/**
 * Loads additional data after main JMDict loading
 * Ported from dict-load.lisp:185-194 load-extras
 *
 * This includes:
 * 1. Primary conjugations
 * 2. Secondary conjugations (causative-te, etc.)
 * 3. Custom data (municipalities, wards, extra entries)
 * 4. Errata corrections
 * 5. Best reading links
 * 6. Final statistics recalculation
 */
export async function loadExtras(): Promise<void> {
  const sql = getConnection();

  console.log('Loading conjugations...');
  await loadConjugations();

  console.log('Loading secondary conjugations...');
  await loadSecondaryConjugations();

  // Load custom data and errata in parallel (they're independent)
  console.log('Loading custom data and applying errata in parallel...');
  await Promise.all([
    loadCustomData({ types: ['extra'], silent: false }),
    addErrata()
  ]);

  console.log('Recalculating entry statistics...');
  await recalcEntryStatsAll(sql);

  console.log('Analyzing database...');
  await sql`ANALYZE`;

  console.log('✓ All extras loaded successfully');
}

/**
 * Gets database statistics
 */
export async function getDbStats(): Promise<{
  totalEntries: number;
  rootEntries: number;
  conjugatedEntries: number;
  kanjiTexts: number;
  kanaTexts: number;
  senses: number;
  glosses: number;
  conjugations: number;
}> {
  const sql = getConnection();

  const [stats] = await sql`
    SELECT
      (SELECT COUNT(*) FROM entry) as total_entries,
      (SELECT COUNT(*) FROM entry WHERE root_p = true) as root_entries,
      (SELECT COUNT(*) FROM entry WHERE root_p = false) as conjugated_entries,
      (SELECT COUNT(*) FROM kanji_text) as kanji_texts,
      (SELECT COUNT(*) FROM kana_text) as kana_texts,
      (SELECT COUNT(*) FROM sense) as senses,
      (SELECT COUNT(*) FROM gloss) as glosses,
      (SELECT COUNT(*) FROM conjugation) as conjugations
  `;

  return {
    totalEntries: Number(stats.totalEntries),
    rootEntries: Number(stats.rootEntries),
    conjugatedEntries: Number(stats.conjugatedEntries),
    kanjiTexts: Number(stats.kanjiTexts),
    kanaTexts: Number(stats.kanaTexts),
    senses: Number(stats.senses),
    glosses: Number(stats.glosses),
    conjugations: Number(stats.conjugations)
  };
}
