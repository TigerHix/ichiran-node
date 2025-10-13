#!/usr/bin/env node
/**
 * CLI for Ichiran data loading operations
 */

import { Command } from 'commander';
import { initTables } from './data/schema.js';
import { loadJMDict, getDbStats } from './data/load-jmdict.js';
import { loadConjugations, loadSecondaryConjugations } from './data/conjugate.js';
import { loadCustomData } from './data/load-custom.js';
import { addErrata } from './data/errata.js';
import { loadKanjidic, getKanjiStats } from './data/load-kanjidic.js';
import { calculateBestReadings, calculateKanjiStatistics, calculateReadingStatistics } from './data/maintenance.js';
import { getConnection, validateDatabaseSafety, setConnection, type ConnectionSpec } from '@ichiran/core';
import { config } from 'dotenv';

// Helper to parse connection from env (moved from core)
function getConnectionFromEnv(): ConnectionSpec | null {
  const dbUrl = process.env.ICHIRAN_DB_URL;
  if (!dbUrl) return null;

  try {
    const normalized = dbUrl.replace(/^postgresql:\/\//, 'postgres://');
    const url = new URL(normalized);

    const database = decodeURIComponent(url.pathname.replace(/^\//, ''));
    if (!database) {
      throw new Error('Database name missing');
    }

    const hostParam = url.searchParams.get('host');
    let host = url.hostname;
    if (!host && hostParam) {
      host = decodeURIComponent(hostParam);
    }
    if (!host) {
      host = 'localhost';
    }

    const portParam = url.port || url.searchParams.get('port') || undefined;
    const user = url.username ? decodeURIComponent(url.username) : '';
    const password = url.password ? decodeURIComponent(url.password) : '';

    const spec: ConnectionSpec = {
      user,
      password,
      host,
      database
    };

    if (portParam) {
      const parsedPort = Number(portParam);
      if (!Number.isFinite(parsedPort)) {
        throw new Error(`Invalid port: ${portParam}`);
      }
      spec.port = parsedPort;
    }

    const sslParam = url.searchParams.get('ssl');
    const sslMode = url.searchParams.get('sslmode');
    if (sslParam) {
      const normalizedSsl = sslParam.toLowerCase();
      if (['true', '1', 'require'].includes(normalizedSsl)) {
        spec.ssl = true;
      } else if (['false', '0', 'disable'].includes(normalizedSsl)) {
        spec.ssl = false;
      }
    } else if (sslMode) {
      const normalizedSslmode = sslMode.toLowerCase();
      if (['require', 'verify-ca', 'verify-full'].includes(normalizedSslmode)) {
        spec.ssl = true;
      } else if (normalizedSslmode === 'disable') {
        spec.ssl = false;
      }
    }

    return spec;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`Invalid database URL (${dbUrl}): ${message}`);
  }
}

// Parse environment variables
config();
import { downloadDataFile, ensureDataFile, dataFileExists, getDataPath } from './data/download.js';

const program = new Command();

program
  .name('ichiran-data')
  .description('Ichiran dictionary data loading and management')
  .version('0.1.0');

program
  .command('init-db')
  .description('Initialize database schema (drops and recreates all tables)')
  .action(async () => {
    try {
      // Setup connection from env
      const connSpec = getConnectionFromEnv();
      if (!connSpec) {
        console.error('ERROR: ICHIRAN_DB_URL environment variable not set');
        process.exit(2);
      }
      setConnection(connSpec);

      // Safety check: prevent accidental drops on production database
      validateDatabaseSafety('init-db (drops and recreates all tables)');

      console.log('Initializing database...');
      const sql = getConnection();
      await initTables(sql);
      console.log('✓ Database initialized successfully');
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('download')
  .description('Download required data files (JMDict, Kanjidic2)')
  .option('--jmdict', 'Download only JMDict file')
  .option('--kanjidic', 'Download only Kanjidic2 file')
  .option('--force', 'Force re-download even if files exist')
  .action(async (options) => {
    try {
      const downloadJmdict = options.jmdict || (!options.jmdict && !options.kanjidic);
      const downloadKanjidic = options.kanjidic || (!options.jmdict && !options.kanjidic);

      if (downloadJmdict) {
        await downloadDataFile('jmdict', { force: options.force });
      }
      if (downloadKanjidic) {
        await downloadDataFile('kanjidic', { force: options.force });
      }

      console.log('✓ Download complete');
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('load-jmdict')
  .description('Load JMDict XML file (main dictionary entries only)')
  .option('-p, --path <path>', 'Path to JMDict XML file (.xml or .gz) - auto-downloads if not specified')
  .option('--no-download', 'Disable auto-download if file is missing')
  .option('--max <number>', 'Maximum number of entries to load (for testing)', parseInt)
  .action(async (options) => {
    try {
      let filePath = options.path;

      // Auto-download if path not specified
      if (!filePath) {
        if (options.download === false) {
          filePath = getDataPath('jmdict');
          if (!dataFileExists('jmdict')) {
            throw new Error('JMDict file not found. Run "bun run data download" or specify --path');
          }
        } else {
          console.log('No path specified, checking for data file...');
          filePath = await ensureDataFile('jmdict');
        }
      }

      await loadJMDict({
        path: filePath,
        maxEntries: options.max
      });
      console.log('✓ JMDict loaded successfully');
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('load-conjugations')
  .description('Generate primary conjugations for all conjugatable entries')
  .option('--limit <number>', 'Limit number of entries to process (for testing)', parseInt)
  .action(async (options) => {
    try {
      const startTime = Date.now();
      await loadConjugations({ limit: options.limit });
      const elapsed = (Date.now() - startTime) / 1000;
      console.log(`✓ Conjugations loaded in ${elapsed.toFixed(1)}s`);
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('load-secondary-conjugations')
  .description('Generate secondary conjugations (causative-te, potential-past, etc.)')
  .option('--from <seqs...>', 'Limit to specific source entry sequences', (val) => val.split(',').map(Number))
  .action(async (options) => {
    try {
      const startTime = Date.now();
      await loadSecondaryConjugations({ from: options.from });
      const elapsed = (Date.now() - startTime) / 1000;
      console.log(`✓ Secondary conjugations loaded in ${elapsed.toFixed(1)}s`);
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('load-errata')
  .description('Apply dictionary errata corrections')
  .action(async () => {
    try {
      const startTime = Date.now();
      const { addErrata } = await import('./data/errata.js');
      await addErrata();
      const elapsed = (Date.now() - startTime) / 1000;
      console.log(`✓ Errata applied in ${elapsed.toFixed(1)}s`);
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('load-custom')
  .description('Load custom data (extra entries, municipalities, wards)')
  .option('--extra', 'Load extra.xml entries')
  .option('--municipality', 'Load municipality data from jichitai.csv')
  .option('--ward', 'Load ward data from gyoseiku.csv')
  .option('--data-path <path>', 'Path to data sources directory', './data/sources')
  .action(async (options) => {
    try {
      const startTime = Date.now();

      // Determine which types to load
      const types: Array<'extra' | 'municipality' | 'ward'> = [];
      if (options.extra) types.push('extra');
      if (options.municipality) types.push('municipality');
      if (options.ward) types.push('ward');

      // If no specific types selected, load all
      if (types.length === 0) {
        types.push('extra');
      }

      await loadCustomData({
        types,
        dataPath: options.dataPath,
        silent: false
      });

      const elapsed = (Date.now() - startTime) / 1000;
      console.log(`✓ Custom data loaded in ${elapsed.toFixed(1)}s`);
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('apply-errata')
  .description('Apply database corrections and errata fixes')
  .action(async () => {
    try {
      const startTime = Date.now();
      await addErrata();
      const elapsed = (Date.now() - startTime) / 1000;
      console.log(`✓ Errata applied in ${elapsed.toFixed(1)}s`);
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('load-kanjidic')
  .description('Load Kanjidic2 XML file (kanji character data)')
  .option('-p, --path <path>', 'Path to kanjidic2.xml file - auto-downloads if not specified')
  .option('--no-download', 'Disable auto-download if file is missing')
  .action(async (options) => {
    try {
      let filePath = options.path;

      // Auto-download if path not specified
      if (!filePath) {
        if (options.download === false) {
          filePath = getDataPath('kanjidic');
          if (!dataFileExists('kanjidic')) {
            throw new Error('Kanjidic2 file not found. Run "bun run data download" or specify --path');
          }
        } else {
          console.log('No path specified, checking for data file...');
          filePath = await ensureDataFile('kanjidic');
        }
      }

      const startTime = Date.now();
      await loadKanjidic(filePath);
      const elapsed = (Date.now() - startTime) / 1000;
      console.log(`✓ Kanjidic2 loaded in ${elapsed.toFixed(1)}s`);
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('calculate-best-readings')
  .alias('best-readings')
  .description('Calculate best_kana and best_kanji fields for dictionary entries')
  .option('--reset', 'Reset existing best_kana/best_kanji values before calculating')
  .action(async (options) => {
    try {
      const startTime = Date.now();
      await calculateBestReadings({ reset: options.reset });
      const elapsed = (Date.now() - startTime) / 1000;
      console.log(`✓ Best readings calculated in ${elapsed.toFixed(1)}s`);
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('calculate-kanji-stats')
  .description('Calculate kanji usage statistics (stat_common, stat_irregular)')
  .action(async () => {
    try {
      const startTime = Date.now();
      await calculateKanjiStatistics();
      const elapsed = (Date.now() - startTime) / 1000;
      console.log(`✓ Kanji statistics calculated in ${elapsed.toFixed(1)}s`);
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('calculate-reading-stats')
  .description('Calculate reading usage statistics for kanji readings')
  .action(async () => {
    try {
      const startTime = Date.now();
      await calculateReadingStatistics();
      const elapsed = (Date.now() - startTime) / 1000;
      console.log(`✓ Reading statistics calculated in ${elapsed.toFixed(1)}s`);
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program
  .command('stats')
  .description('Show database statistics')
  .action(async () => {
    try {
      const stats = await getDbStats();
      const kanjiStats = await getKanjiStats();

      console.log('\n=== Database Statistics ===');
      console.log(`Total entries:      ${stats.totalEntries.toLocaleString()}`);
      console.log(`  Root entries:     ${stats.rootEntries.toLocaleString()}`);
      console.log(`  Conjugated:       ${stats.conjugatedEntries.toLocaleString()}`);
      console.log(`Kanji texts:        ${stats.kanjiTexts.toLocaleString()}`);
      console.log(`Kana texts:         ${stats.kanaTexts.toLocaleString()}`);
      console.log(`Senses:             ${stats.senses.toLocaleString()}`);
      console.log(`Glosses:            ${stats.glosses.toLocaleString()}`);
      console.log(`Conjugations:       ${stats.conjugations.toLocaleString()}`);
      console.log('');
      console.log('=== Kanji Statistics ===');
      console.log(`Total kanji:        ${kanjiStats.totalKanji.toLocaleString()}`);
      console.log(`Readings:           ${kanjiStats.totalReadings.toLocaleString()}`);
      console.log(`Okurigana:          ${kanjiStats.totalOkurigana.toLocaleString()}`);
      console.log(`Meanings:           ${kanjiStats.totalMeanings.toLocaleString()}`);
      console.log('');
      process.exit(0);
    } catch (error) {
      console.error('Error:', error);
      process.exit(1);
    }
  });

program.parse();
