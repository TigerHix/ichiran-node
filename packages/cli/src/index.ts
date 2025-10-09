#!/usr/bin/env node

/**
 * CLI interface for ichiran-node
 * Port of cli.lisp (lines 1-117)
 *
 * Lisp source: cli.lisp
 */

import { Command } from 'commander';
import { romanize, romanizeStar, setConnection, getConnection, type ConnectionSpec, transformRomanizeStarResult, printPerfCountersAndReset } from '@ichiran/core';
import { config } from 'dotenv';

// Parse environment variables
config();

// Helper to parse connection from env (moved from core)
function getConnectionFromEnv(): ConnectionSpec | null {
  const dbUrl = process.env.ICHIRAN_TEST_DB_URL || process.env.ICHIRAN_DB_URL;
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

/**
 * Programmatic interface for CLI operations
 * Returns the output string that would be printed to stdout
 */
export async function runCli(
  input: string,
  options: {
    withInfo?: boolean;
    full?: boolean;
    limit?: number;
    normalizePunctuation?: boolean;
  } = {}
): Promise<string> {
  let output = '';
  const normalizePunctuation = options.normalizePunctuation ?? true;
  
  try {
    if (options.withInfo) {
      const { romanized, info } = await romanize(input, { withInfo: true, normalizePunctuation });
      output = romanized;
      if (info) {
        for (const [word, gloss] of info) {
          output += `\n\n* ${word}  ${gloss}`;
        }
      }
    } else if (options.full) {
      const limitValue = options.limit ?? 1;
      const result = await romanizeStar(input, { limit: limitValue, normalizePunctuation });
      const transformed = await transformRomanizeStarResult(result);
      output = JSON.stringify(transformed);
    } else {
      const { romanized } = await romanize(input, { withInfo: true, normalizePunctuation });
      output = romanized;
    }
    
    output += '\n';
    return output.trim();
  } catch (error) {
    throw error;
  }
}

// Line 48-92: Main CLI handler
/**
 * Lisp source:
 * (defun main ()
 *   (load-connection-from-env)
 *   (multiple-value-bind (options free-args)
 *       (handler-case
 *         (handler-bind ((opts:unknown-option #'unknown-option))
 *           (opts:get-opts))
 *         ...)))
 */
async function main(): Promise<void> {
  const program = new Command();

  program
    .name('ichiran-cli')
    .description('Command line interface for Ichiran')
    .usage('[options] [input]')
    .version('0.1.0')
    .option('-e, --eval <expression>', 'evaluate arbitrary expression and print the result')
    .option('-i, --with-info', 'print dictionary info')
    .option('-f, --full', 'full split info (as JSON)')
    .option(
      '-l, --limit <number>',
      'limit segmentations to the specified number (useful only with -f or --full)',
      '1'
    )
    .helpOption('-h, --help', 'print this help text');

  program.parse(process.argv);
  const options = program.opts();
  const freeArgs = program.args;

  // Line 49: Load connection from environment
  const connSpec = getConnectionFromEnv();
  if (!connSpec) {
    console.error('ERROR: ICHIRAN_DB_URL environment variable not set');
    process.exit(2);
  }
  setConnection(connSpec);

  try {
    // Line 74-77: --eval option
    if (options.eval) {
      console.error('ERROR: --eval option not supported in TypeScript version');
      process.exit(1);
    }
    // Use the programmatic interface
    const input = freeArgs.join(' ');
    const output = await runCli(input, {
      withInfo: options.withInfo,
      full: options.full,
      limit: options.full ? parseInt(options.limit, 10) : undefined
    });
    process.stdout.write(output);
    process.stdout.write('\n');

    // Print performance counters if profiling is enabled
    printPerfCountersAndReset();
  } catch (error) {
    // Line 95-100: Error handler
    console.error(`ERROR: ${error}`);
    if (error instanceof Error && error.stack) {
      console.error(error.stack);
    }
    process.exit(2);
  } finally {
    // Close database connection to allow process to exit
    await getConnection().end();
  }
}

// Line 102-116: Build function (for cache initialization)
/**
 * Initialize all caches for production use
 *
 * Lisp source:
 * (defun build (&key conn debug)
 *   (when conn
 *     (switch-conn-vars conn))
 *   (format t "Initializing caches~%")
 *   (init-all-caches)
 *   (init-suffixes t)
 *   (postmodern:clear-connection-pool)
 *   (unless debug
 *     (setup-debugger))
 *   (asdf:make :ichiran/cli))
 */
export async function initCliCaches(connSpec?: ConnectionSpec): Promise<void> {
  if (connSpec) {
    setConnection(connSpec);
  }

  console.log('Initializing caches');

  // Note: In the TypeScript version, caches are lazy-initialized on first use
  // via defineCache pattern in conn.ts. We could force initialization here
  // by calling the relevant functions, but it's not strictly necessary.

  console.log('Cache initialization complete (lazy loading enabled)');
}

// Run main if this is the entry point
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((error) => {
    console.error(`FATAL: ${error}`);
    process.exit(2);
  });
}