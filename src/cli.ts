#!/usr/bin/env node

/**
 * CLI interface for ichiran-node
 * Port of cli.lisp (lines 1-117)
 *
 * Lisp source: cli.lisp
 */

import { Command } from 'commander';
import { romanize, romanizeStar } from './romanize.js';
import { setConnection, getConnectionFromEnv, getConnection, type ConnectionSpec } from './conn.js';
import type { WordInfo } from './dict.js';
import { wordInfoGlossJson, printPerfCounters } from './dict.js';

// Line 44-46: Print romanization info helper
/**
 * Lisp source:
 * (defun print-romanize-info (info)
 *   (loop for (word . gloss) in info
 *         do (format t "~%~%* ~a  ~a" word gloss)))
 */
function printRomanizeInfo(info: Array<[string, string]>): void {
  for (const [word, gloss] of info) {
    console.log(`\n\n* ${word}  ${gloss}`);
  }
}

/**
 * Transform romanizeStar result to user-friendly JSON format
 * Converts WordInfo objects to WordInfoGlossJson with gloss and conj fields
 */
async function transformRomanizeStarResult(
  result: Array<string | Array<[Array<[string, WordInfo, any]>, number]>>
): Promise<any> {
  return Promise.all(
    result.map(async (segment) => {
      if (typeof segment === 'string') {
        // Non-word segment - keep as-is
        return segment;
      } else {
        // Word segment - transform each alternative
        return Promise.all(
          segment.map(async ([wordList, score]) => {
            const transformedWords = await Promise.all(
              wordList.map(async ([romanized, wordInfo, prop]) => {
                const glossJson = await wordInfoGlossJson(wordInfo);
                return [romanized, glossJson, prop];
              })
            );
            return [transformedWords, score];
          })
        );
      }
    })
  );
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
  } = {}
): Promise<string> {
  let output = '';
  
  try {
    if (options.withInfo) {
      const { romanized, info } = await romanize(input, { withInfo: true });
      output = romanized;
      if (info) {
        for (const [word, gloss] of info) {
          output += `\n\n* ${word}  ${gloss}`;
        }
      }
    } else if (options.full) {
      const limitValue = options.limit ?? 1;
      const result = await romanizeStar(input, { limit: limitValue });
      const transformed = await transformRomanizeStarResult(result);
      output = JSON.stringify(transformed);
    } else {
      const { romanized } = await romanize(input, { withInfo: true });
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
    printPerfCounters();
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