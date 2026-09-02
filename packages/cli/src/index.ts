#!/usr/bin/env node

import { Command } from 'commander';
import { config } from 'dotenv';
import { openNodeRuntime, romanizeWithInfo } from '@ichiran/node';

config();

type Runtime = Awaited<ReturnType<typeof openNodeRuntime>>;
let runtimePromise: Promise<Runtime> | null = null;

function defaultRuntime(): Promise<Runtime> {
  runtimePromise ??= openNodeRuntime();
  return runtimePromise;
}

export interface CliOptions {
  readonly withInfo?: boolean;
  readonly full?: boolean;
  readonly limit?: number;
  readonly normalizePunctuation?: boolean;
  /** Explicit runtime for embedders and tests; normal callers use ICHIRAN_PACK_DIR. */
  readonly runtime?: Runtime;
}

/** Programmatic CLI operation with Lisp-compatible text/JSON presentation. */
export async function runCli(input: string, options: CliOptions = {}): Promise<string> {
  const runtime = options.runtime ?? await defaultRuntime();
  const normalizePunctuation = options.normalizePunctuation ?? true;
  let output: string;

  if (options.withInfo) {
    const result = await romanizeWithInfo(runtime, input, normalizePunctuation);
    output = result.romanized;
    for (const [romanized, definition] of result.info) {
      output += `\n\n* ${romanized}  ${definition}`;
    }
  } else if (options.full) {
    output = JSON.stringify(await runtime.legacy(input, {
      limit: options.limit ?? 1,
      normalizePunctuation
    }));
  } else {
    output = await runtime.romanize(input, { normalizePunctuation });
  }

  return output.trim();
}

export async function initCliCaches(): Promise<void> {
  await defaultRuntime();
}

/** Release the process-wide runtime used by the executable. */
async function disposeCliCaches(): Promise<void> {
  const current = runtimePromise;
  runtimePromise = null;
  if (!current) return;
  let runtime: Runtime;
  try {
    runtime = await current;
  } catch {
    // A failed open has no runtime or temporary detail store to release.
    return;
  }
  runtime.dispose();
}

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
  if (options.eval) {
    console.error('ERROR: --eval option not supported in TypeScript version');
    process.exit(1);
  }

  try {
    const output = await runCli(program.args.join(' '), {
      withInfo: options.withInfo,
      full: options.full,
      limit: options.full ? Number.parseInt(options.limit, 10) : undefined
    });
    process.stdout.write(`${output}\n`);
  } catch (error) {
    console.error(`ERROR: ${error instanceof Error ? error.message : String(error)}`);
    process.exitCode = 2;
  } finally {
    await disposeCliCaches();
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  void main();
}
