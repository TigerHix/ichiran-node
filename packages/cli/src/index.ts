#!/usr/bin/env node

import { Command, InvalidArgumentError } from 'commander';
import { config } from 'dotenv';
import { openAnalyzer } from '@ichiran/node';
import type { RomanizationScheme } from '@ichiran/core';

config({ quiet: true });

type Analyzer = Awaited<ReturnType<typeof openAnalyzer>>;
let analyzer: Analyzer | null = null;

async function getAnalyzer(): Promise<Analyzer> {
  analyzer ??= await openAnalyzer();
  return analyzer;
}

function integer(value: string): number {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new InvalidArgumentError('must be a non-negative integer');
  }
  return parsed;
}

function text(parts: readonly string[]): string {
  return parts.join(' ');
}

async function main(): Promise<void> {
  const program = new Command();
  program
    .name('ichiran')
    .description('Self-contained Japanese morphological analyzer')
    .version('0.1.0');

  program
    .command('analyze')
    .description('analyze Japanese text')
    .argument('<text...>', 'text to analyze')
    .option('-l, --limit <number>', 'maximum analysis paths', integer)
    .action(async (parts: string[], options: { readonly limit?: number }) => {
      const result = await (await getAnalyzer()).analyze(text(parts), { limit: options.limit });
      process.stdout.write(`${JSON.stringify(result)}\n`);
    });

  program
    .command('romanize')
    .description('romanize Japanese text')
    .argument('<text...>', 'text to romanize')
    .option('-m, --method <name>', 'romanization scheme')
    .option('--normalize-punctuation', 'normalize Japanese punctuation')
    .action(async (
      parts: string[],
      options: { readonly method?: string; readonly normalizePunctuation?: boolean }
    ) => {
      const romanized = await (await getAnalyzer()).romanize(text(parts), {
        method: options.method as RomanizationScheme | undefined,
        normalizePunctuation: options.normalizePunctuation
      });
      process.stdout.write(`${romanized}\n`);
    });

  program
    .command('entry')
    .description('read one dictionary entry')
    .argument('<entry-index>', 'entry index returned by analyze', integer)
    .action(async (entryIndex: number) => {
      const entry = await (await getAnalyzer()).entry(entryIndex);
      process.stdout.write(`${JSON.stringify(entry)}\n`);
    });

  try {
    await program.parseAsync(process.argv);
  } catch (error) {
    const value = error as Error & { readonly code?: unknown };
    const code = typeof value.code === 'string' ? ` [${value.code}]` : '';
    process.stderr.write(`ERROR${code}: ${value.message ?? String(error)}\n`);
    process.exitCode = 2;
  } finally {
    analyzer?.dispose();
  }
}

void main();
