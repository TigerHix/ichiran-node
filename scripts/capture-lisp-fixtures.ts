#!/usr/bin/env bun

import { execFileSync } from 'node:child_process';
import { readFileSync, writeFileSync } from 'node:fs';
import { resolve } from 'node:path';

interface TestCases {
  readonly romanization: readonly string[];
  readonly info: readonly string[];
  readonly fullJson: readonly { readonly text: string; readonly limit: number }[];
}

interface ExpectedOutputs {
  readonly romanization: Record<string, string>;
  readonly info: Record<string, string>;
  readonly fullJson: Record<string, string>;
}

const cli = process.env.ICHIRAN_LISP_CLI;
if (!cli) throw new Error('ICHIRAN_LISP_CLI must name the pinned upstream ichiran-cli binary');

function run(input: string, arguments_: readonly string[] = []): string {
  return execFileSync(resolve(cli), [...arguments_, input], {
    encoding: 'utf8',
    maxBuffer: 16 * 1024 * 1024
  }).trim();
}

function capture(casesPath: string, outputPath: string): void {
  const cases = JSON.parse(readFileSync(casesPath, 'utf8')) as TestCases;
  const output: ExpectedOutputs = { romanization: {}, info: {}, fullJson: {} };
  for (const input of cases.romanization) output.romanization[input] = run(input);
  for (const input of cases.info) output.info[input] = run(input, ['-i']);
  for (const { text, limit } of cases.fullJson) {
    output.fullJson[`${text}|${limit}`] = run(text, ['-f', '-l', String(limit)]);
  }
  writeFileSync(outputPath, `${JSON.stringify(output, null, 2)}\n`);
  console.log(`Captured ${cases.fullJson.length + cases.info.length + cases.romanization.length} cases: ${outputPath}`);
}

const data = resolve('packages/cli/tests/data');
capture(resolve(data, 'cli.json'), resolve(data, 'cli-lisp-outputs.json'));
capture(resolve(data, 'hard-cli.json'), resolve(data, 'hard-cli-lisp-outputs.json'));
