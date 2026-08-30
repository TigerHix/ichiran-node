#!/usr/bin/env bun

import { spawnSync } from 'node:child_process';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const repository = resolve(dirname(fileURLToPath(import.meta.url)), '..');

if (!process.env.ICHIRAN_PACK_DIR) {
  console.error('usage: ICHIRAN_PACK_DIR=/path/to/release bun scripts/packed-parity.ts');
  process.exit(2);
}

function run(arguments_: readonly string[], environment = process.env): void {
  const result = spawnSync(process.execPath, arguments_, {
    cwd: repository,
    env: environment,
    stdio: 'inherit'
  });
  if (result.error) throw result.error;
  if (result.status !== 0) process.exit(result.status ?? 1);
}

// Package exports point at dist. These two product-only builds make the gate
// runnable from a clean checkout and never build or import the PG reference.
run(['run', '--cwd', 'packages/core', 'build']);
run(['run', '--cwd', 'packages/node', 'build']);
run([
  'test',
  'packages/cli/tests/cli-parity.test.ts',
  'packages/cli/tests/hard-cli-parity.test.ts',
  'packages/cli/tests/upstream-260118-parity.test.ts'
], { ...process.env, RUN_PARITY_TESTS: 'true' });
