#!/usr/bin/env tsx
// Script to preprocess hard CLI comparison tests by running Docker once and caching outputs

import { execSync } from 'child_process';
import { readFileSync, writeFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Load environment variables from .env file
const envPath = join(__dirname, '../.env');
try {
  const envFile = readFileSync(envPath, 'utf-8');
  envFile.split('\n').forEach(line => {
    const trimmed = line.trim();
    if (trimmed && !trimmed.startsWith('#')) {
      const [key, ...valueParts] = trimmed.split('=');
      const value = valueParts.join('=');
      if (key && value) {
        process.env[key] = value;
      }
    }
  });
} catch (e) {
  console.warn('Warning: Could not load .env file');
}

const LISP_CONTAINER = process.env.ICHIRAN_LISP_CONTAINER || 'komu-ichiran-main-1';

interface TestCases {
  romanization: string[];
  info: string[];
  fullJson: Array<{ text: string; limit: number }>;
}

interface ExpectedOutputs {
  romanization: Record<string, string>;
  info: Record<string, string>;
  fullJson: Record<string, any>;
}

function buildCommand(base: string, flags: string, text: string): string {
  const trimmedFlags = flags.trim();
  const flagSegment = trimmedFlags.length > 0 ? `${trimmedFlags} ` : '';
  return `${base} ${flagSegment}"${text}"`;
}

function runLispCli(text: string, flags: string = ''): string {
  const command = buildCommand(`docker exec ${LISP_CONTAINER} ichiran-cli`, flags, text);
  console.log(`Running: ${command}`);
  return execSync(command, { encoding: 'utf-8' }).trim();
}

function main() {
  console.log('Loading hard test cases...');
  const testCasesPath = join(__dirname, '../tests/data/hard-cli.json');
  const testCases: TestCases = JSON.parse(readFileSync(testCasesPath, 'utf-8'));

  const outputs: ExpectedOutputs = {
    romanization: {},
    info: {},
    fullJson: {}
  };

  // Check that Docker container is running
  try {
    execSync(`docker exec ${LISP_CONTAINER} echo "ok"`, { encoding: 'utf-8' });
    console.log(`✓ Docker container ${LISP_CONTAINER} is running\n`);
  } catch (e) {
    console.error(`✗ Docker container ${LISP_CONTAINER} is not running.`);
    console.error('Set ICHIRAN_LISP_CONTAINER env var if using a different name.');
    process.exit(1);
  }

  // Process romanization test cases
  console.log('Processing romanization tests...');
  for (const text of testCases.romanization) {
    console.log(`  - ${text}`);
    outputs.romanization[text] = runLispCli(text);
  }

  // Process info test cases
  console.log('\nProcessing info tests...');
  for (const text of testCases.info) {
    console.log(`  - ${text}`);
    outputs.info[text] = runLispCli(text, '-i');
  }

  // Process full JSON test cases
  console.log('\nProcessing full JSON tests...');
  for (const testCase of testCases.fullJson) {
    const key = `${testCase.text}|${testCase.limit}`;
    console.log(`  - ${testCase.text.substring(0, 50)}... (limit: ${testCase.limit})`);
    outputs.fullJson[key] = runLispCli(testCase.text, `-f -l ${testCase.limit}`);
  }

  // Write outputs to file
  const outputPath = join(__dirname, '../tests/data/hard-cli-lisp-outputs.json');
  writeFileSync(outputPath, JSON.stringify(outputs, null, 2));
  console.log(`\n✓ Expected outputs written to ${outputPath}`);
}

main();

