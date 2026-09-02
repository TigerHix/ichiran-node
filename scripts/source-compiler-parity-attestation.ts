#!/usr/bin/env bun

import { resolve } from 'node:path';

import { verifySourceCompilerParityAttestation } from '../packages/data/src/source-compiler/parity-attestation.js';

interface Options {
  readonly repository: string;
  readonly report: string;
  readonly release: string;
  readonly attestation: string;
  readonly sourceLock: string;
  readonly oracleLock: string;
}

function usage(message?: string): never {
  const prefix = message ? `error: ${message}\n\n` : '';
  throw new Error(`${prefix}usage: bun scripts/source-compiler-parity-attestation.ts \\
  --report <diagnostic.json> --release <release-directory> [--repository <directory>]`);
}

function argumentsFor(argv: readonly string[]): Options {
  let repository = process.cwd();
  let report: string | undefined;
  let release: string | undefined;
  let attestation = 'data/source-compiler-parity-attestation.json';
  let sourceLock = 'data/source-compiler-sources.lock.json';
  let oracleLock = 'browser-alpha/sources.lock.json';
  for (let index = 0; index < argv.length; index++) {
    const argument = argv[index]!;
    const next = (): string => {
      const value = argv[++index];
      if (!value) usage(`${argument} requires a value`);
      return value;
    };
    if (argument === '--repository') repository = next();
    else if (argument === '--report') report = next();
    else if (argument === '--release') release = next();
    else if (argument === '--attestation') attestation = next();
    else if (argument === '--source-lock') sourceLock = next();
    else if (argument === '--oracle-lock') oracleLock = next();
    else if (argument === '--help' || argument === '-h') usage();
    else usage(`unknown argument ${argument}`);
  }
  if (!report) usage('--report is required');
  if (!release) usage('--release is required');
  repository = resolve(repository);
  const path = (value: string): string => resolve(repository, value);
  return {
    repository,
    report: path(report),
    release: path(release),
    attestation: path(attestation),
    sourceLock: path(sourceLock),
    oracleLock: path(oracleLock)
  };
}

const options = argumentsFor(process.argv.slice(2));
const verified = await verifySourceCompilerParityAttestation({
  attestationPath: options.attestation,
  reportPath: options.report,
  releaseDirectory: options.release,
  sourceLockPath: options.sourceLock,
  oracleLockPath: options.oracleLock
});
process.stdout.write(`${JSON.stringify(verified, null, 2)}\n`);
