import { describe, expect, test } from 'bun:test';

import {
  assertAnalyzerWorkerOnly,
  assertRustRuntimeGraph,
  findTypeScriptOracleWorkerChunks
} from '../scripts/build-audit-policy.js';

describe('browser build analyzer ownership audit', () => {
  test('rejects an explicit TypeScript-oracle Worker as a Rust runtime graph', () => {
    const oracleWorker = 'const magic = "ICHIPACK"; new PortableAnalyzer();';
    expect(() => assertRustRuntimeGraph(oracleWorker)).toThrow('ICHIPACK');
  });

  test('accepts the Rust adapter graph and keeps analyzer code off the main thread', () => {
    expect(() => assertRustRuntimeGraph('IchiranRuntime WebAssembly')).not.toThrow();
    expect(() => assertAnalyzerWorkerOnly('render application shell')).not.toThrow();
    expect(() => assertAnalyzerWorkerOnly('new AnalyzerRuntime()')).toThrow('main-thread');
  });

  test('identifies the explicit oracle runtime as a Worker-only lazy chunk', () => {
    const chunk = 'runtime-typescript-AbCd1234.js';
    expect(findTypeScriptOracleWorkerChunks(`import('./${chunk}')`, [
      'index-AbCd1234.js',
      chunk
    ])).toEqual([chunk]);
    expect(() => findTypeScriptOracleWorkerChunks('worker shell', [chunk])).toThrow(
      'not linked from the analyzer Worker'
    );
  });
});
