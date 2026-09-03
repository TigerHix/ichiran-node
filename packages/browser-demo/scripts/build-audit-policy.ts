const TYPESCRIPT_ANALYZER_SIGNATURES = [
  'ICHIPACK',
  'PortableAnalyzer',
  'TypeScriptOracleRuntime'
] as const;
const MAIN_THREAD_ANALYZER_SIGNATURES = [
  ...TYPESCRIPT_ANALYZER_SIGNATURES,
  'AnalyzerRuntime'
] as const;

export function findTypeScriptOracleWorkerChunks(
  worker: string,
  scripts: readonly string[]
): string[] {
  const chunks = scripts.filter(name => name.startsWith('runtime-typescript-'));
  for (const chunk of chunks) {
    if (!worker.includes(chunk)) {
      throw new Error(`TypeScript-oracle chunk ${chunk} is not linked from the analyzer Worker`);
    }
  }
  return chunks;
}

export function assertRustRuntimeGraph(runtime: string): void {
  for (const signature of TYPESCRIPT_ANALYZER_SIGNATURES) {
    if (runtime.includes(signature)) {
      throw new Error(`TypeScript analyzer signature ${signature} is present in the Rust runtime graph`);
    }
  }
}

export function assertAnalyzerWorkerOnly(main: string): void {
  for (const signature of MAIN_THREAD_ANALYZER_SIGNATURES) {
    if (main.includes(signature)) {
      throw new Error(`${signature} leaked into the main-thread bundle`);
    }
  }
}
