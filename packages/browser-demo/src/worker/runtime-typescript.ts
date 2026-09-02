import { TypeScriptOracleRuntime } from '@ichiran/core/qualification';
import type { InstalledFiles } from './install.js';
import { decodeGzip, detailSource } from './runtime.js';

/** Qualification-only host for the frozen TypeScript differential oracle. */
export async function openTypeScriptAnalyzerRuntime(
  files: InstalledFiles
): Promise<TypeScriptOracleRuntime> {
  return TypeScriptOracleRuntime.open({
    hot: new Uint8Array(await (await files.hot.getFile()).arrayBuffer()),
    details: await detailSource(files.details),
    decodeGzip
  });
}
