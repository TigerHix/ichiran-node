import { TypeScriptOracleRuntime } from '@ichiran/core/qualification';
import type { TokenDetails, TokenDetailsOptions } from '@ichiran/core';
import type { InstalledFiles } from './install.js';
import { decodeGzip, detailSource } from './runtime.js';

/** Qualification-only host for the frozen TypeScript differential oracle. */
export async function openTypeScriptAnalyzerRuntime(
  files: InstalledFiles
): Promise<{
  analyze: TypeScriptOracleRuntime['analyze'];
  romanize: TypeScriptOracleRuntime['romanize'];
  entry: TypeScriptOracleRuntime['describe'];
  details(text: string, options: TokenDetailsOptions): Promise<TokenDetails>;
}> {
  const oracle = await TypeScriptOracleRuntime.open({
    hot: new Uint8Array(await (await files.hot.getFile()).arrayBuffer()),
    details: await detailSource(files.details),
    decodeGzip
  });
  return {
    analyze: oracle.analyze.bind(oracle),
    romanize: oracle.romanize.bind(oracle),
    entry: oracle.describe.bind(oracle),
    details: async () => {
      throw new Error('Token details are provided by the product Rust runtime');
    }
  };
}
